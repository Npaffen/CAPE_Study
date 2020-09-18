library(tidyverse)
library(haven)
library(fastDummies)
data <- haven::read_sav(str_c(here::here(), '/data/sharew1_rel7-0-0_ep.sav'))
data2 <- haven::read_sav(str_c(here::here(), '/data/sharew1_rel7-0-0_gv_grossnet.sav'))
data3 <- haven::read_sav(str_c(here::here(), '/data/sharew1_rel7-0-0_ep.sav'))
data4 <- haven::read_sav(str_c(here::here(), '/data/sharew1_rel7-0-0_gv_isced.sav'))
data5 <- haven::read_sav(str_c(here::here(), '/data/sharew1_rel7-0-0_cv_r.sav'))
CPI <- read_csv(str_c(here::here(), '/data/CPI_Count_00_04.csv')) %>%
  select(Country, Time, Value,`Reference Period`) %>%
  arrange(Country)
  
  

source(str_c(here::here(), '/src/functions.R')) 
#data5 has somewhat a type problem in some variables


#choose the usefull variables from the dataset
hours_per_week <-  tibble(mergeid = data3$mergeid,hours_per_week = data3$ep013_1) %>% drop_na() %>% filter(hours_per_week > 0   )

total_income_gross <- tibble(mergeid = data2$mergeid%>% unname(), ytotg = data2$ytotg%>% unname()) %>% drop_na() %>% filter(ytotg > 0 )

                                           
# do not need controls due to unavaiabillity. Mention in paper!

#filter for countrys used in the paper
country_code <- c(11,23,18,17,12,19,30,16,14,13,15)
country_names <-c('Austria', 'Belgium', 'Denmark', 'France', 'Germany', 'Greece', 'Irland', 'Italy', 'Netherlands', 'Sweden', 'Spain')


names(gdp_1990)==names(pop)
#GDP Per Head will be used as controll - data was accessible 
gdp_1990 <- f_data_cleaner(3) 
pop <- f_data_cleaner(sheet = 2) %>% filter(Country == gdp_1990$Country) %>%
  mutate_at(.funs = list( full = ~.*10^3), .vars = vars(-Country)) %>% select(Country,matches('*full'))%>% select(starts_with('19'))
  

#calc gdp head, transpose, rename
gdp_1990_scale <- gdp_1990 %>%
  mutate_at(.funs = list( head = ~.*10^6), .vars = vars(-Country)) %>% select(Country,matches('*head')) %>%
  data.table::setnames(old = names(.),
                       new = names(gdp_1990[1:195])) %>% select(starts_with('19'))
gdp_1990_head <-  map(.x = 2:length(gdp_1990_scale), ~gdp_1990_scale[,.x]/pop[,.x]) %>% as_tibble(.name_repair = 'unique') %>%
  data.table::setnames(old = names(.),
                       new = names(gdp_1990_scale[2:length(gdp_1990_scale)])) %>%
  mutate(Country = gdp_1990$Country) %>%
  arrange(Country) %>%
  filter(Country %in% c(country_names,'Ireland')) %>%
  arrange(Country)%>%
  mutate(Country = country_names) %>%
  rename(country_name = Country ) %>%
  mutate(country = country_code) #%>% transpose_df() %>%
  #data.table::setnames(old = names(.),
 #                      new = as.character(.[1,])) %>%
  #.[2:nrow(.),] %>%
 # mutate(country = gsub(x = Country, pattern = '_head', replacement = ''))

  #filter only for countries of the paper, calc market entry year for dataset, join gdp_head to dataset by country and year of market entry 



#calculate the mean year of the first cohort potentially affected in germany
cohort_yr_mean_ger <- floor(mean(c(1941, 1934, 1947, 1943, 1953, 1953, 1953, 1953, 1955, 1949))) 
#some help vectors for the incoming dataset mutation. All data can be retrieved from the paper
crit_year <- c(1947,1969,1957,1953,cohort_yr_mean_ger,1958,1958,1949,1959,1947,1947)
ycomp_reform <- c(9,12,9,10,9,9,9,9,10,8,9)
ycomp_old <- c(8,8,7,8,8,6,8,5,9,6,8)
y_till_market <- c(14,14,14,14,14,12,14,11,15,12,14)


mutation_helper <- tibble(country = country_code, crit_year = crit_year,ycomp_reform,ycomp_old,y_till_market) %>%
  right_join(gdp_1990_head, by = 'country') %>% select(country_name, everything()) %>% mutate(CPI_00 = filter(CPI,Time == 2000)$Value) %>%
  mutate(CPI_04 = filter(CPI,Time == 2004)$Value) %>% mutate(PPP_00_04 = (CPI_00/CPI_04 ))

#generate gross hourly log earnings drop NA and <= 0 values of hours and income,
#generate dataset, select educational level of individuals and gender, filter for age as mentioned by Brunelleo at el., add country code 
#add market entry year as described in the paper and and the distance between the first cohort and the first treated + 7 as described
#in the paper
#add a index variable for controll or treatment defined by those who are mentioned as first potentially affected
dataset <- inner_join(x =hours_per_week,y= total_income_gross, by = 'mergeid') %>% #
  inner_join(x = . , y = data4 %>% select(mergeid, isced1997y_r), by = 'mergeid', copy = T)  %>% 
  inner_join(x = ., y = data5 %>% select(mergeid,gender, age2004,yrbirth), by = 'mergeid') %>%
  inner_join(x =. , y = data %>% select(mergeid, country), by = 'mergeid') %>%
  left_join(mutation_helper, by = "country") %>%
  mutate(hourly_earnings = ytotg/(hours_per_week*52)*PPP_00_04 ) %>% 
  mutate(log_hourly_earnings = log(hourly_earnings)) %>%
  mutate_at(.vars = vars(age2004,yrbirth,gender, isced1997y_r, country, hours_per_week ), .funs = list(as.numeric)) %>%
  filter(between(age2004,26,65)) %>%
  mutate(gender = if_else(gender == 2, 0, 1)) %>%
  rename(edu_years = isced1997y_r, age = age2004 ) %>%
  #generate age_squared
  mutate(age_2 = age^2) %>%
  select(mergeid,country, everything()) %>%
  filter(country %in% country_code) %>%
  #generate treatment group dummy
  mutate(treatment = if_else(yrbirth >= crit_year-7, "Treat", "Contr")) %>%
  mutate(treatment_dummy = if_else(treatment == 'Treat', 1, 0)) %>%
  #generate control group dummy
  mutate(control = if_else(yrbirth <= crit_year+7, "Contr", "Treat")) %>%
  #generate possible market entry year for treated,
   mutate(market_entry_y = yrbirth + y_till_market) %>%
  #distance between cohort and first treated cohort 
  mutate(t = yrbirth - crit_year) %>%
  #q = t+ 7 and q^2
  mutate(q = t + 7) %>%
  mutate(q_2 = q^2) %>%
  #compliers 
  mutate(compliers = if_else(t>=0,'complier', 'other')) %>% 
  #calculate gdp head from Maddison Database 2010
  do(.,mutate(.,gdp_head  = pmap(list(1:nrow(.), market_entry_y), function(x,y) .[x,(y-1901+16)]) %>% unlist() ))  %>%
  #first lags for gdp
  do(.,mutate(.,gdp_head_t_1  = pmap(list(1:nrow(.), market_entry_y), function(x,y) .[x,(y-1901+15)]) %>% unlist() )) %>%
  do(.,mutate(.,gdp_head_t_2  = pmap(list(1:nrow(.), market_entry_y), function(x,y) .[x,(y-1901+14)]) %>% unlist() )) %>%
  #create instrumental variable ycomp
  mutate(ycomp = if_else(yrbirth >= crit_year, ycomp_reform,ycomp_old)) %>%
  #create country dummies
  dummy_cols(select_columns = c('country_name')) %>%
  #make sure every columns is of the same type
  mutate_if(is.integer, .funs = list(as.numeric))%>%
  select(!contains('1')|!matches(names(mutation_helper))) %>% #remove gdp_head_helper
  select(-age, age) %>% #sets age as last column so helper variables can easier removed
  arrange(country_name) #sort by country_name

#need interactions terms of q, q_2, age, age_2 with all country dummys  
inter_q <- dataset %>% select(starts_with(match = 'country_name_')) * dataset$q
inter_q <- inter_q %>% data.table::setnames(old = names(.),
                     new = str_c(names(.),'_q'))
inter_q_2 <- dataset %>% select(starts_with(match = 'country_name_')) * dataset$q_2
inter_q_2 <- inter_q_2 %>% data.table::setnames(old = names(.),
                                            new = str_c(names(.),'_q_2'))
inter_age <- dataset %>% select(starts_with(match = 'country_name_')) * dataset$age
inter_age <- inter_age  %>% data.table::setnames(old = names(.),
                         new = str_c(names(.),'_age'))
inter_age_2 <- dataset %>% select(starts_with(match = 'country_name_')) * dataset$age_2
inter_age_2 <- inter_age_2 %>% data.table::setnames(old = names(.),
                                                        new = str_c(names(.),'_age_2'))

interactions <- tibble(inter_q, inter_q_2, inter_age, inter_age_2) %>% mutate(mergeid = dataset$mergeid)

#merge interactions and dataset
dataset <- dataset %>% left_join(interactions)
#reduce the dataset to endogenous, exogenous, treatment/instrument
dataset <- dataset %>% select(-c(1:4,8:16)) %>% select(log_hourly_earnings, everything())
#Generate treated and control group
treated <- dataset %>% filter(treatment == 'Treat') %>% mutate(mean = mean(log_hourly_earnings))
control <- dataset %>% filter(control == 'Contr') %>% mutate(mean = mean(log_hourly_earnings))
  
#write treat and controll
write_rds(treated, path = str_c(here::here(),'/data/treated.Rds'))
write_rds(control, path = str_c(here::here(),'/data/control.Rds'))
#delete treatment 
dataset <- dataset %>% select(-c(6:8, 13))
#write dataset
write_rds(dataset, path = str_c(here::here(),'/data/dataset_no_treat_compl.Rds'))
#Controll
#https://stats.oecd.org/Index.aspx?DataSetCode=EPL_OV# no data before 1985 fpr OECD index of the strictness of employment
#unemployment rate by ILO LAB no data before 1990


#gross domestic product
#https://stats.oecd.org/viewhtml.aspx?datasetcode=PRICES_CPI&lang=en#
 
