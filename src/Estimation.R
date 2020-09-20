library(quantreg)
library(tidyverse)
library(pracma)
library(rqPen)

data(engel)
xx <- engel$income - mean(engel$income)
fit1 <- summary(rq(foodexp~xx,tau=2:98/100, data = engel)) 
fit2 <- summary(rq(foodexp~xx,tau=c(.05, .25, .5, .75, .95), data = engel))

pdf("engelcoef.pdf",width=6.5,height=3.5)
plot(fit1,mfrow = c(1,2))
dev.off()

latex(fit2, caption="Engel's Law", transpose=TRUE)
dataset <- read_rds(str_c(here::here(), '/data/dataset_no_treat_compl.rds'))
source(str_c(here::here(), '/src/functions.R'))



treat <- tibble(t = c(0:5),
treat = ols_residuals(dataset = treated, gender_sub = 1, treat = T)
) #%>% mutate(resid_mean = mean(men_treat , women_treat))
contrl <- tibble(t = c(-1:-5),
emn_control = ols_residuals(dataset = treated, gender_sub = 1, treat = F))

means_key <- tibble(country = unique(dataset$country_name)) %>% arrange(country) %>%
  mutate( log_w = map(.x = country, ~filter(dataset, country_name == .x) %>% .$log_hourly_earnings %>% mean())) %>%
  mutate( s = map(.x = country, ~filter(dataset, country_name == .x) %>% .$edu_years %>% mean())) %>%
  mutate( ycomp = map(.x = country, ~filter(dataset, country_name == .x) %>% .$ycomp %>% mean())) %>%
  mutate( Age = map(.x = country, ~filter(dataset, country_name == .x) %>% .$age %>% mean())) %>%  
  mutate( `% Males` = map(.x = country, ~filter(dataset, country_name == .x) %>% .$gender %>% mean())) %>%
  mutate( Nobs = map(.x = country, ~filter(dataset, country_name == .x) %>% nrow())) %>%  
  mutate( `% Treat` = map(.x = country, ~filter(dataset, country_name == .x) %>% .$treatment_dummy %>% mean())) 


### Empirical model
XZ <- datse
mc_s <- lm(dataset )


tau <- c(0.1,0.3,0.5,0.7,0.9)
dataset_male <- dataset %>% filter(gender == 1)

dataset_female <- dataset %>% filter(gender == 0)

#quantile effects when education is treated as exogenpus
qr_exo_male <- f_rq_results(dataset = dataset_male, tau = tau, stage = 0, IV = F) #inputs dataset, gender, stage, tau
qr_exo_female <- f_rq_results(dataset = dataset_female, tau = tau, stage = 0, IV = F) #inputs dataset, gender, stage, tau

#first stage effect of ycomp on s, coefficients = of 2 parameter, fitted values for distribution of a needs to be done for each tau
set.seed(1337) #Seed for CDF
##############################male##############################
qr_fs_male_t01 <- f_rq_results(dataset = dataset_male, tau = 0.1, stage = 1, IV = T) #tau = 0.1
ability_male_t01 = dataset_male$edu_years - qr_fs_male_t01$fitted.values
dataset_male <- dataset_male  %>% mutate(ability_t01 = ability_male_t01)


qr_fs_male_t03 <- f_rq_results(dataset = dataset_male, tau = 0.3, stage = 1, IV = T) #tau = 0.3
ability_male_t03 = dataset_male$edu_years - qr_fs_male_t03$fitted.values
dataset_male <- dataset_male  %>% mutate(ability_t03 = ability_male_t03)


qr_fs_male_t05 <- f_rq_results(dataset = dataset_male, tau = 0.5, stage = 1, IV = T) #tau = 0.5
ability_male_t05 = dataset_male$edu_years - qr_fs_male_t05$fitted.values
dataset_male <- dataset_male  %>% mutate(ability_t05 = ability_male_t05)


qr_fs_male_t07 <- f_rq_results(dataset = dataset_male, tau = 0.7, stage = 1, IV = T) #tau = 0.7
ability_male_t07 = dataset_male$edu_years - qr_fs_male_t07$fitted.values
dataset_male <- dataset_male  %>% mutate(ability_t07 = ability_male_t07)


qr_fs_male_t09 <- f_rq_results(dataset = dataset_male, tau = 0.9, stage = 1, IV = T) #tau = 0.9
ability_male_t09 = dataset_male$edu_years - qr_fs_male_t09$fitted.values
dataset_male <- dataset_male  %>% mutate(ability_t09 = ability_male_t09)




##############################female##############################
qr_fs_female_t01 <- f_rq_results(dataset = dataset_female, tau = 0.1, stage = 1, IV = T) #tau = 0.1
ability_female_t01 = dataset_female$edu_years - qr_fs_female_t01$fitted.values
dataset_female <- dataset_female  %>% mutate(ability_t01 = ability_female_t01)


qr_fs_female_t03 <- f_rq_results(dataset = dataset_female, tau = 0.3, stage = 1, IV = T) #tau = 0.3
ability_female_t03 = dataset_female$edu_years - qr_fs_female_t03$fitted.values
dataset_female <- dataset_female  %>% mutate(ability_t03 = ability_female_t03)


qr_fs_female_t05 <- f_rq_results(dataset = dataset_female, tau = 0.5, stage = 1, IV = T) #tau = 0.5
ability_female_t05 = dataset_female$edu_years - qr_fs_female_t05$fitted.values
dataset_female <- dataset_female  %>% mutate(ability_t05 = ability_female_t05)


qr_fs_female_t07 <- f_rq_results(dataset = dataset_female, tau = 0.7, stage = 1, IV = T) #tau = 0.7
ability_female_t07 = dataset_female$edu_years - qr_fs_female_t07$fitted.values
dataset_female <- dataset_female  %>% mutate(ability_t07 = ability_female_t07)

qr_fs_female_t09 <- f_rq_results(dataset = dataset_female, tau = 0.9, stage = 1, IV = T) #tau = 0.9
ability_female_t09 = dataset_female$edu_years - qr_fs_female_t09$fitted.values
dataset_female <- dataset_female  %>% mutate(ability_t09 = ability_female_t09)





##########################################################################################################
#obtain fitted values for distribution of luck u and beta
qr_luck_male_t01 <- f_rq_results(dataset = dataset_male, tau = 0.1, stage = 2, IV = T, ability_g_tau = 'ability_t01') #tau = 0.1
luck_male_t01 = dataset_male$log_hourly_earnings - qr_luck_male_t01$fitted.values
dataset_male <- dataset_male  %>% mutate(luck_t01 = luck_male_t01) %>% mutate(beta_01 = qr_luck_male_t01$coefficients[2])


qr_luck_male_t03 <- f_rq_results(dataset = dataset_male, tau = 0.3, stage = 2, IV = T,ability_g_tau = 'ability_t03') #tau = 0.3
luck_male_t03 = dataset_male$log_hourly_earnings - qr_luck_male_t03$fitted.values
dataset_male <- dataset_male  %>% mutate(luck_t03 = luck_male_t03) %>% mutate(beta_03 = qr_luck_male_t03$coefficients[2]) 


qr_luck_male_t05 <- f_rq_results(dataset = dataset_male, tau = 0.5, stage = 2, IV = T, ability_g_tau = 'ability_t05') #tau = 0.5
luck_male_t05 = dataset_male$log_hourly_earnings - qr_luck_male_t05$fitted.values
dataset_male <- dataset_male  %>% mutate(luck_t05 = luck_male_t05) %>% mutate(beta_05 = qr_luck_male_t05$coefficients[2])


qr_luck_male_t07 <- f_rq_results(dataset = dataset_male, tau = 0.7, stage = 2, IV = T, ability_g_tau = 'ability_t07') #tau = 0.7
luck_male_t07 = dataset_male$log_hourly_earnings - qr_luck_male_t07$fitted.values
dataset_male <- dataset_male  %>% mutate(luck_t07 = luck_male_t07) %>% mutate(beta_07 = qr_luck_male_t07$coefficients[2])


qr_luck_male_t09 <- f_rq_results(dataset = dataset_male, tau = 0.9, stage = 2, IV = T, ability_g_tau = 'ability_t09') #tau = 0.9
luck_male_t09 = dataset_male$log_hourly_earnings - qr_luck_male_t09$fitted.values
dataset_male <- dataset_male  %>% mutate(luck_t09 = luck_male_t09) %>% mutate(beta_09 = qr_luck_male_t09$coefficients[2])



##############################female##############################
qr_luck_female_t01 <- f_rq_results(dataset = dataset_female, tau = 0.1, stage = 2, IV = T, ability_g_tau = 'ability_t01') #tau = 0.1
luck_female_t01 = dataset_female$log_hourly_earnings - qr_luck_female_t01$fitted.values
dataset_female <- dataset_female  %>% mutate(luck_t01 = luck_female_t01) %>% mutate(beta_01 = qr_luck_female_t01$coefficients[2])


qr_luck_female_t03 <- f_rq_results(dataset = dataset_female, tau = 0.3, stage = 2, IV = T,ability_g_tau = 'ability_t03') #tau = 0.3
luck_female_t03 = dataset_female$log_hourly_earnings - qr_luck_female_t03$fitted.values
dataset_female <- dataset_female  %>% mutate(luck_t03 = luck_female_t03) %>% mutate(beta_03 = qr_luck_female_t03$coefficients[2]) 


qr_luck_female_t05 <- f_rq_results(dataset = dataset_female, tau = 0.5, stage = 2, IV = T, ability_g_tau = 'ability_t05') #tau = 0.5
luck_female_t05 = dataset_female$log_hourly_earnings - qr_luck_female_t05$fitted.values
dataset_female <- dataset_female  %>% mutate(luck_t05 = luck_female_t05) %>% mutate(beta_05 = qr_luck_female_t05$coefficients[2])


qr_luck_female_t07 <- f_rq_results(dataset = dataset_female, tau = 0.7, stage = 2, IV = T, ability_g_tau = 'ability_t07') #tau = 0.7
luck_female_t07 = dataset_female$log_hourly_earnings - qr_luck_female_t07$fitted.values
dataset_female <- dataset_female  %>% mutate(luck_t07 = luck_female_t07) %>% mutate(beta_07 = qr_luck_female_t07$coefficients[2])


qr_luck_female_t09 <- f_rq_results(dataset = dataset_female, tau = 0.9, stage = 2, IV = T, ability_g_tau = 'ability_t09') #tau = 0.9
luck_female_t09 = dataset_female$log_hourly_earnings - qr_luck_female_t09$fitted.values
dataset_female <- dataset_female  %>% mutate(luck_t09 = luck_female_t09) %>% mutate(beta_09 = qr_luck_female_t09$coefficients[2])

# get generate key parameter estimate si
qr_key_female_t01 <- f_rq_results(dataset = dataset_female, tau = 0.1, stage = 3 , IV = T, ability_g_tau = 'ability_01', luck_g_tau = 'luck_01', key_param_g_tau = '')
#generate key parameter of interest and adjust before all functions for luck


rq(log_hourly_earnings ~ rq(edu_years ~1  + ycomp + ability_female_t01, data = dataset_female, method = 'sfn')$fitted.values:ability_female_t01 +  ability_female_t01 + luck_female_t01 , data =dataset_female, method = 'sfn')
