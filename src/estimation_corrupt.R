require(quantreg)
require(tidyverse)
require(rqPen)



source(str_c(here::here(), '/src/functions.R'))
dataset <- read_rds(str_c(here::here(), '/data/dataset_no_treat_compl.rds'))
resid_edu_ycomp_pre <-c(
 f_ols_resid(dataset = dataset, t_ = '-5') ,
 f_ols_resid(dataset = dataset, t_ = '-4') ,
 f_ols_resid(dataset = dataset, t_ = '-3') ,
 f_ols_resid(dataset = dataset, t_ = '-2') ,
 f_ols_resid(dataset = dataset, t_ = '-1') ,
 NA
)  %>% as_tibble(.name_repair = 'unique') %>% 
  mutate(t = c(-5:-1,-0.5)) %>%
  rename(residuals = value) %>%
  select(t, residuals)

resid_edu_ycomp_post <-c(
  f_ols_resid(dataset = dataset, t_ = '0') ,
  f_ols_resid(dataset = dataset, t_ = '1') ,
  f_ols_resid(dataset = dataset, t_ = '2') ,
  f_ols_resid(dataset = dataset, t_ = '3') ,
  f_ols_resid(dataset = dataset, t_ = '4') ,
  f_ols_resid(dataset = dataset, t_ = '5') 
)  %>% as_tibble(.name_repair = 'unique') %>% 
  mutate(t = 0:5) %>%
  rename(residuals = value) %>%
  select(t, residuals)

resid_edu_ycomp <- full_join(resid_edu_ycomp_pre,resid_edu_ycomp_post)
saveRDS(resid_edu_ycomp, file = str_c(here::here(),'/data/resid_plot.rds'))



dataset <- read_rds(str_c(here::here(), '/data/datasetfull.Rds')) %>% mutate(Comp_dummy = if_else(compliers == 'complier', 1, 0))
means_key <- tibble(country = unique(dataset$country_name)) %>% arrange(country) %>%
  mutate( log_w = map(.x = country, ~filter(dataset, country_name == .x) %>% .$log_hourly_earnings %>% mean() %>% round(3))) %>%
  mutate( s = map(.x = country, ~filter(dataset, country_name == .x) %>% .$edu_years %>% mean()%>% round(3))) %>%
  mutate( ycomp = map(.x = country, ~filter(dataset, country_name == .x) %>% .$ycomp %>% mean()%>% round(3))) %>%
  mutate( Age = map(.x = country, ~filter(dataset, country_name == .x) %>% .$age %>% mean()%>% round(3))) %>%  
  mutate( `% Males` = map(.x = country, ~filter(dataset, country_name == .x) %>% .$gender %>% mean()%>% round(3))) %>%
  mutate( Nobs = map(.x = country, ~filter(dataset, country_name == .x) %>% nrow()%>% round(3))) %>%  
  mutate( `% Complier` = map(.x = country, ~filter(dataset, country_name == .x) %>% .$Comp_dummy %>% mean()%>% round(3))) %>% 
write_rds( path = str_c(here::here(),'/data/dataset_means_key.Rds'))


set.seed(1337) #Seed for jittering, used to prevent singular matrices due to many dummy variables
dataset <- read_rds(str_c(here::here(), '/data/dataset_no_treat_compl.rds')) 

dataset_male <- dataset %>% filter(gender == 1) %>% select(log_hourly_earnings ,everything())%>% 
  jitter_pos(jitter.val =0.1)   %>% select(-ycomp,everything(), ycomp) %>% select(-gender, -t)

dataset_female <- dataset %>% filter(gender == 0)%>% select(log_hourly_earnings, -gender ,everything())%>%
  jitter_pos(jitter.val =0.1)   %>% select(-ycomp,everything(), ycomp )%>% select(-gender, -t)






#quantile effects when education is treated as exogenpus

qr_exo_male_t01 <- f_rq_results(dataset = dataset_male, tau = 0.1, stage = 0, IV = F) #inputs dataset, gender, stage, tau
sum_exo_male_t01 <- summary.rq(qr_exo_male_t01 , se = 'boot')[[3]][c(59,118,177,236)]

qr_exo_male_t03 <- f_rq_results(dataset = dataset_male, tau = 0.3, stage = 0, IV = F) #inputs dataset, gender, stage, tau
sum_exo_male_t03 <- summary.rq(qr_exo_male_t03 , se = 'boot')[[3]][c(59,118,177,236)]


qr_exo_male_t05 <- f_rq_results(dataset = dataset_male, tau = 0.5, stage = 0, IV = F) #inputs dataset, gender, stage, tau
sum_exo_male_t05 <- summary.rq(qr_exo_male_t05 , se = 'boot')[[3]][c(59,118,177,236)]


qr_exo_male_t07 <- f_rq_results(dataset = dataset_male, tau = 0.7, stage = 0, IV = F) #inputs dataset, gender, stage, tau
sum_exo_male_t07 <- summary.rq(qr_exo_male_t07 , se = 'boot')[[3]][c(59,118,177,236)]


qr_exo_male_t09 <- f_rq_results(dataset = dataset_male, tau = 0.9, stage = 0, IV = F) #inputs dataset, gender, stage, tau
sum_exo_male_t09 <- summary.rq(qr_exo_male_t09 , se = 'boot')[[3]][c(59,118,177,236)]


exo_sum_male <- tibble(value = c(sum_exo_male_t01[1],
                                sum_exo_male_t03[1],
                                sum_exo_male_t05[1],
                                sum_exo_male_t07[1],
                                sum_exo_male_t09[1]),
                      std_error = c(sum_exo_male_t01[2],
                                    sum_exo_male_t03[2],
                                    sum_exo_male_t05[2],
                                    sum_exo_male_t07[2],
                                    sum_exo_male_t09[2]),
                      t_val = c(sum_exo_male_t01[3],
                                sum_exo_male_t03[3],
                                sum_exo_male_t05[3],
                                sum_exo_male_t07[3],
                                sum_exo_male_t09[3]),
                      p_val = c(sum_exo_male_t01[4],
                                sum_exo_male_t03[4],
                                sum_exo_male_t05[4],
                                sum_exo_male_t07[4],
                                sum_exo_male_t09[4]))
write_rds(exo_sum_male, path = str_c(here::here(),'/data/exo_sum_male.rds'))
#female

qr_exo_female_t01 <- f_rq_results(dataset = dataset_female, tau = 0.1, stage = 0, IV = F) #inputs dataset, gender, stage, tau
sum_exo_female_t01 <- summary.rq(qr_exo_female_t01 , se = 'boot')[[3]][c(59,118,177,236)]

qr_exo_female_t03 <- f_rq_results(dataset = dataset_female, tau = 0.3, stage = 0, IV = F) #inputs dataset, gender, stage, tau
sum_exo_female_t03 <- summary.rq(qr_exo_female_t03 , se = 'boot')[[3]][c(59,118,177,236)]


qr_exo_female_t05 <- f_rq_results(dataset = dataset_female, tau = 0.5, stage = 0, IV = F) #inputs dataset, gender, stage, tau
sum_exo_female_t05 <- summary.rq(qr_exo_female_t05 , se = 'boot')[[3]][c(59,118,177,236)]


qr_exo_female_t07 <- f_rq_results(dataset = dataset_female, tau = 0.7, stage = 0, IV = F) #inputs dataset, gender, stage, tau
sum_exo_female_t07 <- summary.rq(qr_exo_female_t07 , se = 'boot')[[3]][c(59,118,177,236)]


qr_exo_female_t09 <- f_rq_results(dataset = dataset_female, tau = 0.9, stage = 0, IV = F) #inputs dataset, gender, stage, tau
sum_exo_female_t09 <- summary.rq(qr_exo_female_t09 , se = 'boot')[[3]][c(59,118,177,236)]

exo_sum_female <- tibble(value = c(sum_exo_female_t01[1],
                                  sum_exo_female_t03[1],
                                  sum_exo_female_t05[1],
                                  sum_exo_female_t07[1],
                                  sum_exo_female_t09[1]),
                        std_error = c(sum_exo_female_t01[2],
                                      sum_exo_female_t03[2],
                                      sum_exo_female_t05[2],
                                      sum_exo_female_t07[2],
                                      sum_exo_female_t09[2]),
                        t_val = c(sum_exo_female_t01[3],
                                  sum_exo_female_t03[3],
                                  sum_exo_female_t05[3],
                                  sum_exo_female_t07[3],
                                  sum_exo_female_t09[3]),
                        p_val = c(sum_exo_female_t01[4],
                                  sum_exo_female_t03[4],
                                  sum_exo_female_t05[4],
                                  sum_exo_female_t07[4],
                                  sum_exo_female_t09[4]))
write_rds(exo_sum_female, path = str_c(here::here(),'/data/exo_sum_female.rds'))

#first stage effect of ycomp on s, coefficients = of 2 parameter, fitted values for distribution of a needs to be done for each tau


##############################male##############################
qr_fs_male_t01 <- f_rq_results(dataset = dataset_male, tau = 0.1, stage = 1, IV = T) #tau = 0.1

ability_male_t01 = dataset_male$edu_years - qr_fs_male_t01$fitted.values
dataset_male <- dataset_male  %>% mutate(ability_t01 = ability_male_t01) %>%
         mutate(school_ability_t01 = dataset_male$edu_years*ability_male_t01)
Ftest_male_01 <-f_iv_ftest(dataset = dataset_male, tau = 0.1, stage = 1, IV = T)

sum_fs_male_t01 <- summary.rq(qr_fs_male_t01, se = 'boot')[[3]][c(59,118,177,236)]
qr_fs_male_t03 <- f_rq_results(dataset = dataset_male, tau = 0.3, stage = 1, IV = T) #tau = 0.3

ability_male_t03 = dataset_male$edu_years - qr_fs_male_t03$fitted.values
dataset_male <- dataset_male  %>% mutate(ability_t03 = ability_male_t03) %>%
           mutate(school_ability_t03 = dataset_male$edu_years*ability_male_t03)
Ftest_male_03 <-f_iv_ftest(dataset = dataset_male, tau = 0.3, stage = 1, IV = T)
sum_fs_male_t03 <- summary.rq(qr_fs_male_t03, se = 'boot')[[3]][c(59,118,177,236)]

qr_fs_male_t05 <- f_rq_results(dataset = dataset_male, tau = 0.5, stage = 1, IV = T) #tau = 0.5

ability_male_t05 = dataset_male$edu_years - qr_fs_male_t05$fitted.values
dataset_male <- dataset_male  %>% mutate(ability_t05 = ability_male_t05) %>%
           mutate(school_ability_t05 = dataset_male$edu_years*ability_male_t05)
Ftest_male_05 <-f_iv_ftest(dataset = dataset_male, tau = 0.5, stage = 1, IV = T)
sum_fs_male_t05 <- summary.rq(qr_fs_male_t05, se = 'boot')[[3]][c(59,118,177,236)]


qr_fs_male_t07 <- f_rq_results(dataset = dataset_male, tau = 0.7, stage = 1, IV = T) #tau = 0.7

ability_male_t07 = dataset_male$edu_years - qr_fs_male_t07$fitted.values
dataset_male <- dataset_male  %>% mutate(ability_t07 = ability_male_t07) %>%
           mutate(school_ability_t07 = dataset_male$edu_years*ability_male_t07)
Ftest_male_07 <-f_iv_ftest(dataset = dataset_male, tau = 0.7, stage = 1, IV = T)
sum_fs_male_t07 <- summary.rq(qr_fs_male_t07, se = 'boot')[[3]][c(59,118,177,236)]

qr_fs_male_t09 <- f_rq_results(dataset = dataset_male, tau = 0.9, stage = 1, IV = T) #tau = 0.9

ability_male_t09 = dataset_male$edu_years - qr_fs_male_t09$fitted.values
dataset_male <- dataset_male  %>% mutate(ability_t09 = ability_male_t09) %>%
           mutate(school_ability_t09 = dataset_male$edu_years*ability_male_t09)
Ftest_male_09 <-f_iv_ftest(dataset = dataset_male, tau = 0.9, stage = 1, IV = T)
sum_fs_male_t09 <- summary.rq(qr_fs_male_t09, se = 'boot')[[3]][c(59,118,177,236)]

IV_sum_male <- tibble(value = c(sum_fs_male_t01[1],
                           sum_fs_male_t03[1],
                           sum_fs_male_t05[1],
                           sum_fs_male_t07[1],
                           sum_fs_male_t09[1]),
                 std_error = c(sum_fs_male_t01[2],
                               sum_fs_male_t03[2],
                               sum_fs_male_t05[2],
                               sum_fs_male_t07[2],
                               sum_fs_male_t09[2]),
                 t_val = c(sum_fs_male_t01[3],
                           sum_fs_male_t03[3],
                           sum_fs_male_t05[3],
                           sum_fs_male_t07[3],
                           sum_fs_male_t09[3]),
                 p_val = c(sum_fs_male_t01[4],
                           sum_fs_male_t03[4],
                           sum_fs_male_t05[4],
                           sum_fs_male_t07[4],
                           sum_fs_male_t09[4]))

Ftest_male <- tibble(Ftest_male_01[[1]]) %>%
  add_row(Ftest_male_03[[1]]) %>%
  add_row(Ftest_male_05[[1]]) %>%
  add_row(Ftest_male_07[[1]]) %>%
  add_row(Ftest_male_09[[1]])

write_rds(IV_sum_male, path = str_c(here::here(),'/data/IV_sum_male.rds'))
write_rds(Ftest_male, path = str_c(here::here(),'/data/Ftest_male.rds'))

##############################female##############################


qr_fs_female_t01 <- f_rq_results(dataset = dataset_female, tau = 0.1, stage = 1, IV = T) #tau = 0.1

ability_female_t01 = dataset_female$edu_years - qr_fs_female_t01$fitted.values
dataset_female <- dataset_female  %>% mutate(ability_t01 = ability_female_t01) %>%
  mutate(school_ability_t01 = dataset_female$edu_years*ability_female_t01)
Ftest_female_01 <-f_iv_ftest(dataset = dataset_female, tau = 0.1, stage = 1, IV = T)
sum_fs_female_t01 <- summary.rq(qr_fs_female_t01, se = 'boot')[[3]][c(59,118,177,236)]

qr_fs_female_t03 <- f_rq_results(dataset = dataset_female, tau = 0.3, stage = 1, IV = T) #tau = 0.3

ability_female_t03 = dataset_female$edu_years - qr_fs_female_t03$fitted.values
dataset_female <- dataset_female  %>% mutate(ability_t03 = ability_female_t03) %>%
  mutate(school_ability_t03 = dataset_female$edu_years*ability_female_t03)
Ftest_female_03 <-f_iv_ftest(dataset = dataset_female, tau = 0.3, stage = 1, IV = T)
sum_fs_female_t03 <- summary.rq(qr_fs_female_t03, se = 'boot')[[3]][c(59,118,177,236)]

qr_fs_female_t05 <- f_rq_results(dataset = dataset_female, tau = 0.5, stage = 1, IV = T) #tau = 0.5

ability_female_t05 = dataset_female$edu_years - qr_fs_female_t05$fitted.values
dataset_female <- dataset_female  %>% mutate(ability_t05 = ability_female_t05) %>%
  mutate(school_ability_t05 = dataset_female$edu_years*ability_female_t05)
Ftest_female_05 <-f_iv_ftest(dataset = dataset_female, tau = 0.5, stage = 1, IV = T)
sum_fs_female_t05 <- summary.rq(qr_fs_female_t05, se = 'boot')[[3]][c(59,118,177,236)]

qr_fs_female_t07 <- f_rq_results(dataset = dataset_female, tau = 0.7, stage = 1, IV = T) #tau = 0.7

ability_female_t07 = dataset_female$edu_years - qr_fs_female_t07$fitted.values
dataset_female <- dataset_female  %>% mutate(ability_t07 = ability_female_t07) %>%
  mutate(school_ability_t07 = dataset_female$edu_years*ability_female_t07)
Ftest_female_07 <-f_iv_ftest(dataset = dataset_female, tau = 0.7, stage = 1, IV = T)
sum_fs_female_t07 <- summary.rq(qr_fs_female_t07, se = 'boot')[[3]][c(59,118,177,236)]

qr_fs_female_t09 <- f_rq_results(dataset = dataset_female, tau = 0.9, stage = 1, IV = T) #tau = 0.9

ability_female_t09 = dataset_female$edu_years - qr_fs_female_t09$fitted.values
dataset_female <- dataset_female  %>% mutate(ability_t09 = ability_female_t09) %>%
  mutate(school_ability_t09 = dataset_female$edu_years*ability_female_t09)
Ftest_female_09 <-f_iv_ftest(dataset = dataset_female, tau = 0.9, stage = 1, IV = T)
sum_fs_female_t09 <- summary.rq(qr_fs_female_t09, se = 'boot')[[3]][c(59,118,177,236)]

IV_sum_female <- tibble(value = c(sum_fs_female_t01[1],
                           sum_fs_female_t03[1],
                           sum_fs_female_t05[1],
                           sum_fs_female_t07[1],
                           sum_fs_female_t09[1]),
                 std_error = c(sum_fs_female_t01[2],
                               sum_fs_female_t03[2],
                               sum_fs_female_t05[2],
                               sum_fs_female_t07[2],
                               sum_fs_female_t09[2]),
                 t_val = c(sum_fs_female_t01[3],
                           sum_fs_female_t03[3],
                           sum_fs_female_t05[3],
                           sum_fs_female_t07[3],
                           sum_fs_female_t09[3]),
                 p_val = c(sum_fs_female_t01[4],
                           sum_fs_female_t03[4],
                           sum_fs_female_t05[4],
                           sum_fs_female_t07[4],
                           sum_fs_female_t09[4]))

Ftest_female <- tibble(Ftest_female_01[[1]]) %>%
  add_row(Ftest_female_03[[1]]) %>%
  add_row(Ftest_female_05[[1]]) %>%
  add_row(Ftest_female_07[[1]]) %>%
  add_row(Ftest_female_09[[1]])

write_rds(IV_sum_female, path = str_c(here::here(),'/data/IV_sum_female.rds'))
write_rds(Ftest_female, path = str_c(here::here(),'/data/Ftest_female.rds'))
##########################################################################################################
  #obtain fitted values for distribution of luck u and beta
  qr_luck_male_t01 <- f_rq_results(dataset = dataset_male, tau = 0.1, stage = 2, IV = T, ability_g_tau = 'school_ability_t01') #tau = 0.1
  luck_male_t01 = dataset_male$log_hourly_earnings - qr_luck_male_t01$fitted.values
 
  
  qr_luck_male_t03 <- f_rq_results(dataset = dataset_male, tau = 0.3, stage = 2, IV = T,ability_g_tau = 'school_ability_t03') #tau = 0.3
  luck_male_t03 = dataset_male$log_hourly_earnings - qr_luck_male_t03$fitted.values

  
  
  qr_luck_male_t05 <- f_rq_results(dataset = dataset_male, tau = 0.5, stage = 2, IV = T, ability_g_tau = 'school_ability_t05') #tau = 0.5
  luck_male_t05 = dataset_male$log_hourly_earnings - qr_luck_male_t05$fitted.values

  
  
  qr_luck_male_t07 <- f_rq_results(dataset = dataset_male, tau = 0.7, stage = 2, IV = T, ability_g_tau = 'school_ability_t07') #tau = 0.7
  luck_male_t07 = dataset_male$log_hourly_earnings - qr_luck_male_t07$fitted.values

  #Hier geht es kaputt. Bitte heile machen!
  qr_luck_male_t09 <- f_rq_results(dataset = dataset_male, tau = 0.9, stage = 2, IV = T, ability_g_tau = 'school_ability_t09') #tau = 0.9
  luck_male_t09 = dataset_male$log_hourly_earnings - qr_luck_male_t09$fitted.values
  
  
  
  write_rds(dataset_male_final, path = str_c(here::here(),'/data/dataset_male_final.rds'))
  ##############################female##############################
  qr_luck_female_t01 <- f_rq_results(dataset = dataset_female, tau = 0.1, stage = 2, IV = T, ability_g_tau = 'school_ability_t01') #tau = 0.1
  luck_female_t01 = dataset_female$log_hourly_earnings - qr_luck_female_t01$fitted.values

  
  qr_luck_female_t03 <- f_rq_results(dataset = dataset_female, tau = 0.3, stage = 2, IV = T,ability_g_tau = 'school_ability_t03') #tau = 0.3
  luck_female_t03 = dataset_female$log_hourly_earnings - qr_luck_female_t03$fitted.values

  
  qr_luck_female_t05 <- f_rq_results(dataset = dataset_female, tau = 0.5, stage = 2, IV = T, ability_g_tau = 'school_ability_t05') #tau = 0.5
  luck_female_t05 = dataset_female$log_hourly_earnings - qr_luck_female_t05$fitted.values

  
  qr_luck_female_t07 <- f_rq_results(dataset = dataset_female, tau = 0.7, stage = 2, IV = T, ability_g_tau = 'school_ability_t07') #tau = 0.7
  luck_female_t07 = dataset_female$log_hourly_earnings - qr_luck_female_t07$fitted.values

  
  
  qr_luck_female_t09 <- f_rq_results(dataset = dataset_female, tau = 0.9, stage = 2, IV = T, ability_g_tau = 'school_ability_t09') #tau = 0.9
  luck_female_t09 = dataset_female$log_hourly_earnings - qr_luck_female_t09$fitted.values
 
  
  write_rds(dataset_female_final, path = str_c(here::here(),'/data/dataset_female_final.rds'))
  # get generate key parameter estimate si

  #key parameter regression
  #####################################################################male,vary tau_luck, fix tau_school################################################
  #####################################################################tau_school fix at 0.1################################################
  qr_key_male_ts_01 <- map(.x =c(0.1, 0.3, 0.5, 0.7, 0.9),~f_rq_results(dataset = dataset_male_final, tau = .x, stage = 3 , IV = T, tau_school = 0.1))
  qr_key_se_male_t01 <- map(qr_key_male_ts_01,~summary.rq(.x, se ='boot'))
  
  #####################################################################tau_school fix at 0.3################################################
  qr_key_male_ts_03 <- map(.x =c(0.1, 0.3, 0.5, 0.7, 0.9),~f_rq_results(dataset = dataset_male_final, tau = .x, stage = 3 , IV = T, tau_school = 0.3))
  qr_key_se_male_t03 <- map(qr_key_male_ts_03,~summary.rq(.x, se ='boot'))
  
  #####################################################################tau_school fix at 0.5################################################
  qr_key_male_ts_05 <- map(.x =c(0.1, 0.3, 0.5, 0.7, 0.9),~f_rq_results(dataset = dataset_male_final, tau = .x, stage = 3 , IV = T, tau_school = 0.5))
  qr_key_se_male_t05 <- map(qr_key_male_ts_05,~summary.rq(.x, se ='boot'))
  
  #####################################################################tau_school fix at 0.7################################################
  qr_key_male_ts_07 <- map(.x =c(0.1, 0.3, 0.5, 0.7, 0.9),~f_rq_results(dataset = dataset_male_final, tau = .x, stage = 3 , IV = T, tau_school = 0.7))
  qr_key_se_male_t07 <- map(qr_key_male_ts_07,~summary.rq(.x, se ='boot'))
  
  #####################################################################tau_school fix at 0.9################################################
  qr_key_male_ts_09 <- map(.x =c(0.1, 0.3, 0.5, 0.7, 0.9),~f_rq_results(dataset = dataset_male_final, tau = .x, stage = 3 , IV = T, tau_school = 0.9))
  qr_key_se_male_t09 <- map(qr_key_male_ts_09,~summary.rq(.x, se ='boot'))
  
  
  qr_key_se_male_t01_all<- map(.x = 1:5,~qr_key_se_male_t01[[.x]]$coefficients[c(2, 62, 122, 182, 242)])
  qr_key_se_male_t03_all<- map(.x = 1:5,~qr_key_se_male_t03[[.x]]$coefficients[c(2, 62, 122, 182, 242)])
  qr_key_se_male_t05_all<- map(.x = 1:5,~qr_key_se_male_t05[[.x]]$coefficients[c(2, 62, 122, 182, 242)])
  qr_key_se_male_t07_all<- map(.x = 1:5,~qr_key_se_male_t07[[.x]]$coefficients[c(2, 62, 122, 182, 242)])
  qr_key_se_male_t09_all<- map(.x = 1:5,~qr_key_se_male_t09[[.x]]$coefficients[c(2, 62, 122, 182, 242)])
  
  

  #####################################################################female,vary tau_luck, fix tau_school################################################
  #####################################################################tau_school fix at 0.1################################################
  qr_key_female_ts_01 <- map(.x =c(0.1, 0.3, 0.5, 0.7, 0.9),~f_rq_results(dataset = dataset_female_final, tau = .x, stage = 3 , IV = T, tau_school = 0.1))
  qr_key_se_female_t01 <- map(qr_key_female_ts_01,~summary.rq(.x, se ='boot'))
  
  #####################################################################tau_school fix at 0.3################################################
  qr_key_female_ts_03 <- map(.x =c(0.1, 0.3, 0.5, 0.7, 0.9),~f_rq_results(dataset = dataset_female_final, tau = .x, stage = 3 , IV = T, tau_school = 0.3))
  qr_key_se_female_t03 <- map(qr_key_female_ts_03,~summary.rq(.x, se ='boot'))
  
  #####################################################################tau_school fix at 0.5################################################
  qr_key_female_ts_05 <- map(.x =c(0.1, 0.3, 0.5, 0.7, 0.9),~f_rq_results(dataset = dataset_female_final, tau = .x, stage = 3 , IV = T, tau_school = 0.5))
  qr_key_se_female_t05 <- map(qr_key_female_ts_05,~summary.rq(.x, se ='boot'))
  
  #####################################################################tau_school fix at 0.7################################################
  qr_key_female_ts_07 <- map(.x =c(0.1, 0.3, 0.5, 0.7, 0.9),~f_rq_results(dataset = dataset_female_final, tau = .x, stage = 3 , IV = T, tau_school = 0.7))
  qr_key_se_female_t07 <- map(qr_key_female_ts_07,~summary.rq(.x, se ='boot'))
  
  #####################################################################tau_school fix at 0.9################################################
  qr_key_female_ts_09 <- map(.x =c(0.1, 0.3, 0.5, 0.7, 0.9),~f_rq_results(dataset = dataset_female_final, tau = .x, stage = 3 , IV = T, tau_school = 0.9))
  qr_key_se_female_t09 <- map(qr_key_female_ts_09,~summary.rq(.x, se ='boot'))
  
  
  qr_key_se_female_t01_all<- map(.x = 1:5,~qr_key_se_female_t01[[.x]]$coefficients[c(2, 62, 122, 182, 242)])
  qr_key_se_female_t03_all<- map(.x = 1:5,~qr_key_se_female_t03[[.x]]$coefficients[c(2, 62, 122, 182, 242)])
  qr_key_se_female_t05_all<- map(.x = 1:5,~qr_key_se_female_t05[[.x]]$coefficients[c(2, 62, 122, 182, 242)])
  qr_key_se_female_t07_all<- map(.x = 1:5,~qr_key_se_female_t07[[.x]]$coefficients[c(2, 62, 122, 182, 242)])
  qr_key_se_female_t09_all<- map(.x = 1:5,~qr_key_se_female_t09[[.x]]$coefficients[c(2, 62, 122, 182, 242)])
  
  

  
  
  #####estimates of 
  key_para_male <- tibble(Key_para = c(
    map(.x = 1:5, ~qr_key_se_male_t01_all[[.x]][1]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_male_t03_all[[.x]][1]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_male_t05_all[[.x]][1]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_male_t07_all[[.x]][1]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_male_t09_all[[.x]][1]) %>% unlist
    ),
    std_err = c(
      map(.x = 1:5, ~qr_key_se_male_t01_all[[.x]][2]) %>% unlist,
      map(.x = 1:5, ~qr_key_se_male_t03_all[[.x]][2]) %>% unlist,
      map(.x = 1:5, ~qr_key_se_male_t05_all[[.x]][2]) %>% unlist,
      map(.x = 1:5, ~qr_key_se_male_t07_all[[.x]][2]) %>% unlist,
      map(.x = 1:5, ~qr_key_se_male_t09_all[[.x]][2]) %>% unlist
    ),
    p_val = c(
      map(.x = 1:5, ~qr_key_se_male_t01_all[[.x]][4]) %>% unlist,
      map(.x = 1:5, ~qr_key_se_male_t03_all[[.x]][4]) %>% unlist,
      map(.x = 1:5, ~qr_key_se_male_t05_all[[.x]][4]) %>% unlist,
      map(.x = 1:5, ~qr_key_se_male_t07_all[[.x]][4]) %>% unlist,
      map(.x = 1:5, ~qr_key_se_male_t09_all[[.x]][4]) %>% unlist
    )
    ) %>% as_tibble
  
  mean_effect_male <- c(
    mean(key_para_male$Key_para[c(1,6,11,16,21)]),
    mean(key_para_male$Key_para[c(2,7,12,17,22)]),
    mean(key_para_male$Key_para[c(3,8,13,18,23)]),
    mean(key_para_male$Key_para[c(4,9,14,19,24)]),
    mean(key_para_male$Key_para[c(5,10,15,20,25)])) %>% as_tibble()
  
  write_rds( key_para_male, path = str_c(here::here(),'/data/key_para_male.rds'))
  write_rds( mean_effect_male, path = str_c(here::here(),'/data/mean_effect_male'))
  
  key_para_female <- tibble(Key_para = c(
    map(.x = 1:5, ~qr_key_se_female_t01_all[[.x]][1]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_female_t03_all[[.x]][1]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_female_t05_all[[.x]][1]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_female_t07_all[[.x]][1]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_female_t09_all[[.x]][1]) %>% unlist
  ),
  std_err = c(
    map(.x = 1:5, ~qr_key_se_female_t01_all[[.x]][2]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_female_t03_all[[.x]][2]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_female_t05_all[[.x]][2]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_female_t07_all[[.x]][2]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_female_t09_all[[.x]][2]) %>% unlist
  ),
  p_val = c(
    map(.x = 1:5, ~qr_key_se_female_t01_all[[.x]][4]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_female_t03_all[[.x]][4]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_female_t05_all[[.x]][4]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_female_t07_all[[.x]][4]) %>% unlist,
    map(.x = 1:5, ~qr_key_se_female_t09_all[[.x]][4]) %>% unlist
  )
  ) %>% as_tibble

  
  mean_effect_female <- c(
    mean(key_para_female$Key_para[c(1,6,11,16,21)]),
    mean(key_para_female$Key_para[c(2,7,12,17,22)]),
    mean(key_para_female$Key_para[c(3,8,13,18,23)]),
    mean(key_para_female$Key_para[c(4,9,14,19,24)]),
    mean(key_para_female$Key_para[c(5,10,15,20,25)])) %>% as_tibble()
  
  
  write_rds( key_para_female, path = str_c(here::here(),'/data/key_para_female.rds'))
  write_rds( mean_effect_female, path = str_c(here::here(),'/data/mean_effect_female'))
