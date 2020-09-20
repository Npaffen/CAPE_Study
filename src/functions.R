#Set NA to 0 or 1 for population sheet, Rename columns by year or country for first col

f_data_cleaner <- function(sheet){
  x <- readxl::read_xlsx(str_c(here::here(), '/data//gdp_data.xlsx'), 
                         sheet = sheet,
  ) %>% select(-c('...3', '...5', '...7', '...9', '...11'))
  x[2,1] <- 'Country'
  x <- x %>% data.table::setnames(
    old = x %>% names(),
    new = as.character(x[2, ])
  ) 
  if(sheet == 2){x <- x %>% mutate_all(funs(ifelse(is.na(.), 1, .))) %>% .[-c(1,2), ] %>% select(-c('2009','NA','2030')) 
  }else {x <- x %>% mutate_all(funs(ifelse(is.na(.), 0, .))) %>% .[-c(1,2,194,195,196,197), ] 
  }
  return(x)
}

#for the ols results
f_ols_residuals <- function(dataset, gender_sub, treat  ) {
  if(treat == T){t <- 0:5}else{t <- -5:-1}
  
  residuals_out<- tibble( residuals = numeric(length(t)))
  for (t_sub in t ) {
  
  
  data_lm <- dataset %>% filter(t == t_sub)
  #control
  
  X <- dataset %>% select(gdp_head, gdp_head_t_1, gdp_head_t_2) %>% mutate(cons = 1)
  index <- ifelse(treat == T, t_sub+1,t_sub+6)
  if(nrow(dataset)>0){
 residuals_out[index,2] <- lm(data = data_lm,  edu_years ~ 1 + ycomp + country_name_Austria + country_name_Belgium +  country_name_Denmark + country_name_Germany + country_name_Greece +
       country_name_Italy + country_name_Netherlands + country_name_Spain + country_name_Sweden + q + q_2 +
       q:country_name_Austria + q:country_name_Belgium +  q:country_name_Denmark + q:country_name_Germany + q:country_name_Greece +
       q:country_name_Italy + q:country_name_Netherlands + q:country_name_Spain + q:country_name_Sweden +
       q_2:country_name_Austria + q_2:country_name_Belgium +  q_2:country_name_Denmark + q_2:country_name_Germany + q_2:country_name_Greece +
       q_2:country_name_Italy + q_2:country_name_Netherlands + q_2:country_name_Spain + q_2:country_name_Sweden+ age + age_2 + gender
  ) %>% residuals %>% mean} else {residuals_out[index,2] <- 0}
  }
 return(residuals_out)
}
######################################################################################################################################################################################

#Generate estimation formulas for the different stages
f_lasso_cov <- function(controls, Y=0, Z=0, IV, stage = 0 , tau, ability_g_tau = 0, luck_g_tau = 0, beta_tau = 0){
if(IV == F){ #formula for quantile effects when education is treated as exogenpus
  rq1 = LASSO.fit(Y, controls , tau = tau, intercept = T, lambda = 1, coef.cutoff=1e-08)
  X_Lasso <-  controls %>% .[,rq1[2:length(rq1)]!=0] %>% as_tibble() %>% names
formula <- as.formula(str_c('log_hourly_earnings ~ 1 + edu_years',str_c(X_Lasso, collapse = '+'), sep = '+'))


}else if(IV == T && stage == 1){ #formula for first stage effect of ycomp on s, coefficients = of 2 parameter, fitted values for distribution of a
  rq1 = LASSO.fit(Z, controls , tau = tau, intercept = T, lambda = 1, coef.cutoff=1e-08)
  X_Lasso <-  controls %>% .[,rq1[2:length(rq1)]!=0] %>% as_tibble() %>% names
  formula <- as.formula(str_c('edu_years ~ 1 +  ycomp',str_c(X_Lasso, collapse = '+'), sep = '+'))
  

  }else if(IV == T && stage == 2){ #formula to obtain residuals for distribution of luck u
  rq1 = LASSO.fit(Y, controls , tau = tau, intercept = T, lambda = 1, coef.cutoff=1e-08)
  X_Lasso <-  controls %>% .[,rq1[2:length(rq1)]!=0] %>% as_tibble() %>% names
  if(ability_g_tau %in% X_Lasso){X_Lasso <- gsub(pattern = ability_g_tau, replacement = '') } #make sure that specific variables do not appear in X_Lasso
  if('edu_years' %in% X_Lasso){X_Lasso <- gsub(pattern = 'edu_years', replacement = '') } #make sure that specific variables do not appear in X_Lasso
  
  formula <- as.formula(str_c('log_hourly_earnings ~ 1+ edu_years',str_c(ability_g_tau,':edu_years'),
                              str_c(X_Lasso, collapse = '+'), sep = '+') %>% gsub(pattern = '\\+\\+', replacement = '+'))
  
  
  }else if(IV == T && stage == 3){ #formula to get effect of key parameter
  rq1 = LASSO.fit(Y, IV_dataset , tau = tau, intercept = T, lambda = 1, coef.cutoff=1e-08)
  X_Lasso_earn <-  IV_dataset %>% .[,rq1[2:length(rq1)]!=0] %>% as_tibble() %>% names
  rq2 = LASSO.fit(Z, controls , tau = tau, intercept = T, lambda = 1, coef.cutoff=1e-08)
  X_Lasso_school <-  controls %>% .[,rq2[2:length(rq2)]!=0] %>% as_tibble() %>% names
  if( str_c(beta_tau, ability_g_tau, luck_g_tau, sep = '|') %in% X_Lasso_earn){X_Lasso_earn <- gsub(pattern = str_c(beta_tau ,ability_g_tau, luck_g_tau, sep = '|'), replacement = '') }
  if( str_c(beta_tau, ability_g_tau, luck_g_tau, sep = '|') %in% X_Lasso_school){X_Lasso_school <- gsub(pattern = str_c(beta_tau ,ability_g_tau, luck_g_tau, sep = '|'), replacement = '') }
  formula <- as.formula(str_c('log_hourly_earnings ~ 1', 
                              str_c('rq(edu_years ~ ycomp','+', ability_g_tau,
                                    str_c(X_Lasso_school, collapse = '+'),
                                    ',data = dataset, method =',str_c('"sfn"'),')$fitted.values:',key_param),
                              str_c(X_Lasso_earn, collapse = '+'),
                              ability_g_tau,
                              luck_g_tau,
                               sep = '+') %>% gsub(pattern = '\\+\\+', replacement = '+'))
  #The latter generates a formular object for the simultaneous estimate of 
  #selected quantiles ln(w) conditional on Q_s(tau_a|X,z), X and Z as in the hybrid
  #model in (8)
  
  }
  return(formula)
}



######################################################################################################################################################################################


#Quantile regression of the different stages
f_rq_results <- function(dataset, tau, IV = F, stage = 0, ability_g_tau = 0, luck_g_tau = 0, beta_tau = 0 ){
  #split dataset into, controls, and Y
  if(IV == T){
    Y <- dataset %>% .$log_hourly_earnings %>% as.matrix()
    Z <- dataset %>% .$ycomp %>% as.matrix()
    controls <- dataset %>% select(-log_hourly_earnings, -ycomp) %>% as.matrix()
  } else if(IV == T && stage == 3){
    
    Y <- dataset %>% .$log_hourly_earnings %>% as.matrix()
    specific_quantile <- dataset %>% select(matches(ability_g_tau),matches(luck_g_tau),ycomp, matches(beta)) %>%
      mutate(ability_g_tau = (ability_g_tau)^-1)
    controls <- dataset %>% select(-log_hourly_earnings,-edu_years) %>%
      select(-matches('ability|luck|beta')) %>%
      inner_join(specific_quantile) %>%
      as.matrix()
    Z <- controls 
    
  } else{
    Y <- dataset %>% .$edu_years %>% as.matrix()
    controls <- dataset %>% select(-log_hourly_earnings,-edu_years) %>% as.matrix()
  }
  
  if(IV == F && stage == 0){
  # for quantile effects when education is treated as exogenous
  formula <- f_lasso_cov(controls = controls,IV = IV, stage = stage , Y = Y, tau = tau)
  fit <- rq(formula, tau = tau, method = 'sfn', data = dataset)
 
  #first stage effect of ycomp on s, coefficients = of 2 parameter, fitted values for distribution of ability  a
  }else if(IV == T && stage == 1){
    formula <- f_lasso_cov(controls = controls,IV = IV, stage = stage , Y = Y, Z = Z, tau = tau)
  fit <- rq(formula, tau = tau, method = 'sfn', data = dataset)
  
  #obtain residuals for distribution of luck u
  }else if(IV == T && stage == 2){
    if(ability_g_tau %in% colnames(dataset)){
    formula <- f_lasso_cov(controls = controls,IV = IV, stage = stage , Y = Y, tau = tau, ability_g_tau = ability_g_tau)
    fit <- rq(formula, tau = tau, method = 'sfn', data = dataset)
    
   
    }else {stop('Ability is not defined in the dataset!')}
    #get effect of key parameter
  }else if(IV == T && stage == 3){
    if(beta %in% colnames(dataset) &&  ability_g_tau %in% colnames(dataset) && luck_g_tau %in% colnames(dataset)){
      formula <- f_lasso_cov(controls = controls,IV = IV, stage = stage , Y = Y, tau = tau, ability_g_tau = ability_g_tau, luck_g_tau = luck_g_tau, beta_tau = beta_tau)
      fit <- rq(formula, tau = tau, method = 'sfn', data = dataset)
      
    }else {stop(str_c('Either', ability_g_tau, luck_g_tau,' or ',beta_tau,' of the specified tau(s) is/are not defined in the dataset!'))}
  }
  return(fit)
  }



dataset
#Need to be done for stage 3
#generate key_param
#generate cdf of qunatile distributin function for ability and random error u

'. First, we estimate the conditional quantile
functions of schooling s and compute the control variate
aðsaÞ ¼ s  Qsðsa j X; zÞ ð11Þ
where s is observed schooling and Qsðsa j X; zÞ is the estimated conditional quantile.
Second, we augment the conditional quantile functions of ln (w) with the relevant
control variate and its interaction with schooling. Finally, we simultaneously estimate
the selected quantiles of ln(s) conditional on Qs(sa j X, z), X and z as in the hybrid
model in (8) and obtain the variance–covariance matrix by bootstrapping'