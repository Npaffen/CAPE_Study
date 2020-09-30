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

f_ols_resid <- function(dataset, t_){ #generate residuals of regression of years of schooling
  dataset_t <- dataset %>% filter(t == t_, gender == 1)
  
  controls <- dataset_t %>% select(-edu_years, -log_hourly_earnings, -t, -ycomp, -gender)
    
  X_names <- controls %>% names()
  formula <- as.formula(str_c('edu_years ~ 1 +',str_c(X_names, collapse = '+')))
  fit <- unname(lm(formula, data = dataset_t)$residuals)
  resid_men <- mean(fit)
  
  dataset_t <- dataset %>% filter(t == t_, gender == 0,) 
  controls <- dataset_t %>% select(-edu_years, -log_hourly_earnings, -t, -ycomp, -gender)
  
  X_names <- controls %>% names()
  formula <- as.formula(str_c('edu_years ~ 1 +',str_c(X_names, collapse = '+')))
  fit <- unname(lm(formula, data = dataset_t)$residuals)
  resid_women <- mean(fit)
  
  resid <- mean(c(resid_men, resid_women))
  return(resid)

  }

#jitter but return only positive values
jitter_pos <- function(dataset, jitter.val = 0.01){
  #jitter the dataset
  dataset_old <- dataset %>% as.matrix() %>% jitter(jitter.val) %>% as_tibble() %>% mutate_if(is_double, .funs = list(as.numeric))

return(dataset_old)
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
f_formula <- function(controls, Y=0, Z=0, IV, stage = 0 , tau, ability_g_tau = 0, luck_g_tau = 0, tau_school = 0, IV_dataset = 0){
if(IV == F){ #formula for quantile effects when education is treated as exogenpus
  X_names <-  controls  %>% names()
formula <- as.formula(str_c('log_hourly_earnings ~ 1 +' ,str_c(X_names, collapse = '+'), sep = '+')
                      )


}else if(IV == T && stage == 1){ #formula for first stage effect of ycomp on s, coefficients = of 2 parameter, fitted values for distribution of a
  X_names <-  controls  %>% names()
  formula <- list(as.formula(str_c('edu_years ~ 1 +',str_c(X_names , collapse = '+')%>%str_replace(pattern = '\\+ycomp', replace = '' ), sep = '')),
                  as.formula(str_c('edu_years ~ 1 +',str_c(X_names, collapse = '+'), sep = ''))
  )

  }else if(IV == T && stage == 2){ #formula to obtain residuals for distribution of luck u
  X_names <-  controls  %>% names
  formula <- as.formula(str_c('log_hourly_earnings ~ 1 +',str_c(ability_g_tau, '+'),
                              str_c(X_names, collapse = '+'), sep = '') %>% gsub(pattern = '\\+\\+', replacement = '')
                        )
  
  
  }else if(IV == T && stage == 3){ #formula to get effect beta
  X_names_school <-  IV_dataset %>% names
  X_names_earn <-  controls  %>% names
  formula <- list(as.formula(str_c('log_hourly_earnings ~ 1  + quant_school +', 
                              str_c(X_names_earn, collapse = '+'),
                               sep = '' )),
                  as.formula(str_c('edu_years ~ 1 +', 
                        str_c(X_names_school, collapse = '+', sep = '')))
  )
                        }
  
  
  
  return(formula)
}

 


######################################################################################################################################################################################


#Quantile regression of the different stages
f_rq_results <- function(dataset, tau, IV = F, stage = 0, ability_g_tau = 0, luck_g_tau = 0, tau_school= 0, beta_tau =0 ){
  #split dataset into, controls, and Y
  if(IV == T && stage == 1){
    controls <- dataset_male %>% select(-log_hourly_earnings, -edu_years, -matches('ability|luck'))
    
  } else if(IV == T && stage == 2){
    controls <- dataset %>% select(-log_hourly_earnings, -edu_years, -matches('ability|luck|beta'), -ycomp)
    } else if(IV == T && stage == 3){
    
    
    controls <- dataset %>% select(-log_hourly_earnings,-edu_years, -ycomp, -matches('ability|luck|beta'))
    IV_dataset <- dataset %>% select(-log_hourly_earnings, -edu_years, -matches('ability|luck|beta'))
   
    
    }else{
    controls <- dataset %>% select(-log_hourly_earnings, -ycomp) %>% select(-edu_years, everything(), edu_years) 
  }
  
  if(IV == F && stage == 0){
  # for quantile effects when education is treated as exogenous
  formula <- f_formula(controls = controls,IV = IV, stage = stage , Y = Y, tau = tau)
  fit <- rq(formula, tau = tau, method = 'sfn', data = dataset)
 
  #first stage effect of ycomp on s, coefficients = of 2 parameter, fitted values for distribution of ability  a
  }else if(IV == T && stage == 1){
  formula <- f_formula(controls = controls,IV = IV, stage = stage , Y = Y, tau = tau)
  fit <- rq(formula[[2]], tau = tau, method = 'sfn', data = dataset)
  
  #obtain residuals for distribution of luck u
  }else if(IV == T && stage == 2){
    if(ability_g_tau %in% colnames(dataset)){
    formula <- f_formula(controls = controls,IV = IV, stage = stage , Y = Y, tau = tau, ability_g_tau = ability_g_tau, IV_dataset = IV_dataset)
    fit <- rq(formula, tau = tau, method = 'sfn', data = dataset)
    
   
    }else {stop('Ability is not defined in the dataset!')}
    #get effect of key parameter
  }else if(IV == T && stage == 3){

      formula <- f_formula(controls = controls,IV = IV, stage = stage , Y = Y, tau = tau, ability_g_tau = ability_g_tau, luck_g_tau = luck_g_tau, tau_school = tau_school,
                             IV_dataset = IV_dataset)
      fit_1 <- rq(formula[[2]], tau = tau_school, method = 'sfn', data = dataset)
      dataset <- dataset %>% mutate(quant_school = fit_1$fitted.values)
      fit <-   rq(formula[[1]], tau = tau, method = 'sfn', data = dataset)

  }
  return(fit)
  }

f_iv_ftest <- function(dataset, tau, IV = T, stage = 1, ability_g_tau = 0, luck_g_tau = 0, beta_tau = 0 ){
  Y <- dataset %>% .$edu_years %>% as.matrix()
  controls_1 <- dataset %>% select(-log_hourly_earnings,-edu_years, -ycomp,  -matches('ability|luck|beta')) %>% as.matrix()
  controls_2 <- dataset %>% select(-log_hourly_earnings,-edu_years,  -matches('ability|luck|beta')) %>% as.matrix()
  X_names_f1 <-  controls_1 %>% as_tibble() %>% names()
  X_names_f2 <-  controls_2 %>% as_tibble() %>% names()
  formula <- list(as.formula(str_c('edu_years ~ 1 +',str_c(X_names_f1 , collapse = '+'), sep = '')),
                  as.formula(str_c('edu_years ~ 1 +',str_c(X_names_f2, collapse = '+'), sep = ''))
  )
  fit_1 <- rq(formula[[1]], tau = tau, method = 'sfn', data = dataset) #without instrument
  fit_2 <- rq(formula[[2]], tau = tau, method = 'sfn', data = dataset) #with instrument
  names(fit_1$coefficients) <- X_names_f1
  names(fit_2$coefficients) <- X_names_f2
  ftest <- anova.rqlist(list(fit_2, fit_1), se = 'boot', method = 'Wald')
  return(ftest)

  }


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




