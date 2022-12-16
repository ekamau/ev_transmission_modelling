# Estimate time varying FOI for three cross-sectional serosurveys combined

library("rstan")
library("bayesplot")
library(dplyr)
library(ggplot2)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
rm(list=ls())

##-- Data processing --##

threshold.seropositive <- 8
dat_E71 <- read.csv("data/titers_EVA71.csv", header = TRUE, sep = ",")
dat_E71 <- dat_E71 %>% mutate(seroStatus = case_when(final_Titer >= 8 ~ 'Positive', 
                                                     final_Titer < 8 ~ 'Negative'))
dat_E71$age_rounded <- floor(dat_E71$Age)
age_discrete_Yr <- dat_E71 %>% group_by(Year_collection) %>% 
  filter(age_rounded <= 80) %>% count(age_rounded) 

cum_prev_Ageclass <- dat_E71 %>% group_by(Year_collection, age_rounded) %>% 
  filter(seroStatus == "Positive" & age_rounded <= 80) %>% summarise(seropositive = n()) 

#for missing data points:
cum_prev_Ageclass <- rbind(cum_prev_Ageclass[1:66,], c(Year_collection = 2006, age_rounded = 68,seropositive=0), cum_prev_Ageclass[67:225,])  
cum_prev_Ageclass <- rbind(cum_prev_Ageclass[1:67,], c(Year_collection = 2006,age_rounded = 69,seropositive=0), cum_prev_Ageclass[68:226,]) 
cum_prev_Ageclass <- rbind(cum_prev_Ageclass[1:71,], c(Year_collection = 2006,age_rounded = 74,seropositive=0), cum_prev_Ageclass[72:227,]) 
cum_prev_Ageclass <- rbind(cum_prev_Ageclass[1:76,], c(Year_collection = 2006,age_rounded = 79,seropositive=0), cum_prev_Ageclass[77:228,]) 
cum_prev_Ageclass <- rbind(cum_prev_Ageclass[1:110,], c(Year_collection = 2011,age_rounded = 34,seropositive=0), cum_prev_Ageclass[111:229,]) 
cum_prev_Ageclass <- rbind(cum_prev_Ageclass[1:144,], c(Year_collection = 2011,age_rounded = 70,seropositive=0), cum_prev_Ageclass[145:230,]) 
cum_prev_Ageclass <- rbind(cum_prev_Ageclass[1:152,], c(Year_collection = 2011,age_rounded = 78,seropositive=0), cum_prev_Ageclass[153:231,])

modellingData <- cbind(age_discrete_Yr, cum_prev_Ageclass)
colnames(modellingData)
modellingData <- modellingData %>% select(-c(Year_collection...4, age_rounded...5))
modellingData <- modellingData[-c(1,79,156), ] # remove age 0 data!
df_2006 <- modellingData %>% filter(Year_collection...1 == 2006)
df_2011 <- modellingData %>% filter(Year_collection...1 == 2011)
df_2017 <- modellingData %>% filter(Year_collection...1 == 2017)

df_2006 <- rbind(df_2006[1:54, ], c(2006,55,0,0), df_2006[55:77, ])
df_2006 <- rbind(df_2006[1:61, ], c(2006,62,0,0), df_2006[62:78, ])
df_2006 <- rbind(df_2006[1:72, ], c(2006,73,0,0), df_2006[73:79, ])

df_2011 <- rbind(df_2011[1:12, ], c(2011,13,0,0), df_2011[13:76, ]) 
df_2011 <- rbind(df_2011[1:30, ], c(2011,31,0,0), df_2011[31:77, ]) 
df_2011 <- rbind(df_2011[1:34, ], c(2011,35,0,0), df_2011[35:78, ])
df_2011 <- rbind(df_2011[1:51, ], c(2011,52,0,0), df_2011[52:79, ])

df_2017 <- rbind(df_2017[1:49, ], c(2017,50,0,0), df_2017[50:76, ])
df_2017 <- rbind(df_2017[1:62, ], c(2017,63,0,0), df_2017[63:77, ])
df_2017 <- rbind(df_2017[1:68, ], c(2017,69,0,0), df_2017[69:78, ])
df_2017 <- rbind(df_2017[1:70, ], c(2017,71,0,0), df_2017[71:79, ])

# data for model fitting:
yr3 = 2017; yr2 = 2011; yr1 = 2006

E71_dat <- list(N = 80, yr3 = yr3, yr2 = yr2, yr1 = yr1, 
                n_2006 = df_2006$n, z_2006 = df_2006$seropositive,
                n_2011 = df_2011$n, z_2011 = df_2011$seropositive, 
                n_2017 = df_2017$n, z_2017 = df_2017$seropositive,
                rho = 0.2)

# initialize values:
initf <- function(chain_id = 1) { list(lambda = list(runif(n = (E71_dat$N)+yr3-yr1 , min=0.01, max=0.5)),
                                       sigma = list(rexp(1, 0.1))) }
n_chains <- 4
init_ll <- lapply(1:n_chains, function(id) initf(chain_id = id))

##-- Model fitting --##

fit <- stan(
  file = "src/stan/model4.stan", data = E71_dat, chains = 4, 
  init = "random",  warmup = 3000, iter = 10000, refresh = 5000, 
  control = list(adapt_delta = 0.9999, max_treedepth = 25))

##-- computing LOO approximation --##

library("loo")

# Extract pointwise log-likelihood using merge_chains=FALSE returns an array to use with relative_eff()
pars = c("log_likelihood")
logLik_fit <- extract_log_lik(fit, parameter_name = pars, merge_chains = FALSE)

#relative effective sample sizes to estimate the PSIS effective sample sizes and Monte Carlo error
reff_fit <- relative_eff(exp(logLik_fit), cores = 2)
loo_fit <- loo(logLik_fit, r_eff = reff_fit, cores = 2)
print(loo_fit)

##-- Extract fit summary --##

# Summarize the distributions of estimated parameters and derived quantities using the posterior draws. Generate a matrix with rows corresponding to parameters and columns to the various summary quantities.

fit.summary <- data.frame(summary(fit)$summary)
saveRDS(fit.summary, file = "results/fitSummary_EVA71_model4b.rds")
write.csv(fit.summary, file = "results/fitSummary_EVA71_model4b.csv")
