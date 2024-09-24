# Estimate time varying FOI for three cross-sectional serosurveys combined

library("rstan")
library("bayesplot")
library(dplyr)
library(ggplot2)
library("loo")
library(posterior)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE, threads_per_chain = 1)
rm(list=ls())

##-- Data processing --##

# threshold.seropositive = 8
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

E71_dat <- list(N = 80, yr3 = 2017, yr2 = 2011, yr1 = 2006,
                n_2006 = df_2006$n, z_2006 = df_2006$seropositive,
                n_2011 = df_2011$n, z_2011 = df_2011$seropositive,
                n_2017 = df_2017$n, z_2017 = df_2017$seropositive,
                max_yr_cst = 1995)

# pts = 20
# E71_dat <- list(N = pts, yr3 = 2017, yr2 = 2011, yr1 = 2006,
#                 n_2006 = head(df_2006$n, pts), z_2006 = head(df_2006$seropositive, pts),
#                 n_2011 = head(df_2011$n, pts), z_2011 = head(df_2011$seropositive, pts),
#                 n_2017 = head(df_2017$n, pts), z_2017 = head(df_2017$seropositive, pts),
#                 max_yr_cst = 2005, 
#                 rho = 0.2)

# initialize values:
# initf <- function(chain_id = 1) { list(lambda = list(runif(n = (yr3 - E71_dat$max_yr_cst), min = 0.01, max = 0.5)),
#                                        sigma = list(rexp(1, 1)), lambda_cst = list(rexp(1, 1))) }
# n_chains <- 4
# init_ll <- lapply(1:n_chains, function(id) initf(chain_id = id))

##-- Model fitting --##
# method 1 - MPS
# fit <- stan(file = "src/stan/model3_4_31Oct23/model3-lambdaCst-EK.stan", data = E71_dat, chains = 4, init = init_ll,  warmup = 3000, 
#             iter = 10000, refresh = 5000, 
#             control = list(adapt_delta = 0.9999, max_treedepth = 25))


model_v3 <- stan_model("src/stan/model3_4_31Oct23/model3-lambdaCst-EK.stan")
# fit <- rstan::sampling(model_v3, data = E71_dat, chains = 4, init = init_ll,  warmup = 3000, 
#             iter = 10000, refresh = 5000, 
#             control = list(adapt_delta = 0.9999, max_treedepth = 25))
# 
fit <- rstan::sampling(model_v3, data = E71_dat, chains = 4, init = "random",  warmup = 3000, 
                      iter = 10000, refresh = 5000, 
                      control = list(adapt_delta = 0.9999, max_treedepth = 25))

# opt <- rstan::optimizing(model_v3, data = E71_dat, as_vector = FALSE)
# initf <- function(chain_id = 1) { list( lambda = list(opt$par$lambda), lambda_cst = list(opt$par$lambda_cst), 
#                             sigma = list(opt$par$sigma) ) }
# 
# n_chains <- 4
# init_ll <- lapply(1:n_chains, function(id) initf(chain_id = id))
# 
# fit <- rstan::sampling(model_v3, data = E71_dat, chains = 4, init = init_ll, warmup = 3000, 
#                         iter = 10000, refresh = 5000, 
#                         control = list(adapt_delta = 0.9999, max_treedepth = 25))

saveRDS(fit, file = "results/model3_4_31Oct23/fit_EVA71_model3-lambdaCst-31Oct23.rds")

## analyze the posterior:
# df1 <- rstan::extract(fit) %>% as.data.frame()
# df_sum <- summarize_draws(df1) %>%  as.data.frame()
# sum(df_sum$rhat > 1.01)
# summary(df_sum)

sum_df <- summarise_draws(fit)
sum(sum_df$rhat[!is.na(sum_df$rhat)] > 1.01) == 0
sum(sum_df$ess_tail[!is.na(sum_df$ess_tail)] < 400) == 0
sum(sum_df$ess_bulk[!is.na(sum_df$ess_bulk)] < 400) == 0

##-- computing LOO and WAIC approximation --##
# Extract pointwise log-likelihood using merge_chains=FALSE returns an array to use with relative_eff()
pars = c("log_likelihood")
logLik_fit <- extract_log_lik(fit, parameter_name = pars, merge_chains = FALSE)
dim(logLik_fit)

# waic:
waic(logLik_fit)
waic_m3 <- loo::waic(logLik_fit)
sink('results/EVA71/model3/waic_E71_m3.txt'); print(waic_m3); sink()

#relative effective sample sizes to estimate the PSIS effective sample sizes and Monte Carlo error
reff_fit <- relative_eff(exp(logLik_fit), cores = 2)
loo_fit <- loo(logLik_fit, r_eff = reff_fit, cores = 2)
sink('results/EVA71/model3/loo_E71_m3.txt'); print(loo_fit); sink()

##-- Extract fit summary --##

# Summarize the distributions of estimated parameters and derived quantities using the posterior draws. Generate a matrix with rows corresponding to parameters and columns to the various summary quantities.

fit.summary <- data.frame(summary(fit)$summary)
saveRDS(fit.summary, file = "results/EVA71/model3/fitSummary_E71_m3.rds")
write.csv(fit.summary, file = "results/EVA71/model3/fitSummary_E71_m3.csv")
