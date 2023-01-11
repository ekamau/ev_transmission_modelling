plot_fig4 <- function(){
  # CVA6:
  dat_CVA6 <- read.csv("data/titers_CVA6.csv", header = TRUE, sep = ",")
  dat_CVA6 <- dat_CVA6 %>% mutate(seroStatus = case_when(final_Titer >= 8 ~ 'Positive',
                                                         final_Titer < 8 ~ 'Negative'))
  dat_CVA6$age_rounded <- floor(dat_CVA6$Age)
  ageYrCA6 <- dat_CVA6 %>% group_by(Year_collection) %>% 
    filter(age_rounded <= 80) %>% count(age_rounded)
  
  prevAgeCA6 <- dat_CVA6 %>% group_by(Year_collection, age_rounded) %>% 
    filter(seroStatus == "Positive" & age_rounded <= 80) %>% summarise(seropositive = n()) 
  
  prevAgeCA6 <- rbind(prevAgeCA6[1:137,], c(Year_collection = 2011,age_rounded = 63,seropositive = 0), prevAgeCA6[138:226,])  
  prevAgeCA6 <- rbind(prevAgeCA6[1:138,], c(Year_collection = 2011,age_rounded = 64,seropositive = 0), prevAgeCA6[139:227,]) 
  prevAgeCA6 <- rbind(prevAgeCA6[1:180,], c(Year_collection = 2017,age_rounded = 25,seropositive = 0), prevAgeCA6[181:228,]) 
  prevAgeCA6 <- rbind(prevAgeCA6[1:209,], c(Year_collection = 2017,age_rounded = 55,seropositive = 0), prevAgeCA6[210:229,]) 
  prevAgeCA6 <- rbind(prevAgeCA6[1:215,], c(Year_collection = 2017,age_rounded = 61,seropositive = 0), prevAgeCA6[216:230,]) 
  prevAgeCA6 <- rbind(prevAgeCA6[1:225,], c(Year_collection = 2017,age_rounded = 74,seropositive = 0), prevAgeCA6[226:231,]) 
  
  modellingDataCA6 <- cbind(ageYrCA6, prevAgeCA6)
  colnames(modellingDataCA6)
  modellingDataCA6 <- modellingDataCA6 %>% select(-c(Year_collection...4, age_rounded...5))
  modellingDataCA6 <- modellingDataCA6[-c(1,79,156), ]
  modellingDataCA6 <- as.data.frame(modellingDataCA6)
  modellingDataCA6$propn <- modellingDataCA6$seropositive / modellingDataCA6$n
  
  CA6_06 <- modellingDataCA6 %>% filter(Year_collection...1 == 2006)
  CA6_11 <- modellingDataCA6 %>% filter(Year_collection...1 == 2011)
  CA6_17 <- modellingDataCA6 %>% filter(Year_collection...1 == 2017)
  
  CA6_06 <- rbind(CA6_06[1:54,], c(2006,55,0,0,0), CA6_06[55:77,]) 
  CA6_06 <- rbind(CA6_06[1:61,], c(2006,62,0,0,0), CA6_06[62:78,])
  CA6_06 <- rbind(CA6_06[1:72,], c(2006,73,0,0,0), CA6_06[73:79,])
  
  CA6_11 <- rbind(CA6_11[1:12,], c(2011,13,0,0,0), CA6_11[13:76,]) 
  CA6_11 <- rbind(CA6_11[1:30,], c(2011,31,0,0,0), CA6_11[31:77,]) 
  CA6_11 <- rbind(CA6_11[1:34,], c(2011,35,0,0,0), CA6_11[35:78,])
  CA6_11 <- rbind(CA6_11[1:51,], c(2011,52,0,0,0), CA6_11[52:79,])
  
  CA6_17 <- rbind(CA6_17[1:49,], c(2017,50,0,0,0), CA6_17[50:76,])
  CA6_17 <- rbind(CA6_17[1:62,], c(2017,63,0,0,0), CA6_17[63:77,])
  CA6_17 <- rbind(CA6_17[1:68,], c(2017,69,0,0,0), CA6_17[69:78,])
  CA6_17 <- rbind(CA6_17[1:70,], c(2017,71,0,0,0), CA6_17[71:79,])
  
  fit_CA6 <- read.csv('data/CA6_fit_summary_model2.csv', header = TRUE, sep = ',')
  rownames(fit_CA6) <- fit_CA6[,1]
  fit_CA6[,1] <- NULL
  est_CA6_06 <- data.frame(mean=rep(NA,times=80), low=rep(NA,times=80), up=rep(NA,times=80))
  est_CA6_11 <- data.frame(mean=rep(NA,times=80), low=rep(NA,times=80), up=rep(NA,times=80))
  est_CA6_17 <- data.frame(mean=rep(NA,times=80), low=rep(NA,times=80), up=rep(NA,times=80))
  
  est_CA6_06$mean = fit_CA6$mean[grep("z_sim_2006",rownames(fit_CA6))]
  est_CA6_06$low  = fit_CA6$X2.5.[grep("z_sim_2006",rownames(fit_CA6))]
  est_CA6_06$up   = fit_CA6$X97.5.[grep("z_sim_2006",rownames(fit_CA6))]
  
  est_CA6_11$mean = fit_CA6$mean[grep("z_sim_2011",rownames(fit_CA6))]
  est_CA6_11$low  = fit_CA6$X2.5.[grep("z_sim_2011",rownames(fit_CA6))]
  est_CA6_11$up   = fit_CA6$X97.5.[grep("z_sim_2011",rownames(fit_CA6))]
  
  est_CA6_17$mean = fit_CA6$mean[grep("z_sim_2017",rownames(fit_CA6))]
  est_CA6_17$low  = fit_CA6$X2.5.[grep("z_sim_2017",rownames(fit_CA6))]
  est_CA6_17$up   = fit_CA6$X97.5.[grep("z_sim_2017",rownames(fit_CA6))]
  
  df06_CA6 <- cbind(CA6_06[,], est_CA6_06[,])
  df06_CA6$estProp06 = df06_CA6$mean/df06_CA6$n # estimated seroprevalence
  df06_CA6$estProp06low = df06_CA6$low/df06_CA6$n
  df06_CA6$estProp06up = df06_CA6$up/df06_CA6$n
  
  df11_CA6 <- cbind(CA6_11[,], est_CA6_11[,])
  df11_CA6$estProp11 = df11_CA6$mean/df11_CA6$n # estimated seroprevalence
  df11_CA6$estProp11low = df11_CA6$low/df11_CA6$n
  df11_CA6$estProp11up = df11_CA6$up/df11_CA6$n
  
  df17_CA6 <- cbind(CA6_17[,], est_CA6_17[,])
  df17_CA6$estProp17 = df17_CA6$mean/df17_CA6$n # estimated seroprevalence
  df17_CA6$estProp17low = df17_CA6$low/df17_CA6$n
  df17_CA6$estProp17up = df17_CA6$up/df17_CA6$n
  
  colnames(df06_CA6) <- c("Year","age","n","seropositive","propn","mean","low","up","estProp","estProplow","estPropup")
  colnames(df11_CA6) <- c("Year","age","n","seropositive","propn","mean","low","up","estProp","estProplow","estPropup")
  colnames(df17_CA6) <- c("Year","age","n","seropositive","propn","mean","low","up","estProp","estProplow","estPropup")
  df_fitCA6 <- bind_rows(df06_CA6, df11_CA6, df17_CA6)
  
  c <- ggplot() +
    geom_errorbar(data = df_fitCA6, aes(ymin = estProplow, ymax = estPropup, x = age), 
                  color = "#D95F02", width = 0.7, size = 0.5) +
    geom_line(data = df_fitCA6, aes(x = age, y = estProp), color = "#D95F02", size=0.8) +
    #geom_pointrange(data = df_fitCA6, aes( ymin = LCB, ymax = UCB),  color = 'black') +
    geom_point(data = df_fitCA6, aes(x = age, y = propn), shape = 1, size = 0.2, 
               color = 'black') + 
    theme_bw() +
    facet_wrap(~Year) +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 9),
          plot.title = element_text(face = 'bold', size = 14),
          axis.title = element_text(size = 11),
          legend.position = 'none',
          strip.text.x = element_text(size = 11),
          strip.background = element_rect(fill = "gray90")) + 
    scale_x_continuous(breaks = seq(from = 0, to = 80, by = 20)) + 
    scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.4), 
                       labels = scales::percent_format(accuracy = 1)) + 
    labs(y = 'Seroprevalence', x = '', title = '(C)')
  
  c
  
  # Fit data - model 3:
  
  fit_CA6B <- read.csv('data/CA6_fit_summary_model3.csv', header = TRUE, sep = ',')
  rownames(fit_CA6B) <- fit_CA6B[,1]
  fit_CA6B[,1] <- NULL
  est_CA6B_06 <- data.frame(mean=rep(NA,times=80), low=rep(NA,times=80), up=rep(NA,times=80))
  est_CA6B_11 <- data.frame(mean=rep(NA,times=80), low=rep(NA,times=80), up=rep(NA,times=80))
  est_CA6B_17 <- data.frame(mean=rep(NA,times=80), low=rep(NA,times=80), up=rep(NA,times=80))
  
  est_CA6B_06$mean = fit_CA6B$mean[grep("z_sim_2006",rownames(fit_CA6B))]
  est_CA6B_06$low  = fit_CA6B$X2.5.[grep("z_sim_2006",rownames(fit_CA6B))]
  est_CA6B_06$up   = fit_CA6B$X97.5.[grep("z_sim_2006",rownames(fit_CA6B))]
  
  est_CA6B_11$mean = fit_CA6B$mean[grep("z_sim_2011",rownames(fit_CA6B))]
  est_CA6B_11$low  = fit_CA6B$X2.5.[grep("z_sim_2011",rownames(fit_CA6B))]
  est_CA6B_11$up   = fit_CA6B$X97.5.[grep("z_sim_2011",rownames(fit_CA6B))]
  
  est_CA6B_17$mean = fit_CA6B$mean[grep("z_sim_2017",rownames(fit_CA6B))]
  est_CA6B_17$low  = fit_CA6B$X2.5.[grep("z_sim_2017",rownames(fit_CA6B))]
  est_CA6B_17$up   = fit_CA6B$X97.5.[grep("z_sim_2017",rownames(fit_CA6B))]
  
  df06_CA6B <- cbind(CA6_06[,], est_CA6B_06[,])
  df06_CA6B$estProp06 = df06_CA6B$mean/df06_CA6B$n # estimated seroprevalence
  df06_CA6B$estProp06low = df06_CA6B$low/df06_CA6B$n
  df06_CA6B$estProp06up = df06_CA6B$up/df06_CA6B$n
  
  df11_CA6B <- cbind(CA6_11[,], est_CA6B_11[,])
  df11_CA6B$estProp11 = df11_CA6B$mean/df11_CA6B$n # estimated seroprevalence
  df11_CA6B$estProp11low = df11_CA6B$low/df11_CA6B$n
  df11_CA6B$estProp11up = df11_CA6B$up/df11_CA6B$n
  
  df17_CA6B <- cbind(CA6_17[,], est_CA6B_17[,])
  df17_CA6B$estProp17 = df17_CA6B$mean/df17_CA6B$n # estimated seroprevalence
  df17_CA6B$estProp17low = df17_CA6B$low/df17_CA6B$n
  df17_CA6B$estProp17up = df17_CA6B$up/df17_CA6B$n
  
  colnames(df06_CA6B) <- c("Year","age","n","seropositive","propn","mean","low","up","estProp","estProplow","estPropup")
  colnames(df11_CA6B) <- c("Year","age","n","seropositive","propn","mean","low","up","estProp","estProplow","estPropup")
  colnames(df17_CA6B) <- c("Year","age","n","seropositive","propn","mean","low","up","estProp","estProplow","estPropup")
  df_fitCA6B <- bind_rows(df06_CA6B, df11_CA6B, df17_CA6B)
  
  d <- ggplot() +
    geom_errorbar(data = df_fitCA6B, aes(x = age, ymin = estProplow, ymax = estPropup), 
                  color = "#D95F02", width = 0.7, size = 0.5) +
    geom_line(data = df_fitCA6B, aes(x = age, y = estProp), color = "#D95F02", size=0.8) +
    #geom_pointrange(data = df_fitCA6, aes( ymin = LCB, ymax = UCB),  color = 'black') + # if plotting binomial intervals!
    geom_point(data = df_fitCA6B, aes(x = age, y = propn), shape = 1, size=0.2, color = 'black') + 
    theme_bw() +
    facet_wrap(~Year) +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 9),
          plot.title = element_text(face = 'bold', size = 14),
          axis.title = element_text(size = 11),
          legend.position = 'none',
          strip.text.x = element_text(size = 11),
          strip.background = element_rect(fill = "gray90")) + 
    scale_x_continuous(breaks = seq(from = 0, to = 80, by = 20)) + 
    scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.4), 
                       labels = scales::percent_format(accuracy = 1)) + 
    labs(y = 'Seroprevalence', x = 'Age, years', title = '(D)')
  
  d
  
  # EV-A71:
  dat_EVA71 <- read.csv("data/titers_EVA71.csv", header = TRUE, sep = ",")
  dat_EVA71 <- dat_EVA71 %>% mutate(seroStatus = case_when(final_Titer >= 8 ~ 'Positive',
                                                           final_Titer < 8 ~ 'Negative'))
  dat_EVA71$age_rounded <- floor(dat_EVA71$Age)
  ageYrE71 <- dat_EVA71 %>% group_by(Year_collection) %>% 
    filter(age_rounded <= 80) %>% count(age_rounded)
  
  prevAgeE71 <- dat_EVA71 %>% group_by(Year_collection, age_rounded) %>% 
    filter(seroStatus == "Positive" & age_rounded <= 80) %>% summarise(seropositive = n())
  
  prevAgeE71 <- rbind(prevAgeE71[1:66,], c(Year_collection=2006,age_rounded=68,seropositive=0), prevAgeE71[67:225,])  
  prevAgeE71 <- rbind(prevAgeE71[1:67,], c(Year_collection=2006,age_rounded=69,seropositive=0), prevAgeE71[68:226,]) 
  prevAgeE71 <- rbind(prevAgeE71[1:71,], c(Year_collection=2006,age_rounded=74,seropositive=0), prevAgeE71[72:227,]) 
  prevAgeE71 <- rbind(prevAgeE71[1:76,], c(Year_collection=2006,age_rounded=79,seropositive=0), prevAgeE71[77:228,]) 
  prevAgeE71 <- rbind(prevAgeE71[1:110,], c(Year_collection=2011,age_rounded=34,seropositive=0), prevAgeE71[111:229,]) 
  prevAgeE71 <- rbind(prevAgeE71[1:144,], c(Year_collection=2011,age_rounded=70,seropositive=0), prevAgeE71[145:230,]) 
  prevAgeE71 <- rbind(prevAgeE71[1:152,], c(Year_collection=2011,age_rounded=78,seropositive=0), prevAgeE71[153:231,]) # 
  modellingDataE71 <- cbind(ageYrE71, prevAgeE71)
  colnames(modellingDataE71)
  
  modellingDataE71 <- modellingDataE71 %>% select(-c(Year_collection...4, age_rounded...5))
  modellingDataE71 <- modellingDataE71[-c(1,79,156), ] # remove age 0 data!
  modellingDataE71 <- as.data.frame(modellingDataE71)
  modellingDataE71$propn <- modellingDataE71$seropositive / modellingDataE71$n
  
  E71_06 <- modellingDataE71 %>% filter(Year_collection...1 == 2006)
  E71_11 <- modellingDataE71 %>% filter(Year_collection...1 == 2011)
  E71_17 <- modellingDataE71 %>% filter(Year_collection...1 == 2017)
  
  E71_06 <- rbind(E71_06[1:54,], c(2006,55,0,0,0), E71_06[55:77,]) 
  E71_06 <- rbind(E71_06[1:61,], c(2006,62,0,0,0), E71_06[62:78,])
  E71_06 <- rbind(E71_06[1:72,], c(2006,73,0,0,0), E71_06[73:79,])
  
  E71_11 <- rbind(E71_11[1:12,], c(2011,13,0,0,0), E71_11[13:76,]) 
  E71_11 <- rbind(E71_11[1:30,], c(2011,31,0,0,0), E71_11[31:77,]) 
  E71_11 <- rbind(E71_11[1:34,], c(2011,35,0,0,0), E71_11[35:78,])
  E71_11 <- rbind(E71_11[1:51,], c(2011,52,0,0,0), E71_11[52:79,])
  
  E71_17 <- rbind(E71_17[1:49,], c(2017,50,0,0,0), E71_17[50:76,])
  E71_17 <- rbind(E71_17[1:62,], c(2017,63,0,0,0), E71_17[63:77,])
  E71_17 <- rbind(E71_17[1:68,], c(2017,69,0,0,0), E71_17[69:78,])
  E71_17 <- rbind(E71_17[1:70,], c(2017,71,0,0,0), E71_17[71:79,])
  
  # Fit data - model 2:
  
  fit_E71 <- read.csv('data/E71_fit_summary_model2.csv', header = TRUE, sep = ',')
  rownames(fit_E71) <- fit_E71[,1]
  fit_E71[,1] <- NULL
  est_E71_06 <- data.frame(mean=rep(NA,times=80), low=rep(NA,times=80), up=rep(NA,times=80))
  est_E71_11 <- data.frame(mean=rep(NA,times=80), low=rep(NA,times=80), up=rep(NA,times=80))
  est_E71_17 <- data.frame(mean=rep(NA,times=80), low=rep(NA,times=80), up=rep(NA,times=80))
  
  est_E71_06$mean = fit_E71$mean[grep("z_sim_2006",rownames(fit_E71))]
  est_E71_06$low  = fit_E71$X2.5.[grep("z_sim_2006",rownames(fit_E71))]
  est_E71_06$up   = fit_E71$X97.5.[grep("z_sim_2006",rownames(fit_E71))]
  
  est_E71_11$mean = fit_E71$mean[grep("z_sim_2011",rownames(fit_E71))]
  est_E71_11$low  = fit_E71$X2.5.[grep("z_sim_2011",rownames(fit_E71))]
  est_E71_11$up   = fit_E71$X97.5.[grep("z_sim_2011",rownames(fit_E71))]
  
  est_E71_17$mean = fit_E71$mean[grep("z_sim_2017",rownames(fit_E71))]
  est_E71_17$low  = fit_E71$X2.5.[grep("z_sim_2017",rownames(fit_E71))]
  est_E71_17$up   = fit_E71$X97.5.[grep("z_sim_2017",rownames(fit_E71))]
  
  df06_E71 <- cbind(E71_06[,], est_E71_06[,])
  df06_E71$estProp06 = df06_E71$mean/df06_E71$n # estimated seroprevalence
  df06_E71$estProp06low = df06_E71$low/df06_E71$n
  df06_E71$estProp06up = df06_E71$up/df06_E71$n
  
  df11_E71 <- cbind(E71_11[,], est_E71_11[,])
  df11_E71$estProp11 = df11_E71$mean/df11_E71$n # estimated seroprevalence
  df11_E71$estProp11low = df11_E71$low/df11_E71$n
  df11_E71$estProp11up = df11_E71$up/df11_E71$n
  
  df17_E71 <- cbind(E71_17[,], est_E71_17[,])
  df17_E71$estProp17 = df17_E71$mean/df17_E71$n # estimated seroprevalence
  df17_E71$estProp17low = df17_E71$low/df17_E71$n
  df17_E71$estProp17up = df17_E71$up/df17_E71$n
  
  colnames(df06_E71)
  colnames(df06_E71) <- c("Year","age","n","seropositive","propn","mean","low","up","estProp","estProplow","estPropup")
  colnames(df11_E71) <- c("Year","age","n","seropositive","propn","mean","low","up","estProp","estProplow","estPropup")
  colnames(df17_E71) <- c("Year","age","n","seropositive","propn","mean","low","up","estProp","estProplow","estPropup")
  df_fitE71 <- bind_rows(df06_E71, df11_E71, df17_E71)
  
  a <- ggplot() +
    geom_errorbar(data = df_fitCA6, aes(ymin = estProplow, ymax = estPropup, x = age), 
                  color = "#1B9E77", width = 0.7, size = 0.5) +
    geom_line(data = df_fitE71, aes(x = age, y = estProp), color = "#1B9E77", size=0.8) +
    geom_point(data = df_fitE71, aes(x = age, y = propn), shape = 1, size=0.2, 
               color = 'black') + 
    theme_bw() +
    facet_wrap(~Year) +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 9),
          plot.title = element_text(face = 'bold', size = 14),
          axis.title = element_text(size = 11),
          legend.position = 'none',
          strip.text.x = element_text(size = 11),
          strip.background = element_rect(fill = "gray90")) + 
    scale_x_continuous(breaks = seq(from = 0, to = 80, by = 20)) + 
    scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.4), 
                       labels = scales::percent_format(accuracy = 1)) + 
    labs(y = 'Seroprevalence', x = '', title = '(A)')
  
  a
  
  # Fit data - model 3:
  
  fit_E71B <- read.csv('data/E71_fit_summary_model3.csv', header = TRUE, sep = ',')
  rownames(fit_E71B) <- fit_E71B[,1]
  fit_E71B[,1] <- NULL
  est_E71B_06 <- data.frame(mean=rep(NA,times=80), low=rep(NA,times=80), up=rep(NA,times=80))
  est_E71B_11 <- data.frame(mean=rep(NA,times=80), low=rep(NA,times=80), up=rep(NA,times=80))
  est_E71B_17 <- data.frame(mean=rep(NA,times=80), low=rep(NA,times=80), up=rep(NA,times=80))
  
  est_E71B_06$mean = fit_E71B$mean[grep("z_sim_2006",rownames(fit_E71B))]
  est_E71B_06$low  = fit_E71B$X2.5.[grep("z_sim_2006",rownames(fit_E71B))]
  est_E71B_06$up   = fit_E71B$X97.5.[grep("z_sim_2006",rownames(fit_E71B))]
  
  est_E71B_11$mean = fit_E71B$mean[grep("z_sim_2011",rownames(fit_E71B))]
  est_E71B_11$low  = fit_E71B$X2.5.[grep("z_sim_2011",rownames(fit_E71B))]
  est_E71B_11$up   = fit_E71B$X97.5.[grep("z_sim_2011",rownames(fit_E71B))]
  
  est_E71B_17$mean = fit_E71B$mean[grep("z_sim_2017",rownames(fit_E71B))]
  est_E71B_17$low  = fit_E71B$X2.5.[grep("z_sim_2017",rownames(fit_E71B))]
  est_E71B_17$up   = fit_E71B$X97.5.[grep("z_sim_2017",rownames(fit_E71B))]
  
  df06_E71B <- cbind(E71_06[,], est_E71B_06[,])
  df06_E71B$estProp06 = df06_E71B$mean/df06_E71B$n # estimated seroprevalence
  df06_E71B$estProp06low = df06_E71B$low/df06_E71B$n
  df06_E71B$estProp06up = df06_E71B$up/df06_E71B$n
  
  df11_E71B <- cbind(E71_11[,], est_E71B_11[,])
  df11_E71B$estProp11 = df11_E71B$mean/df11_E71B$n # estimated seroprevalence
  df11_E71B$estProp11low = df11_E71B$low/df11_E71B$n
  df11_E71B$estProp11up = df11_E71B$up/df11_E71B$n
  
  df17_E71B <- cbind(E71_17[,], est_E71B_17[,])
  df17_E71B$estProp17 = df17_E71B$mean/df17_E71B$n # estimated seroprevalence
  df17_E71B$estProp17low = df17_E71B$low/df17_E71B$n
  df17_E71B$estProp17up = df17_E71B$up/df17_E71B$n
  
  colnames(df06_E71B)
  colnames(df06_E71B) <- c("Year","age","n","seropositive","propn","mean","low","up","estProp","estProplow","estPropup")
  colnames(df11_E71B) <- c("Year","age","n","seropositive","propn","mean","low","up","estProp","estProplow","estPropup")
  colnames(df17_E71B) <- c("Year","age","n","seropositive","propn","mean","low","up","estProp","estProplow","estPropup")
  df_fitE71B <- bind_rows(df06_E71B, df11_E71B, df17_E71B)
  
  b <- ggplot() +
    geom_errorbar(data = df_fitE71B, aes(x = age, ymin = estProplow, ymax = estPropup), 
                  color = "#1B9E77", width = 0.7, size = 0.5) +
    geom_line(data = df_fitE71B, aes(x = age, y = estProp), color = "#1B9E77", size=0.8) +
    #geom_pointrange(data = df_fitE71, aes( ymin = LCB, ymax = UCB),  color = 'black') +
    geom_point(data = df_fitE71B, aes(x = age, y = propn), shape = 1, size=0.2, 
               color = 'black') + 
    theme_bw() +
    facet_wrap(~Year) +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 9),
          plot.title = element_text(face = 'bold', size = 14),
          axis.title = element_text(size = 11),
          legend.position = 'none',
          strip.text.x = element_text(size = 11),
          strip.background = element_rect(fill = "gray90")) + 
    scale_x_continuous(breaks = seq(from = 0, to = 80, by = 20)) + 
    scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.4), labels = scales::percent_format(accuracy = 1)) + 
    labs(y = 'Seroprevalence', x = '', title = '(B)')
  
  b
  
  fig4 <- a / b / c / d
  ggsave(filename = 'figures/Figure4.pdf', fig4, height = 200, width = 183, units = 'mm', dpi = 300)
  
}


