process_eva71_data <- function(filename = "data/titers_EVA71.csv") {
  dat_EVA71 <- read.csv(filename, header = TRUE, sep = ",")
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
  prevAgeE71 <- rbind(prevAgeE71[1:152,], c(Year_collection=2011,age_rounded=78,seropositive=0), prevAgeE71[153:231,])
  modellingDataE71 <- cbind(ageYrE71, prevAgeE71)
  modellingDataE71 <- modellingDataE71 %>% select(-c(Year_collection...4, age_rounded...5))
  modellingDataE71 <- modellingDataE71[-c(1,79,156), ] # remove data for age 0!
  modellingDataE71 <- as.data.frame(modellingDataE71)
  modellingDataE71$propn <- modellingDataE71$seropositive / modellingDataE71$n
  
  E71_06 <- modellingDataE71 %>% filter(Year_collection...1 == 2006)
  E71_11 <- modellingDataE71 %>% filter(Year_collection...1 == 2011)
  E71_17 <- modellingDataE71 %>% filter(Year_collection...1 == 2017)
  
  E71_06 <- rbind(E71_06[1:54, ], c(2006, 55, 0, 0, 0), E71_06[55:77, ])
  E71_06 <- rbind(E71_06[1:61, ], c(2006, 62, 0, 0, 0), E71_06[62:78, ])
  E71_06 <- rbind(E71_06[1:72, ], c(2006, 73, 0, 0, 0), E71_06[73:79, ])
  
  E71_11 <- rbind(E71_11[1:12, ], c(2011, 13, 0, 0, 0), E71_11[13:76, ]) 
  E71_11 <- rbind(E71_11[1:30, ], c(2011, 31, 0, 0, 0), E71_11[31:77, ]) 
  E71_11 <- rbind(E71_11[1:34, ], c(2011, 35, 0, 0, 0), E71_11[35:78, ])
  E71_11 <- rbind(E71_11[1:51, ], c(2011, 52, 0, 0, 0), E71_11[52:79, ])
  
  E71_17 <- rbind(E71_17[1:49, ], c(2017, 50, 0, 0, 0), E71_17[50:76, ])
  E71_17 <- rbind(E71_17[1:62, ], c(2017, 63, 0, 0, 0), E71_17[63:77, ])
  E71_17 <- rbind(E71_17[1:68, ], c(2017, 69, 0, 0, 0), E71_17[69:78, ])
  E71_17 <- rbind(E71_17[1:70, ], c(2017, 71, 0, 0, 0), E71_17[71:79, ])
  
  output <- list(E71_06, E71_11, E71_17)
  return(output)
}
