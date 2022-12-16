process_cva6_data <- function(filename = "data/titers_CVA6.csv") {
  dat_CVA6 <- read.csv(filename, header = TRUE, sep = ",")
  dat_CVA6 <- dat_CVA6 %>% mutate(seroStatus = case_when(final_Titer >= 8 ~ 'Positive', 
                                                         final_Titer < 8 ~ 'Negative'))
  dat_CVA6$age_rounded <- floor(dat_CVA6$Age)
  ageYrCA6 <- dat_CVA6 %>% group_by(Year_collection) %>% 
    filter(age_rounded <= 80) %>% count(age_rounded)
  
  prevAgeCA6 <- dat_CVA6 %>% group_by(Year_collection, age_rounded) %>% 
    filter(seroStatus == "Positive" & age_rounded <= 80) %>% summarise(seropositive = n()) 
  
  prevAgeCA6 <- rbind(prevAgeCA6[1:137,], c(Year_collection=2011,age_rounded=63,seropositive=0), prevAgeCA6[138:226,])  
  prevAgeCA6 <- rbind(prevAgeCA6[1:138,], c(Year_collection=2011,age_rounded=64,seropositive=0), prevAgeCA6[139:227,]) 
  prevAgeCA6 <- rbind(prevAgeCA6[1:180,], c(Year_collection=2017,age_rounded=25,seropositive=0), prevAgeCA6[181:228,]) 
  prevAgeCA6 <- rbind(prevAgeCA6[1:209,], c(Year_collection=2017,age_rounded=55,seropositive=0), prevAgeCA6[210:229,]) 
  prevAgeCA6 <- rbind(prevAgeCA6[1:215,], c(Year_collection=2017,age_rounded=61,seropositive=0), prevAgeCA6[216:230,]) 
  prevAgeCA6 <- rbind(prevAgeCA6[1:225,], c(Year_collection=2017,age_rounded=74,seropositive=0), prevAgeCA6[226:231,]) 
  
  modellingDataCA6 <- cbind(ageYrCA6, prevAgeCA6)
  modellingDataCA6 <- modellingDataCA6 %>% select(-c(Year_collection...4, age_rounded...5))
  modellingDataCA6 <- modellingDataCA6[-c(1,79,156), ] # remove data for age 0!
  modellingDataCA6 <- as.data.frame(modellingDataCA6)
  modellingDataCA6$propn <- modellingDataCA6$seropositive / modellingDataCA6$n
  
  CA6_06 <- modellingDataCA6 %>% filter(Year_collection...1 == 2006)
  CA6_11 <- modellingDataCA6 %>% filter(Year_collection...1 == 2011)
  CA6_17 <- modellingDataCA6 %>% filter(Year_collection...1 == 2017)
  
  CA6_06 <- rbind(CA6_06[1:54, ], c(2006, 55, 0, 0, 0), CA6_06[55:77, ])  
  CA6_06 <- rbind(CA6_06[1:61, ], c(2006, 62, 0, 0, 0), CA6_06[62:78, ])
  CA6_06 <- rbind(CA6_06[1:72, ], c(2006, 73, 0, 0, 0), CA6_06[73:79, ])
  
  CA6_11 <- rbind(CA6_11[1:12, ], c(2011, 13, 0, 0, 0), CA6_11[13:76, ]) 
  CA6_11 <- rbind(CA6_11[1:30, ], c(2011, 31, 0, 0, 0), CA6_11[31:77, ]) 
  CA6_11 <- rbind(CA6_11[1:34, ], c(2011, 35, 0, 0, 0), CA6_11[35:78, ])
  CA6_11 <- rbind(CA6_11[1:51, ], c(2011, 52, 0, 0, 0), CA6_11[52:79, ])
  
  CA6_17 <- rbind(CA6_17[1:49, ], c(2017, 50, 0, 0, 0), CA6_17[50:76, ])
  CA6_17 <- rbind(CA6_17[1:62, ], c(2017, 63, 0, 0, 0), CA6_17[63:77, ])
  CA6_17 <- rbind(CA6_17[1:68, ], c(2017, 69, 0, 0, 0), CA6_17[69:78, ])
  CA6_17 <- rbind(CA6_17[1:70, ], c(2017, 71, 0, 0, 0), CA6_17[71:79, ])
  
  output <- list(CA6_06, CA6_11, CA6_17)
  return(output)
}
