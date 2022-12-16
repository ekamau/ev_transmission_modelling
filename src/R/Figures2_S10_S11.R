plot_fig2_figS10_figS11 <- function() {
  # EV-A71:
  dfE71 <- read.csv('data/titers_EVA71.csv', header = TRUE, sep = ',')
  dfE71$age_rounded <- floor(dfE71$Age)
  df <- dfE71 %>% filter(age_rounded >= 1)
  df <- df %>% mutate(Titre = case_when(final_Titer <8 ~ 4,
                                        final_Titer == 8 ~ 8,
                                        final_Titer >=9 & final_Titer <= 16 ~ 16,
                                        final_Titer >= 17 & final_Titer <= 32 ~ 32,
                                        final_Titer >= 33 & final_Titer <= 64 ~ 64,
                                        final_Titer >= 65 & final_Titer <= 128 ~ 128,
                                        final_Titer >= 129 & final_Titer <= 256 ~ 256,
                                        final_Titer >= 257 & final_Titer <= 512 ~ 512,
                                        final_Titer >= 513 & final_Titer <= 2048 ~ 1024))
  
  df <- df %>% mutate(ageGp = case_when(age_rounded > 60 ~ '>60',
                                        age_rounded > 40  & age_rounded <= 60 ~ '41-60',
                                        age_rounded > 20  & age_rounded <= 40 ~ '21-40',
                                        age_rounded > 10  & age_rounded <= 20 ~ '11-20',
                                        age_rounded > 5  & age_rounded <= 10 ~ '6-10',
                                        age_rounded >= 1  & age_rounded <= 5 ~ '1-5'))
  
  df$log2Titre <- log2(df$Titre)
  df$Year_collection[df$Year_collection == 2016] <- 2017
  a <- ggplot(df, aes(log2Titre, fill = factor(ageGp, levels = c("1-5", "6-10", "11-20", "21-40", "41-60", ">60")))) +
    geom_histogram(bins = length(unique(df$log2Titre))) +
    scale_fill_viridis(discrete = TRUE) +
    scale_x_continuous(breaks = seq(min(df$log2Titre), max(df$log2Titre), by = 1)) +
    scale_y_continuous(limits = c(0, 400)) +
    labs(y = 'Count', x = 'log2 titer', title = '(A)') +
    theme_bw() +
    theme(axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'),
          legend.position = 'none')
  
  # CVA6:
  dfCA6 <- read.csv('data/titers_CVA6.csv', header = TRUE, sep = ',')
  dfCA6$age_rounded <- floor(dfCA6$Age)
  dfB <- dfCA6 %>% filter(age_rounded >= 1)
  dfB <- dfB %>% mutate(Titre = case_when(final_Titer <8 ~ 4,
                                          final_Titer == 8 ~ 8,
                                          final_Titer >=9 & final_Titer <= 16 ~ 16,
                                          final_Titer >= 17 & final_Titer <= 32 ~ 32,
                                          final_Titer >= 33 & final_Titer <= 64 ~ 64,
                                          final_Titer >= 65 & final_Titer <= 128 ~ 128,
                                          final_Titer >= 129 & final_Titer <= 256 ~ 256,
                                          final_Titer >= 257 & final_Titer <= 512 ~ 512,
                                          final_Titer >= 513 & final_Titer <= 2048 ~ 1024))
  
  dfB <- dfB %>% mutate(ageGp = case_when(age_rounded > 60 ~ '>60',
                                          age_rounded > 40  & age_rounded <= 60 ~ '41-60',
                                          age_rounded > 20  & age_rounded <= 40 ~ '21-40',
                                          age_rounded > 10  & age_rounded <= 20 ~ '11-20',
                                          age_rounded > 5  & age_rounded <= 10 ~ '6-10',
                                          age_rounded >= 1  & age_rounded <= 5 ~ '1-5'))
  
  dfB$log2Titre <- log2(dfB$Titre)
  dfB$Year_collection[dfB$Year_collection == 2016] <- 2017
  b <- ggplot(dfB, aes(log2Titre, fill = factor(ageGp, levels = c("1-5", "6-10", "11-20", "21-40", "41-60", ">60")))) +
    geom_histogram(bins = length(unique(df$log2Titre))) +
    scale_fill_viridis(discrete = TRUE) +
    scale_x_continuous(breaks = seq(min(df$log2Titre), max(df$log2Titre), by = 1)) +
    scale_y_continuous(limits = c(0, 400)) +
    labs(y = 'Count', x = 'log2 titer', title = '(B)', fill = 'Age') +
    theme_bw() +
    theme(axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'))
  
  # EV-A71 seroprevalence:
  df <- df %>% mutate(seroStatus = case_when(Titre >= 8 ~ 'Positive', Titre < 8 ~ 'Negative'),
                      ageGp2 = case_when(age_rounded >= 80 ~ '80-100',
                                         age_rounded >= 70  & age_rounded < 80 ~ '70-79',
                                         age_rounded >= 60  & age_rounded < 70 ~ '60-69',
                                         age_rounded >= 50  & age_rounded < 60 ~ '50-59',
                                         age_rounded >= 40  & age_rounded < 50 ~ '40-49',
                                         age_rounded >= 30  & age_rounded < 40 ~ '30-39',
                                         age_rounded >= 20  & age_rounded < 30 ~ '20-29',
                                         age_rounded >= 10  & age_rounded < 20 ~ '10-19',
                                         age_rounded >= 5  & age_rounded < 10 ~ '5-9',
                                         age_rounded >= 1  & age_rounded < 5 ~ '1-4',
                                         age_rounded >= 0  & age_rounded < 1 ~ '<1'))
  
  df$ageGp <- factor(df$ageGp); df$ageGp2 <- factor(df$ageGp2)
  samples_Yr_AgeGp <- df %>% group_by(Year_collection, ageGp2) %>% summarise(no = n())
  positive_ageGp_Yr <- df %>% group_by(Year_collection, ageGp2) %>% 
    filter(seroStatus == "Positive") %>% summarise(positives = n())
  
  positive_ageGp_Yr$percPos <- round(((positive_ageGp_Yr$positives / samples_Yr_AgeGp$no) * 100), 1)
  positive_ageGp_Yr$propPos <- round((positive_ageGp_Yr$positives / samples_Yr_AgeGp$no), 3)
  seroprev_df <- merge(positive_ageGp_Yr, samples_Yr_AgeGp) 
  seroprev_df$negatives <- (seroprev_df$no) - (seroprev_df$positives)
  seroprev_df$propNeg <- 1 - (seroprev_df$propPos)
  
  for( i in rownames(seroprev_df) ) {
    seroprev_df[i,"binomCI_low"] <- round(prop.test(seroprev_df[i,"positives"], seroprev_df[i,"no"], 
                                                    conf.level = 0.95, correct = FALSE)$conf.int[1], 3)
    seroprev_df[i,"binomCI_up"] <- round(prop.test(seroprev_df[i,"positives"], seroprev_df[i,"no"], 
                                                   conf.level = 0.95, correct = FALSE)$conf.int[2], 3)
    seroprev_df[i,"binomCI_pval"] <- round(prop.test(seroprev_df[i,"positives"], seroprev_df[i,"no"], 
                                                     conf.level = 0.95, correct = FALSE)$p.value, 3)
    seroprev_df[i,"binomCI_prop"] <- round(prop.test(seroprev_df[i,"positives"], seroprev_df[i,"no"], 
                                                     conf.level = 0.95, correct = FALSE)$estimate, 3)
  }
  
  c <- ggplot(seroprev_df, 
              aes(x = factor(ageGp2, levels = c("<1","1-4","5-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-100")), y = propPos, group = Year_collection)) +
    geom_errorbar(aes(ymin = (binomCI_low), ymax = (binomCI_up), color = factor(Year_collection)), 
                  width=.2, position = position_dodge(0.3)) +
    geom_point(aes(color = factor(Year_collection)), shape = 16, 
               size = 2.5, position = position_dodge(0.3)) +
    #geom_line(aes(color = factor(Year_collection))) +
    labs(x = "Age, years", y = "Seroprevalence", title = "(C)") +
    scale_color_brewer("Year", palette = "Dark2") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0,1), 
                       labels = scales::percent_format(accuracy = 1)) +
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 1, size = 9, angle = 90),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'),
          legend.title = element_text(face = "bold", size = 9),
          legend.position = "none")
  
  # CVA6 seroprevalence:
  dfB <- dfB %>% mutate(seroStatus = case_when(Titre >= 8 ~ 'Positive', Titre < 8 ~ 'Negative'),
                        ageGp2 = case_when(age_rounded >= 80 ~ '80-100',
                                           age_rounded >= 70  & age_rounded < 80 ~ '70-79',
                                           age_rounded >= 60  & age_rounded < 70 ~ '60-69',
                                           age_rounded >= 50  & age_rounded < 60 ~ '50-59',
                                           age_rounded >= 40  & age_rounded < 50 ~ '40-49',
                                           age_rounded >= 30  & age_rounded < 40 ~ '30-39',
                                           age_rounded >= 20  & age_rounded < 30 ~ '20-29',
                                           age_rounded >= 10  & age_rounded < 20 ~ '10-19',
                                           age_rounded >= 5  & age_rounded < 10 ~ '5-9',
                                           age_rounded >= 1  & age_rounded < 5 ~ '1-4',
                                           age_rounded >= 0  & age_rounded < 1 ~ '<1'))
  
  dfB$ageGp <- factor(dfB$ageGp)
  dfB$ageGp2 <- factor(dfB$ageGp2)
  samples_Yr_AgeGpB <- dfB %>% group_by(Year_collection, ageGp2) %>% summarise(no = n())
  positive_ageGp_YrB <- dfB %>% group_by(Year_collection, ageGp2) %>% 
    filter(seroStatus == "Positive") %>% summarise(positives = n())
  
  positive_ageGp_YrB$percPos <- round(((positive_ageGp_YrB$positives / samples_Yr_AgeGpB$no) * 100), 1)
  positive_ageGp_YrB$propPos <- round((positive_ageGp_YrB$positives / samples_Yr_AgeGpB$no), 3)
  seroprev_dfB <- merge(positive_ageGp_YrB, samples_Yr_AgeGpB) 
  seroprev_dfB$negatives <- (seroprev_dfB$no) - (seroprev_dfB$positives)
  seroprev_dfB$propNeg <- 1 - (seroprev_dfB$propPos)
  
  for( i in rownames(seroprev_dfB) ) {
    seroprev_dfB[i,"binomCI_low"] <- round(prop.test(seroprev_dfB[i,"positives"], seroprev_dfB[i,"no"], 
                                                     conf.level = 0.95, correct = FALSE)$conf.int[1], 3)
    seroprev_dfB[i,"binomCI_up"] <- round(prop.test(seroprev_dfB[i,"positives"], seroprev_dfB[i,"no"], 
                                                    conf.level = 0.95, correct = FALSE)$conf.int[2], 3)
    seroprev_dfB[i,"binomCI_pval"] <- round(prop.test(seroprev_dfB[i,"positives"], seroprev_dfB[i,"no"],
                                                      conf.level = 0.95, correct = FALSE)$p.value, 3)
    seroprev_dfB[i,"binomCI_prop"] <- round(prop.test(seroprev_dfB[i,"positives"], seroprev_dfB[i,"no"],
                                                      conf.level = 0.95, correct = FALSE)$estimate, 3)
  }
  
  d <- ggplot(seroprev_dfB, 
              aes(x = factor(ageGp2, levels = c("<1","1-4","5-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-100")), y = propPos, group = Year_collection)) +
    geom_errorbar(aes(ymin = (binomCI_low), ymax = (binomCI_up), color = factor(Year_collection)), 
                  width = .1, position = position_dodge(0.35)) +
    geom_point(aes(color = factor(Year_collection)), shape = 16, size = 2.5, 
               position = position_dodge(0.35)) +
    #geom_line(aes(color = factor(Year_collection))) +
    labs(x = "Age, years", y = "", title = "(D)") +
    scale_color_brewer("Year", palette = "Dark2") +
    theme_classic() +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0,1), 
                       labels = scales::percent_format(accuracy = 1)) +
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 1, size = 9, angle = 90),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'),
          legend.text = element_text(size = 9),
          legend.background = element_blank(), 
          legend.title = element_text(face = "bold", size = 10),
          legend.position = "right") # c(0.7, 0.2)
  
  ### Supplementary figures:
  A <- ggplot(df, aes(log2Titre)) +
    geom_histogram(bins = length(unique(df$log2Titre)), fill = "#1B9E77", alpha = 0.8) +
    scale_x_continuous(breaks = seq(min(df$log2Titre), max(df$log2Titre), by = 1)) +
    labs(y = 'Count', x = '', title = '(A)') +
    theme_bw() +
    theme(axis.text.x = element_text(size = 9, angle = 90),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'), 
          strip.text = element_text(colour = "black", size = 12)) +
    facet_wrap(. ~ Year_collection, nrow = 1, scales = "free")
  
  B <- ggplot(dfB, aes(log2Titre)) +
    geom_histogram(bins = length(unique(dfB$log2Titre)), fill = "#D95F02", alpha = 0.8) +
    scale_x_continuous(breaks = seq(min(dfB$log2Titre), max(dfB$log2Titre), by = 1)) +
    labs(y = 'Count', x = 'log2 titer', title = '(B)') +
    theme_bw() +
    theme(axis.text.x = element_text(size = 9, angle = 90),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'), 
          strip.text = element_text(colour = "black", size = 12)) +
    facet_wrap(. ~ Year_collection, nrow = 1, scales = "free")
  
  # Antibody titers by age group
  df$ageF <- factor(df$ageGp, levels = c("1-5", "6-10", "11-20", "21-40", "41-60", ">60"))
  C <- ggplot(df, aes(log2Titre)) +
    geom_histogram(bins = length(unique(df$log2Titre)), fill = "#1B9E77", alpha = 0.8) +
    scale_x_continuous(breaks = seq(min(df$log2Titre), max(df$log2Titre), by = 1)) +
    #scale_y_continuous(limits = c(0, 120)) +
    labs(y = 'Count', x = '', title = '(A)') +
    theme_bw() +
    theme(axis.text.x = element_text(size = 9, angle = 90),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'),
          strip.text = element_text(colour = "black", size = 10)) +
    facet_wrap(ageF ~ ., scales = "free")
  
  dfB$ageF <- factor(dfB$ageGp, levels = c("1-5", "6-10", "11-20", "21-40", "41-60", ">60"))
  D <- ggplot(dfB, aes(log2Titre)) +
    geom_histogram(bins = length(unique(dfB$log2Titre)), fill = "#D95F02", alpha = 0.8) +
    scale_x_continuous(breaks = seq(min(dfB$log2Titre), max(dfB$log2Titre), by = 1)) +
    #scale_y_continuous(limits = c(0, 90)) +
    labs(y = 'Count', x = 'log2 titer', title = '(B)') +
    theme_bw() +
    theme(axis.text.x = element_text(size = 9, angle = 90),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'),
          strip.text = element_text(colour = "black", size = 10)) +
    facet_wrap(ageF ~ ., scales = "free")
  
  fig2 <- (a | b) / (c | d)
  ggsave('figures/Figure2.pdf', fig2, height = 183, width = 183, units = 'mm', dpi = 300)
  
  figS10 <- A / B
  ggsave('figures/FigureS10.pdf', figS10, height = 183, width = 183, units = 'mm', dpi = 300)
  
  figS11 <- C / D
  ggsave('figures/FigureS11.pdf', figS11, height = 230, width = 183, units = 'mm', dpi = 300)
  
}
