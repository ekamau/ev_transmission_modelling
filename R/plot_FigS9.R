lapply(c("rstan", "tidyverse", "lubridate", "reshape2", "patchwork", "cowplot", "viridis", "viridisLite"), require, character.only = TRUE)

plot_figS9 <- function() {
  d <- read.csv("data/virologic_data.csv", header = TRUE, sep = ',')
  symptoms_l <- readRDS(file = "data/symptoms_list.RData")
  d$QEWHAT = tolower(d$QEWHAT)
  df <- data.frame(year = integer(), symptom_gp = character())
  
  for(i in 1:nrow(d)){
    symptoms = (unlist(strsplit(d[i, "QEWHAT"], ",")))
    for(str in symptoms){
      for(name in names(symptoms_l)){
        symptoms_group = symptoms_l[[name]]
        if(gsub(" ", "", str, fixed = TRUE) %in% symptoms_group){
          df[nrow(df) + 1,] = c(d[i, "YEAR"], name)
        }
      }
    }
  }
  
  options(dplyr.summarise.inform = FALSE)
  # subset and retain only symptom groups of interest: 
  sympt_grps <- dfF$symptom_gp
  dF <- df %>% filter(symptom_gp %in% sympt_grps)
  p <- dF %>%
    group_by(symptom_gp, year) %>% summarise(freq = n()) %>% as.data.frame() %>%
    ggplot(aes(x = year, y = freq)) +
    geom_bar(stat = "identity", width = 0.8, fill = "#999999") +
    labs(x = "", y = "Number") +
    scale_x_discrete(breaks = seq(2006, 2016, by=1), limits = factor(2006:2016), drop = FALSE) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 8),
          axis.text.y = element_text(size = 9),
          panel.spacing = unit(2, "lines")) +
    facet_wrap( ~ .data$symptom_gp, nrow = 4, ncol = 3, scales = "free")
  
  ggsave("results/figures/Figure_S9.pdf", p, height = 183, width = 183, units = "mm", dpi = 300)
  
}

plot_figS9()

