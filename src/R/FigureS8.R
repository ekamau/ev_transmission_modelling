plot_figS8 <- function() {
  d <- read.csv("data/virologic_data.csv", header = TRUE, sep = ',')
  sample_l <- readRDS(file = "data/sample_type_list.RData")
  for(name in names(sample_l)){
    #print(name)
    #print(sample_l[[name]])
    sampletypes = sample_l[[name]]
    for(i in 1:nrow(d)){
      #print(d[i, "MAT"])
      if(d[i, "MAT"] %in% sampletypes){
        d[i, "samType"] = name
      }
    }
  }
  
  options(dplyr.summarise.inform = FALSE)
  dF <- d %>% group_by(samType, YEAR) %>% summarise(count_samples = n()) %>% as.data.frame()
  p <- dF[dF$samType %in% c('Blood', 'CSF', 'Skin_swab', 'Faeces', 
                            'Respiratory_sample', 'Gastrointestinal_sample'),] %>%
    ggplot(aes(x = YEAR, y = count_samples)) +
    geom_bar(stat = "identity", width = 0.8, fill = "#999999") +
    labs(x = "", y = "Number") +
    scale_x_continuous(breaks = seq(2006, 2016),limits = c(2005.5, 2016.5)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 8),
          axis.text.y = element_text(size = 9),
          panel.spacing = unit(2, "lines")) +
    facet_wrap( ~ samType, nrow = 2, scales = "free")
  
  ggsave("figures/Figures_S8.pdf", p, height = 103, width = 183, units = "mm", dpi = 300)

} 
