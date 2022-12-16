plot_figS3 <- function() {
  d <- read.csv("data/virologic_data.csv", header = TRUE, sep = ',')
  df = d %>% count(YEAR)
  p <- ggplot(df, aes(x = as.factor(YEAR), y = n)) + 
    geom_bar(stat = "identity") +
    labs(x = "", y = "Number") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 14))
  
  ggsave('figures/Figures_S3.pdf', p, height = 90, width = 90, units = 'mm', dpi = 300)
  
}
