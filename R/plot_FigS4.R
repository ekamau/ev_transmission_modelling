lapply(c("rstan", "tidyverse", "lubridate", "reshape2", "patchwork", "cowplot", "viridis", "viridisLite"), require, character.only = TRUE)

plot_figS4 <- function() {
  d <- read.csv("data/virologic_data.csv", header = TRUE, sep = ',')
  serotype_df = d %>% count(YEAR, ENGENG) 
  total_df = d %>% count(YEAR)
  
  #(1) EV-D68:
  e68 = serotype_df[serotype_df$ENGENG == '#ent68',]
  head(e68)
  YEAR <- c(2006:2011); ENGENG <- c(rep("#ent68", 6)); n <- c(rep(0, 6))
  e68b <- data.frame(YEAR, ENGENG, n)
  ev68df <- cbind(total_df, rbind(e68b, e68)$n)
  colnames(ev68df) <- c("year", "total", "e68_N")
  ev68df$other <- ev68df$total - ev68df$e68_N
  ev68df$propE68 <- ev68df$e68_N / ev68df$total
  ev68df$propOther <- 1 - ev68df$propE68

  ev68dfB <- melt(ev68df, id.vars = c("year"), measure.vars = c("other", "e68_N"), 
                  variable.name = "serotype", value.name = "count")
  
  q1 <- ggplot(ev68df, aes(x = year, y = propE68)) +
    geom_bar(stat="identity", width=0.8, fill="#CC6666") +
    labs(x = "", y = "Proportion", title = "(A)") +
    scale_x_continuous(breaks=seq(2006,2016),limits = c(2005.5,2016.5)) +
    scale_y_continuous(limits=c(0,0.3)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 14, face = 'bold'))
  
  #(2) CVA16:
  ca16 = serotype_df[serotype_df$ENGENG == '#ca16',]
  ca16df <- cbind(total_df, ca16$n)
  colnames(ca16df) <- c("year", "total", "ca16_N")
  ca16df$other <- ca16df$total - ca16df$ca16_N
  ca16df$propCA16 <- ca16df$ca16_N / ca16df$total
  ca16df$propOther <- 1 - ca16df$propCA16
  
  ca16dfB <- melt(ca16df, id.vars = c("year"), measure.vars = c("total", "ca16_N"), 
                  variable.name = "serotype", value.name = "count")
  
  q2 <- ggplot(ca16df, aes(x = year, y = propCA16)) +
    geom_bar(stat = "identity", width = 0.8, fill = "#9999CC") +
    labs(x = "", y = "", title = "(B)") +
    scale_x_continuous(breaks = seq(2006,2016), limits = c(2005.5,2016.5)) +
    scale_y_continuous(limits = c(0,0.3)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 14, face = 'bold'))
  
  figS4 <- (q1 | q2)
  ggsave('figures/Figure_S4.pdf', figS4, height = 90, width = 183, units = 'mm', dpi = 300)
  
}

plot_figS4()
