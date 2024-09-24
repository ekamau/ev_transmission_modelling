lapply(c("rstan", "tidyverse", "lubridate", "reshape2", "patchwork", "cowplot", "viridis", "viridisLite"), require, character.only = TRUE)

plot_figS5_S6 <- function() {
  d <- read.csv("data/virologic_data.csv", header = TRUE, sep = ',')
  
  # Cities reporting the most
  d$CITY[d$CITY == "LAMBETH"] <- "LONDON"
  d$CITY[d$CITY == "WHITECHAPEL"] <- "LONDON"
  
  cities_above_50 <- d %>% group_by(CITY) %>% summarize(freq = n()) %>%
    filter(freq > 50) %>% as.data.frame() # retains 17 cities
  total_df = d %>% count(YEAR)
  d$CITY[!(d$CITY %in% cities_above_50$CITY)] = "Other"
  city_df = d %>% count(YEAR, CITY)
  for(i in 1:nrow(city_df)){
    city_df[i, "total"] <- total_df$n[total_df$YEAR == city_df[i, "YEAR"]]
  }
  
  city_df$prop <- city_df$n / city_df$total
  cities_df <- data.frame(CITY = character(), YEAR = integer(), counts = integer(), number = integer())
  for(city in cities_above_50$CITY){
    df <- melt(city_df %>% filter(CITY == city), 
               id.vars = c("CITY", "YEAR"), variable.name = "counts", value.name = "number")
    cities_df <- rbind(cities_df, df)
  }
  levels = factor(c("n", "total"))
  
  p <- filter(city_df, ! CITY %in% c("Other")) %>% 
    ggplot(aes(x = as.factor(YEAR), y = prop)) +
    geom_bar(stat = "identity", width = 0.8) +
    labs(x = "", y = "Proportion") +
    scale_x_discrete(breaks = seq(2006, 2016, by=1), limits = factor(2006:2016), drop = FALSE) +
    scale_y_continuous(limits=c(0,0.5)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 7),
          axis.text.y = element_text(size = 8),
          strip.text.x = element_text(size = 8)) +
    facet_wrap( ~ .data$CITY, nrow = 5, ncol = 4)
  
  q <- cities_df %>% filter(counts == "n") %>%
    ggplot(aes(x = as.factor(YEAR), y = number)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "Number") +
    scale_x_discrete(breaks = seq(2006, 2016, by=1), limits = factor(2006:2016), 
                     drop = FALSE) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 6),
          axis.text.y = element_text(size = 5),
          strip.text.x = element_text(size = 7)) +
    facet_wrap( ~ .data$CITY, nrow = 6, ncol = 4, scales = "free")
  
  ggsave("figures/Figure_S5.pdf", p, height = 183, width = 183, units = "mm", dpi = 300)
  ggsave("figures/Figure_S6.pdf", q, height = 183, width = 183, units = "mm", dpi = 300)
  
}
  
plot_figS5_S6()
