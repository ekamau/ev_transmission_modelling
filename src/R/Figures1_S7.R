plot_fig1_figS7 <- function() {
  d <- read.csv("data/virologic_data.csv", header = TRUE, sep = ',')
  serotype_df = d %>% count(YEAR,ENGENG) 
  total_df = d %>% count(YEAR) 
  serotypes_v1 = c("#ca6", "#ent71")
  serotypes_names_v = c("CVA6", "EV-A71")
  
  ### EV-A71:
  e71 = serotype_df[serotype_df$ENGENG == '#ent71',]
  ev71df <- cbind(total_df, e71$n)
  colnames(ev71df) <- c("year", "total", "e71_N")
  ev71df$other <- ev71df$total - ev71df$e71_N
  ev71df$propE71 <- ev71df$e71_N / ev71df$total
  ev71df$propOther <- 1 - ev71df$propE71
  ev71dfB <- melt(ev71df, id.vars = c("year"), measure.vars = c("other", "e71_N"), 
                  variable.name = "serotype", value.name = "count")
  
  A <- ggplot() +
    geom_bar(data = ev71df, aes(x=year, y = e71_N), fill = "#1B9E77", stat = 'identity', alpha = 0.8) +
    labs(x = "", y = "Number", title = '(A)') +
    scale_y_continuous(limits = c(0, 200)) +
    scale_x_continuous(breaks = seq(from = 2016, to = 2006, by = -1)) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 9, angle = 90),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'))
  
  C <- ggplot(ev71df, aes(x = year, y = propE71)) +
    geom_bar(stat="identity", width=0.8, fill = "#1B9E77", alpha = 0.8) +
    labs(x = "", y = "Proportion", title = "(C)") +
    scale_x_continuous(breaks=seq(2006,2016),limits = c(2005.5,2016.5)) +
    scale_y_continuous(limits=c(0,0.3)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 9),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'))
  
  ### CVA6:
  ca6 = serotype_df[serotype_df$ENGENG == '#ca6',]
  ca6df <- cbind(total_df, ca6$n)
  colnames(ca6df) <- c("year", "total", "ca6_N")
  ca6df$other <- ca6df$total - ca6df$ca6_N
  ca6df$propCA6 <- ca6df$ca6_N / ca6df$total
  ca6df$propOther <- 1 - ca6df$propCA6
  ca6dfB <- melt(ca6df, id.vars = c("year"), measure.vars = c("other","ca6_N"), 
                 variable.name = "serotype", value.name = "count")
  
  B <- ggplot() +
    geom_bar(data = ca6df, aes(x=year, y = ca6_N), fill = "#D95F02", stat = 'identity', alpha = 0.8) +
    labs(x = "", y = "", title = '(B)') +
    scale_x_continuous(breaks = seq(from = 2016, to = 2006, by = -1)) +
    scale_y_continuous(limits = c(0, 200)) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 9, angle = 90),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'))
  
  D <- ggplot(ca6df, aes(x = year, y = propCA6)) +
    geom_bar(stat = "identity", width = 0.8, fill = "#D95F02", alpha = 0.8) +
    labs(x = "", y = "", title = "(D)") +
    scale_x_continuous(breaks=seq(2006, 2016),limits = c(2005.5, 2016.5)) +
    scale_y_continuous(limits=c(0,0.3)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 9),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'))
  
  # Sample types:
  sample_l <- readRDS(file = "data/sample_type_list.RData")
  unknown_sample_df <- d[d$MAT %in% sample_l[["Unknown"]],] # c("Unknown","Not known","Other","")
  unknown_sample_df <- unknown_sample_df %>% count(YEAR) 
  total_sample_df <- d %>% count(YEAR) 
  total_sample_df$sample_unknown <- unknown_sample_df$n
  total_sample_df$sample_known <- total_sample_df$n - total_sample_df$sample_unknown
  total_sample_df$prop_known <- total_sample_df$sample_known / total_sample_df$n
  total_sample_df = total_sample_df[,!(names(total_sample_df) %in% c("n"))]
  total_sample_df_long <- melt(total_sample_df, id.vars = c("YEAR"), 
                               variable.name = "Sample_type")
  total_sample_df_long = total_sample_df_long[total_sample_df_long$Sample_type != "prop_known", ]
  total_sample_df_long$Sample_type = factor(total_sample_df_long$Sample_type, levels = c("sample_known", "sample_unknown"), labels = c("Known","Unknown"))
  
  E <- ggplot(total_sample_df_long, aes(fill = Sample_type, y = value, x = as.factor(YEAR))) + 
    geom_bar(stat = "identity") +
    labs(x = "", y = "Number", fill = "Sample type", title = "(E)") +
    scale_fill_manual(labels = c("Known", "Unknown"), values = c("#1F78B4", "gray66")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 9),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          plot.title = element_text(size = 14, face = 'bold'),
          legend.position = "bottom")
  
  G <- ggplot(data = total_sample_df_long, aes(x = YEAR, y = value, fill = Sample_type)) +
    geom_bar(stat = "identity", width = 0.8, position = position_fill(reverse = TRUE)) +
    labs(x = "", y = "Proportion", title = "(A)", fill = "Sample type") +
    scale_x_continuous(breaks = seq(2006, 2016),limits = c(2005.5, 2016.5)) +
    scale_fill_manual(values = c("#1F78B4", "gray66")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 9),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'),
          legend.text = element_text(size = 10),
          legend.position = "bottom")
  
  # Symptoms:
  symptoms_l <- readRDS(file = "data/symptoms_list.RData")
  d$QEWHAT = tolower(d$QEWHAT)
  known_symptoms_v = c()
  for(i in 1:length(symptoms_l)){
    known_symptoms_v = c(known_symptoms_v, symptoms_l[[i]])
  } 
  
  y = d %>% filter(grepl(paste(known_symptoms_v,collapse = "|"), QEWHAT)) %>% count(YEAR) 
  unknown_symptoms_df = filter(d, grepl("#inp",QEWHAT) | QEWHAT=="") 
  unknown_symptoms_df = unknown_symptoms_df %>% count(YEAR) 
  total_symptoms_df = d %>% count(YEAR) 
  total_symptoms_df$sympt_unknown = unknown_symptoms_df$n
  total_symptoms_df$sympt_known = total_symptoms_df$n - total_symptoms_df$sympt_unknown
  total_symptoms_df = total_symptoms_df[,!(names(total_symptoms_df) %in% c("n"))]
  total_symptoms_df_long = melt(total_symptoms_df, id.vars = c("YEAR"), variable.name = "Symptoms")
  total_symptoms_df_long$Symptoms = factor(total_symptoms_df_long$Symptoms, levels = c("sympt_known", "sympt_unknown"), labels = c("Known","Unknown"))
  
  F <- ggplot(total_symptoms_df_long, aes(x = as.factor(YEAR), y = value, fill = Symptoms)) + 
    geom_bar(stat = "identity", alpha = 0.7) +
    labs(x = "", y = "", fill = "Symptoms", title = "(F)") +
    scale_fill_manual(labels = c("Known", "Unknown"), values = c("#E31A1C", "gray66")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 9),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          plot.title = element_text(size = 14, face = 'bold'),
          legend.position = "bottom")
  
  H <- ggplot(total_symptoms_df_long, aes(x = YEAR, y = value, fill = Symptoms)) +
    geom_bar(stat = "identity", width = 0.8, position = position_fill(reverse = TRUE), alpha = 0.7) +
    labs(x = "", y = "", title = "(B)") +
    scale_x_continuous(breaks=seq(2006, 2016), limits = c(2005.5, 2016.5)) +
    scale_fill_manual(values = c("#E31A1C", "gray66")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 9),
          axis.text.y = element_text(size = 9),
          axis.title = element_text(size = 11),
          plot.title = element_text(size = 14, face = 'bold'),
          legend.text = element_text(size = 10),
          legend.position = "bottom")
  
  fig1 <- (A | B) / (C | D) 
  ggsave(filename = 'figures/Figure1.pdf', fig1, height = 183, width = 183, units = 'mm', dpi = 300)
  
  figS7 <- (E | F) / (G | H)
  ggsave(filename = 'figures/FigureS7.pdf', figS7, height = 183, width = 183, units = 'mm', dpi = 300)
  
}
