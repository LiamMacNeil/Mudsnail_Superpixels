library(tidyverse)
library(stringr)
library(oce)
#remotes::install_github("juliasilge/tidytext")
library(fs)
library(ggridges)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#files <- (list.files("../Datasets/Hydrobia_full/Outputs/Features_all/", pattern = "\\.csv$", full.names = T))
'''
features <- list.files(path = "../Datasets/Hydrobia_full/Outputs/Features_all/",
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  reduce(full_join)  


features_w <- features %>% separate_wider_delim(Sample, delim = "_", names = c("a",
                                                                                     "Species", 
                                                                                     "Sample", 
                                                                                     "Treatment", 
                                                                                     "Event", 
                                                                                     "Tank", 
                                                                                     "Image",
                                                                                     "y"), 
                                                      too_few  = "align_end", too_many = "merge")

features_w <- features_w %>% separate_wider_delim(y, delim = ".", names = c("Number",
                                                                                 "Extension"),
                                                too_few  = "align_end", too_many = "merge")
features_w <- features_w[,-c(1, 10:11)]


colnames(features_w) <- c(colnames(features_w)[1:8], "Species", "Sample" ,"Treatment" ,"Event" ,
                             "Tank", "Image" ,"Extension")

features_w$Event <- as.numeric(features_w$Event) 
features_w$Image <- as.numeric(features_w$Image) 

features_w <- features_w %>% 
  mutate(Date = case_when(
    Event == 0 ~ "03-30-2022",
    Event == 1 ~ "04-25-2022",
    Event == 2 ~ "05-24-2022",
    Event == 3 ~ "06-20-2022"))

features_w$Date <- mdy(features_w$Date)
features_w$Month <- factor(months(features_w$Date), levels = c("March", "April", "May", "June"))
features_w$Treatment <- factor(features_w$Treatment, levels = c("three", "1p5","amb"))

write_csv(features_w, "../Datasets/Features_cleaned.csv")
'''
features_w <- read_csv("../Datasets/Features_cleaned.csv")

cols <- RColorBrewer::brewer.pal(4, "Spectral")[3:1]

features_w$Treatment <- factor(features_w$Treatment, levels = c("three", "1p5", "amb"))

growth <- features_w %>% group_by(Date, Treatment) %>% summarise(n = (size..mm.)) %>% 
  ggplot(aes(x = Date, y = n), group = Treatment)+
  stat_summary(aes(color = Treatment),fun.data = "mean_cl_boot") +
  scale_color_manual(values = rev(cols),
                    labels = c("+ 3 \u00B0C", "+ 1.5 \u00B0C", "Ambient"),
                    name = "Treatment")+
  theme_bw(18)+
  #facet_wrap(~Treatment)+
  stat_smooth(aes(color = Treatment), method="glm", se = T)+
  stat_cor(aes(color = Treatment, label = ..r.label..),
           method = "pearson", 
           size=5,  r.accuracy = 0.01, label.y = c(3.8,3.82,3.84))+
  labs(x = "Month", y = "Size (mm)")
ggsave("../Figures/Growth_statsummary.png", width = 15, height = 12, dpi = 300, units = "cm")

tank_growth <- features_w %>% group_by(Date, Tank, Treatment) %>% summarise(n = (size..mm.)) %>% 
  ggplot(aes(x = Date, y = n), group = Treatment)+
  stat_summary(aes(color = Treatment),fun.data = "mean_cl_boot") +
  facet_wrap(~ Tank)+
  scale_fill_manual(values = cols,
                    labels = c("Ambient","+ 1.5 \u00B0C", "+ 3 \u00B0C"),
                    name = "Treatment")+
  theme_bw(18)+
  stat_smooth(aes(color = Treatment), method=lm)+
  stat_cor(aes(color = Treatment, label = ..r.label..),
           method = "pearson", 
           size=4,  r.accuracy = 0.01, label.y = 4)+
  labs(x = "Month", y = "Size (mm)")
ggsave("../Figures/Growth_statsummary.png", width = 15, height = 12, dpi = 300, units = "cm")


ridge_bulk <- ggplot(features_w, aes(x = size..mm., y = Month, group = Month ,fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles")+
  theme_bw(18) +
  labs( x = "Length (mm)")+
  theme(legend.position = "none")
ggsave("../Figures/Ridgeplot_AllSamples.png", width = 15, height = 14, dpi = 300, units = "cm")

ridge_tank <- ggplot(features_w, aes(x = size..mm., y = Month, group = Month)) +
  stat_density_ridges(aes(fill = Treatment),
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  facet_wrap(~ Tank)+
  scale_fill_manual(values = cols,
                    labels = c("Ambient","+ 1.5 \u00B0C", "+ 3 \u00B0C"),
                    name = "Treatment")+
  theme_bw(18) +
  labs( x = "Length (mm)")
ggsave("../Figures/Ridgeplot_Tank.png", width = 20, height = 16, dpi = 300, units = "cm")

  
features_w$Date <- factor(features_w$Date, ordered = T)

ridge_treatment <- ggplot(features_w, aes(x = size..mm., y = Date)) +
  #stat_density_ridges(aes(fill = Treatment),
  #                    geom = "density_ridges_gradient", calc_ecdf = TRUE,
  #                    quantiles = 4, quantile_lines = TRUE
  #) +
  geom_density_ridges(aes(fill = Treatment, color = Treatment), alpha = 0.5, quantile_lines =T)+
  scale_fill_manual(values = rev(cols),
                    labels = c("+ 3 \u00B0C", "+ 1.5 \u00B0C", "Ambient"),
                    name = "Treatment")+
  scale_color_manual(values = rev(cols))+
  theme_bw(18) +
  theme(axis.text.y = element_blank())+
  labs( x = "Length (mm)")+
  guides(color = "none")  
ggsave("../Figures/Ridgeplot_Treatment.png", width = 15, height = 14, dpi = 300, units = "cm")



features_w