library(tidyverse)
library(stringr)
library(oce)
#remotes::install_github("juliasilge/tidytext")
library(fs)
library(ggridges)
library(overlapping)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Features
'''
features <- list.files(path = "../Datasets/600Spx_test/",
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

write_csv(features_w, "../Datasets/600spx_Features_cleaned.csv")
'''

manual_w <- read_csv("../Datasets/Hydrobia_Manual_Corrected_data.csv")
features <- read_csv("../Datasets/600spx_Features_cleaned.csv")

features <- read_delim("../Datasets/Hydrobia_methods_auto1.txt")

features$size..mm. <- (gsub(",", ".", features$size..mm.))
features$size..mm. <- as.numeric(features$size..mm.)
# Distinguish

manual_w$Source <- "Manual"
features$Source <- "Automatic"


manual_samples <- unique(manual_w$Sample)

features_eval <- features %>% 
  filter(Sample %in% manual_samples)

#################
# Overlapping empirical distributions
#################

Lists <- list(X1 = features_eval$size..mm., X2 = manual_w$size..mm.) 
overlap(Lists)$OV * 100

overlap(Lists, plot = T)

###############

#SF131
manual_SF131 <- manual_w %>% 
  filter(Sample %in% unique(manual_w$Sample)[1])
features_eval_SF131 <- features %>% 
  filter(Sample %in% unique(manual_w$Sample)[1])

#SF114
manual_SF114 <- manual_w %>% 
  filter(Sample %in% unique(manual_w$Sample)[2])
features_eval_SF114 <- features %>% 
  filter(Sample %in% unique(manual_w$Sample)[2])
# Correct another mistake during data entry
manual_SF114$Treatment <- unique(features_eval_SF114$Treatment)

#SF43
manual_SF43 <- manual_w %>% 
  filter(Sample %in% unique(manual_w$Sample)[3])
features_eval_SF43 <- features %>% 
  filter(Sample %in% unique(manual_w$Sample)[3])

#SF84
manual_SF84 <- manual_w %>% 
  filter(Sample %in% unique(manual_w$Sample)[4])
features_eval_SF84 <- features %>% 
  filter(Sample %in% unique(manual_w$Sample)[4])

cols <- RColorBrewer::brewer.pal(5, "Dark2")[1:6]

cols <- c("#0072B2", "#009E73")
cols <- c("#332288", "#117733")

#SF43
SF43 <- list(X1 = manual_SF43$size..mm., X2 = features_eval_SF43$size..mm.) 
overlap(SF43)$OV * 100

SF43_p <- final.plot(SF43) +
  scale_fill_manual(values = cols,
                    labels = c("Manual",
                               "Automatic"),
                    name = "Groups")+
  scale_color_manual(values = cols)+
  theme_bw(22) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())+
  labs( x = "Length (mm)", y = "Density")+
  guides(color = "none")
ggsave("../Figures/SF43_600spx_compare.png", width = 18, height = 14, dpi = 300, units = "cm")

#SF84
SF84 <- list(X1 = features_eval_SF84$size..mm., X2 = manual_SF84$size..mm.) 
overlap(SF84)$OV * 100

SF84_p <- final.plot(SF84) +
  scale_fill_manual(values =cols,
                    labels = c("Manual",
                               "Automatic"),
                    name = "Groups")+
  scale_color_manual(values = cols)+
  theme_bw(22) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())+
  labs( x = "Length (mm)", y = "Density")+
  guides(color = "none")
ggsave("../Figures/SF84_600spx_compare.png", width = 18, height = 14, dpi = 300, units = "cm")


#SF114
SF114 <- list(X1 = features_eval_SF114$size..mm., X2 = manual_SF114$size..mm.) 
overlap(SF114)$OV * 100
#overlap(SF114, plot = T)

SF114_p <- final.plot(SF114) +
  scale_fill_manual(values =cols,
                    labels = c("Manual",
                               "Automatic"),
                    name = "Groups")+
  scale_color_manual(values = cols)+
  theme_bw(22) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())+
  labs( x = "Length (mm)", y = "Density")+
  guides(color = "none")
ggsave("../Figures/SF114_600spx_compare.png", width = 18, height = 14, dpi = 300, units = "cm")


#SF131
SF131 <- list(X1 = features_eval_SF131$size..mm., X2 = manual_SF131$size..mm.) 
overlap(SF131)$OV * 100
overlap(SF131, plot = T)

SF131_p <- final.plot(SF131) +
  scale_fill_manual(values =cols,
                    labels = c("Manual",
                               "Automatic"),
                    name = "Groups")+
  scale_color_manual(values = cols)+
  theme_bw(22) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())+
  labs( x = "Length (mm)", y = "Density")+
  guides(color = "none")
ggsave("../Figures/SF131_1500spx_compare.png", width = 18, height = 14, dpi = 300, units = "cm")
