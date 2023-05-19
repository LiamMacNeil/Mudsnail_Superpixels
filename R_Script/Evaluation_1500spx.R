library(tidyverse)
library(stringr)
library(oce)
#remotes::install_github("juliasilge/tidytext")
library(tidytext)
library(fs)
library(ggridges)
library(overlapping)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#files <- (list.files("../Datasets/Hydrobia_full/Outputs/Features_all/", pattern = "\\.csv$", full.names = T))
'''
manual <- read_csv("../Datasets/Hydrobia_Manual_data.csv")

# Cleaning to correct entry inconsistancies
wrong <- c("SF_114", "SF_43", "SF_84")

manual$Photo <- (gsub("SF_114", "SF114", manual$Photo))
manual$Photo <- (gsub("SF_43", "SF43", manual$Photo))
manual$Photo <- (gsub("SF_84", "SF84", manual$Photo))

manual$Photo <- (gsub("SF114_Hydrobia", "Hydrobia_SF114", manual$Photo))
manual$Photo <- (gsub("SF43_Hydrobia", "Hydrobia_SF43", manual$Photo))
manual$Photo <- (gsub("SF84_Hydrobia", "Hydrobia_SF84", manual$Photo))

# Splitting and formatting to match feature df
manual_w <- manual %>% separate_wider_delim(Photo, delim = "_", names = c(
                                                                               "Spec", 
                                                                               "Samp", 
                                                                               "Image"), 
                                                too_few  = "align_end", too_many = "merge")

manual_w <- manual_w[,-c(1,2, 9, 10)]
colnames(manual_w) <- c("Species", "Sample", "Image", "Event", "Tank", "Treatment", "size..mm.")

# Fixing data types to match features
manual_w$Event <- as.numeric(manual_w$Event) 
manual_w$Image <- as.numeric(manual_w$Image) 

manual_w <- manual_w %>% 
  mutate(Date = case_when(
    Event == 0 ~ "03-30-2022",
    Event == 1 ~ "04-25-2022",
    Event == 2 ~ "05-24-2022"))

manual_w$Date <- mdy(manual_w$Date)
manual_w$Month <- factor(months(manual_w$Date), levels = c("March", "April", "May"))

manual_w$Treatment <- (gsub("Ambient", "amb", manual_w$Treatment))
manual_w$Treatment <- (gsub("3+", "three", manual_w$Treatment))
manual_w$Treatment <- factor(manual_w$Treatment, levels = c("amb", "three"))

#Save
write_csv(manual_w, "../Datasets/Hydrobia_Manual_Corrected_data.csv")
'''

manual_w <- read_csv("../Datasets/Hydrobia_Manual_Corrected_data.csv")

# Features
features <- read_csv("../Datasets/Features_cleaned.csv")

# Distinguish
manual_w$Source <- "Manual"
features$Source <- "Automatic"

# Join
#manual_features <- full_join(manual_w, features)
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

###
# Sample-wise overlap
###
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
ggsave("../Figures/SF43_compare.png", width = 18, height = 14, dpi = 300, units = "cm")

#SF84
SF84 <- list(X1 = features_eval_SF84$size..mm., X2 = manual_SF84$size..mm.) 
overlap(SF84)$OV * 100
overlap(SF84, plot = T)

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
ggsave("../Figures/SF84_compare.png", width = 18, height = 14, dpi = 300, units = "cm")


#SF114
SF114 <- list(X1 = features_eval_SF114$size..mm., X2 = manual_SF114$size..mm.) 
overlap(SF114)$OV * 100
overlap(SF114, plot = T)

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
ggsave("../Figures/SF114_compare.png", width = 18, height = 14, dpi = 300, units = "cm")


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
ggsave("../Figures/SF131_600spx_compare.png", width = 18, height = 14, dpi = 300, units = "cm")
