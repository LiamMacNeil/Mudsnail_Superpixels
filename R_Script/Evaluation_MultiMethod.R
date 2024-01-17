library(tidyverse)
library(stringr)
library(oce)
library(fs)
library(ggridges)
library(overlapping)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

manual_w <- read_csv("../Datasets/Hydrobia_Manual_Corrected_data.csv")

# Features
features <- read_csv("../Datasets/Features_cleaned.csv")
  
features_binarized <- list.files(path = "../Datasets/Hydrobia_full/Outputs/Features_binary/",
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  reduce(full_join)  %>% 
  separate_wider_delim(Sample, delim = "_", names = c(
                                                          "Species", 
                                                          "Sample", 
                                                          "Treatment", 
                                                          "Event", 
                                                          "Tank", 
                                                          "Image"), 
                           too_few  = "align_end", too_many = "merge") %>% 
  separate_wider_delim(Image,
                       delim = ".", 
                       names = c("Number","Extension"),
                       too_few  = "align_end", 
                       too_many = "merge")

features_edge<- list.files(path = "../Datasets/Hydrobia_full/Outputs/Features_edge/",
                                 pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  reduce(full_join)  %>% 
  separate_wider_delim(Sample, delim = "_", names = c(
    "Species", 
    "Sample", 
    "Treatment", 
    "Event", 
    "Tank", 
    "Image"), 
    too_few  = "align_end", too_many = "merge") %>% 
  separate_wider_delim(Image,
                       delim = ".", 
                       names = c("Number","Extension"),
                       too_few  = "align_end", 
                       too_many = "merge")

# Distinguish
manual_w$Source <- "Manual"
features$Source <- "Region-based"
features_binarized$Source <- "Threshold-based"
features_edge$Source <- "Edge-based"

# Join
#manual_features <- full_join(manual_w, features)
manual_samples <- unique(manual_w$Sample)

features_watershed <- features %>% 
  filter(Sample %in% manual_samples)

#SF131
manual_SF131 <- manual_w %>% 
  filter(Sample %in% unique(manual_w$Sample)[1])
features_watershed_SF131 <- features %>% 
  filter(Sample %in% unique(manual_w$Sample)[1])
features_binarized_SF131 <- features_binarized %>% 
  filter(Sample %in% unique(manual_w$Sample)[1])
features_edge_SF131 <- features_edge %>% 
  filter(Sample %in% unique(manual_w$Sample)[1])

#SF114
manual_SF114 <- manual_w %>% 
  filter(Sample %in% unique(manual_w$Sample)[2])
features_watershed_SF114 <- features %>% 
  filter(Sample %in% unique(manual_w$Sample)[2])
features_binarized_SF114 <- features_binarized %>% 
  filter(Sample %in% unique(manual_w$Sample)[2])
features_edge_SF114 <- features_edge %>% 
  filter(Sample %in% unique(manual_w$Sample)[2])
# Correct another mistake during data entry
manual_SF114$Treatment <- unique(features_watershed_SF114$Treatment)

#SF43
manual_SF43 <- manual_w %>% 
  filter(Sample %in% unique(manual_w$Sample)[3])
features_watershed_SF43 <- features %>% 
  filter(Sample %in% unique(manual_w$Sample)[3])
features_binarized_SF43 <- features_binarized %>% 
  filter(Sample %in% unique(manual_w$Sample)[3])
features_edge_SF43 <- features_edge %>% 
  filter(Sample %in% unique(manual_w$Sample)[3])

#SF84
manual_SF84 <- manual_w %>% 
  filter(Sample %in% unique(manual_w$Sample)[4])
features_watershed_SF84 <- features %>% 
  filter(Sample %in% unique(manual_w$Sample)[4])
features_binarized_SF84 <- features_binarized %>% 
  filter(Sample %in% unique(manual_w$Sample)[4])
features_edge_SF84 <- features_edge %>% 
  filter(Sample %in% unique(manual_w$Sample)[4])

###
# Sample-wise overlap
###
cols <- c("#E69F00",  "#0072B2", "#117733",  "#332288")

#RGB codes
# threshold : 4095c5ff 
# edge : 4d9966ff
# region : 6659a6ff

#SF43
SF43 <- list(X1 = manual_SF43$size..mm., 
             X2 = features_binarized_SF43$size..mm.,
             X3 = features_edge_SF43$size..mm.,
             X4 = features_watershed_SF43$size..mm.) 

SF43_overlap <- overlap(SF43)$OVPairs[1:3]

SF43_p <- final.plot(SF43) +
  #geom_density(alpha = 0.4) + 
  scale_fill_manual(values = alpha(cols, 0.8),
                    labels = c("Manual",
                               "Threshold-based", 
                               "Edge-based",
                               "Region-based"),
                    name = "Method")+
  scale_color_manual(values = cols)+
  theme_bw(22) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())+
  labs( x = "Length (mm)", y = "Density")+
  guides(color = "none",
         fill = guide_legend(override.aes = list(alpha = 0.75)))

ggsave("../Figures/SF43_multimethod.png", SF43_p,width = 18, height = 14, dpi = 600, units = "cm")

#SF84
SF84 <- list(X1 = manual_SF84$size..mm., 
             X2 = features_binarized_SF84$size..mm.,
             X3 = features_edge_SF84$size..mm.,
             X4 = features_watershed_SF84$size..mm.) 

SF84_overlap <- overlap(SF84)$OVPairs[1:3]

SF84_p <- final.plot(SF84) +
  #geom_density(alpha = 0.4) + 
  scale_fill_manual(values = alpha(cols, 0.8),
                    labels = c("Manual",
                               "Threshold-based", 
                               "Edge-based",
                               "Region-based"),
                    name = "Method")+
  scale_color_manual(values = cols)+
  theme_bw(22) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())+
  labs( x = "Length (mm)", y = "Density")+
  guides(color = "none",
         fill = guide_legend(override.aes = list(alpha = 0.75)))
ggsave("../Figures/SF84_multimethod.png", SF84_p,width = 18, height = 14, dpi = 600, units = "cm")


#SF114
SF114 <- list(X1 = manual_SF114$size..mm., 
             X2 = features_binarized_SF114$size..mm.,
             X3 = features_edge_SF114$size..mm.,
             X4 = features_watershed_SF114$size..mm.) 

SF114_overlap <- overlap(SF114)$OVPairs[1:3]

SF114_p <- final.plot(SF114) +
  #geom_density(alpha = 0.4) + 
  scale_fill_manual(values = alpha(cols, 0.8),
                    labels = c("Manual",
                               "Threshold-based", 
                               "Edge-based",
                               "Region-based"),
                    name = "Method")+
  scale_color_manual(values = cols)+
  theme_bw(22) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())+
  labs( x = "Length (mm)", y = "Density")+
  guides(color = "none",
         fill = guide_legend(override.aes = list(alpha = 0.75)))
ggsave("../Figures/SF114_multimethod.png", SF114_p,width = 18, height = 14, dpi = 600, units = "cm")

#SF131
SF131 <- list(X1 = manual_SF131$size..mm., 
              X2 = features_binarized_SF131$size..mm.,
              X3 = features_edge_SF131$size..mm.,
              X4 = features_watershed_SF131$size..mm.) 

SF131_overlap <- overlap(SF131)$OVPairs[1:3]

SF131_p <- final.plot(SF131) +
  #geom_density(alpha = 0.4) + 
  scale_fill_manual(values = alpha(cols, 0.8),
                    labels = c("Manual",
                               "Threshold-based", 
                               "Edge-based",
                               "Region-based"),
                    name = "Method")+
  scale_color_manual(values = cols)+
  theme_bw(22) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())+
  labs( x = "Length (mm)", y = "Density")+
  guides(color = "none",
         fill = guide_legend(override.aes = list(alpha = 0.75)))
ggsave("../Figures/SF131_multimethod.png", SF131_p,width = 18, height = 14, dpi = 600, units = "cm")



# Average overlap?
pivot_longer(cbind(as.data.frame(SF43_overlap), 
      as.data.frame(SF84_overlap),
      as.data.frame(SF114_overlap),
      as.data.frame(SF131_overlap)), cols = everything()) %>% 
  summarise(mean = mean(value))
