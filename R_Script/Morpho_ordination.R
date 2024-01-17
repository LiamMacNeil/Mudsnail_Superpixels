remotes::install_github("fawda123/ggord")
remotes::install_github("kassambara/factoextra")
library(tidyverse)
library(ggplot2)
#remotes::install_github("juliasilge/tidytext")
library(ggpubr)
library(vegan)
library(ggord)
library(factoextra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

features_w <- read_csv("../Datasets/Features_cleaned.csv")

features_w$Treatment <- factor(features_w$Treatment, levels = c("three", "1p5", "amb"))
features_w$Tank <- factor(features_w$Tank, levels = sort(unique(features_w$Tank)))

features_ord <- features_w[,c(-15,-9)]

features_ord[,c(1:8)] <- scale(features_ord[,c(1:8)])

cols <- RColorBrewer::brewer.pal(4, "Spectral")[3:1]

#Calculate Circularity
features_ord$roundness <- (4*(features_ord$s.area/83))/(pi*(features_ord$size..mm.^2))
hist(features_ord$roundness)

#Calculate Elongation
features_ord$elongation <- ((features_ord$s.radius.min*2)/83)/((features_ord$s.radius.max*2)/83)
hist(features_ord$elongation)

#Calculate Complexity
features_ord$complexity <- ((features_ord$s.perimeter)/83)/(features_ord$size..mm.)
hist(features_ord$complexity)

###################
#Ratos
###################
features_ord %>% 
  group_by(Month) %>% 
  summarise(complex = median(complexity),
            Area = median(s.area),
            Perimeter = median(s.perimeter),
            Size = median(size..mm.),
            roundness = median(roundness),
            elongation = median(elongation))

# Subset
features_ord_set <- features_ord[,c("s.area", "s.perimeter", 
                                    "size..mm.", "roundness",
                                    "elongation", "complexity")]

#Standardize descriptors
features_ord_set <- scale(features_ord_set)
colnames(features_ord_set) <- c("Area", "Perimeter",
                                "Size", "Roundness",
                                "Elongation", "Complexity")
###############
#PCA
###############

pairs(features_ord_set)

ord <- prcomp(features_ord_set)

fviz_screeplot(ord, ncp = 10)


ggord(ord, features_ord$Treatment, alpha = 0.55, ellipse = F, grp_title = "", legend = F) +
  scale_color_manual(values = rev(cols),
                    labels = c("+ 3 \u00B0C", "+ 1.5 \u00B0C", "Ambient"),
                    name = "Treatment")+
  guides(color = guide_legend(override.aes = list(alpha = 1)))

