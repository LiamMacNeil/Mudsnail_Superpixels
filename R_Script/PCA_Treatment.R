#remotes::install_github("fawda123/ggord")
#remotes::install_github("kassambara/factoextra")
library(tidyverse)
library(ggplot2)
#remotes::install_github("juliasilge/tidytext")
library(ggpubr)
library(vegan)
library(ggord)
library(factoextra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

features_w <- read_csv("../Datasets/Features_cleaned.csv")

features_w$Treatment <- factor(features_w$Treatment, levels = c("amb", "three", "1p5"))
features_w$Tank <- factor(features_w$Tank, levels = sort(unique(features_w$Tank)))
features_w$Event_factor <- factor(features_w$Event, levels = sort(unique(features_w$Event)))
features_w$Month_factor <- factor(features_w$Month, levels = c("March", "April", "May", "June"))

features_ord <- features_w[,c(-15,-9)]

#features_ord[,c(1:8)] <- scale(features_ord[,c(1:8)])

cols <- RColorBrewer::brewer.pal(4, "Spectral")[3:1]
month_cols <- RColorBrewer::brewer.pal(4, "BrBG")[4:1]

#Calculate Circularity
features_ord$roundness <- (4*(features_ord$s.area/83))/(pi*(features_ord$size..mm.^2))
hist(features_ord$roundness)

#Calculate Elongation
features_ord$elongation <- ((features_ord$s.radius.min*2)/83)/((features_ord$s.radius.max*2)/83)
hist(features_ord$elongation)

#Calculate Complexity
features_ord$complexity <- ((features_ord$s.perimeter)/83)/(features_ord$size..mm.)
hist(features_ord$complexity)

features_ord_set <- features_ord[,c("s.area", "s.perimeter", 
                                    "size..mm.", "roundness",
                                    "elongation", "complexity")]

#Standardize descriptors
features_ord_set <- scale(features_ord_set)
colnames(features_ord_set) <- c("Area", "Perimeter",
                                "Size", "Roundness",
                                "Elongation", "Complexity")


#Values per treatment group
calcs <- features_ord %>% 
  group_by(Event, Treatment) %>% 
  summarise(mean(s.area), mean(s.perimeter), mean(size..mm.),
            mean(roundness), mean(elongation), mean(complexity)) %>% 
  group_by(Event, Treatment) 

# Calculate ratio of descriptors per treatment between first and last event
calcs %>% 
  group_by(Treatment) %>% 
  summarize(area = `mean(s.area)`[Event == 3]/`mean(s.area)`[Event == 0],
            perim = `mean(s.perimeter)`[Event == 3]/`mean(s.perimeter)`[Event == 0],
            Length = `mean(size..mm.)`[Event == 3]/`mean(size..mm.)`[Event == 0],
            Roundness = `mean(roundness)`[Event == 3]/`mean(roundness)`[Event == 0],
            Elongation = `mean(elongation)`[Event == 3]/`mean(elongation)`[Event == 0],
            Complexity = `mean(complexity)`[Event == 3]/`mean(complexity)`[Event == 0]
            )  

  
###############
#PCA
###############

pairs(features_ord_set)

ord <- prcomp(features_ord_set)
#ord <- rda(features_ord_set)

#ev <- ord$CA$eig
#n <- length(ev)
#barplot(ev, main = "", col = "grey", las = 2)
#abline(h = mean(ev), col = "red3", lwd = 2)
#legend("topright", "Average eigenvalue", lwd = 2, col = "red3",
#       bty = "n")

fviz_screeplot(ord, ncp = 5)+
  ggtitle("")+
  theme_bw(18)+
  ylab("Variance Explained (%)")
ggsave("../Figures/PCA_Screeplot.png", width = 12, height = 12, dpi = 300, units = "cm")

summary(ord)


ggord(ord, axes=c(2,3) , 
      features_ord$Treatment, 
      alpha = 0.2, 
      ellipse = T, 
      alpha_el = 0.4,
      grp_title = "T", 
      legend = F, 
      size = 3, 
      txt = 3,
      veclsz = 0.25,
      arrow = 0.25,
      vectyp = "solid",
      ext = 2.5,
      polylntyp = "dash",
      xlim = c(-8, 8),
      ylim = c(-6,8)) +
  scale_fill_manual(values = (cols),
                    labels = c("Ambient", "+ 1.5 \u00B0C","+ 3 \u00B0C"),
                    name = "Treatment")+
  scale_color_manual(values = (cols),
                     labels = c("", "", ""),
                     name = "")+
  guides(color = guide_legend(override.aes = list(alpha = 0, size = 0)),
         fill = guide_legend(override.aes = list(alpha = 1, size = 5)))+
  theme_bw(18)
ggsave("../Figures/PCA_Treatment_Axes2_3.png", width = 18, height = 16, dpi = 300, units = "cm")

ggord(ord, axes=c(2,3) , 
      features_ord$Month_factor, 
      alpha = 0.2, 
      ellipse = T, 
      alpha_el = 0.4,
      grp_title = "Month", 
      legend = F, 
      size = 3, 
      txt = 3,
      veclsz = 0.25,
      arrow = 0.25,
      vectyp = "solid",
      ext = 2.5,
      polylntyp = "dash",
      xlim = c(-8, 8),
      ylim = c(-6,8)) +
  scale_fill_manual(values = rev(month_cols),
                     labels = c("March", "April", "May", "June"),
                     name = "Month")+
  scale_color_manual(values = (month_cols),
                    labels = c("", "", "", ""),
                    name = "")+
  guides(color = guide_legend(override.aes = list(alpha = 0, size = 0)),
         fill = guide_legend(override.aes = list(alpha = 1, size = 5)))+
  theme_bw(18)
ggsave("../Figures/PCA_Month_axes2_3.png", width = 18, height = 16, dpi = 300, units = "cm")

