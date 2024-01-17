library(tidyverse)
library(stringr)
library(oce)
#remotes::install_github("juliasilge/tidytext")
library(see)
library(lme4)
library(glmmTMB)
library(ggpubr)
library(equatiomatic)
library(performance)
library(ggeffects)
library(car)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

features_w <- read_csv("../Datasets/Features_cleaned.csv")

cols <- RColorBrewer::brewer.pal(4, "Spectral")[3:1]

features_w$Treatment <- factor(features_w$Treatment, levels = c("three", "1p5", "amb"))
features_w$Tank <- factor(features_w$Tank, levels = sort(unique(features_w$Tank)))
features_w$Event_factor <- factor(features_w$Event, levels = c("0", "1", "2", "3"))
features_w$Month_factor <- factor(features_w$Month, levels = c("March", "April", "May", "June"))

growth <- features_w %>% group_by(Date, Treatment) %>% summarise(n = (size..mm.)) %>% 
  ggplot(aes(x = Date, y = n), group = Treatment)+
  stat_summary(aes(color = Treatment),fun.data = "mean_cl_boot") +
  scale_color_manual(values = rev(cols),
                     labels = c("+ 3 \u00B0C", "+ 1.5 \u00B0C", "Ambient"),
                     name = "Treatment")+
  theme_bw(18)+
  #facet_wrap(~Treatment)+
  stat_smooth(aes(color = Treatment), method="glm", se = T, alpha = 0.25)+
  stat_cor(aes(color = Treatment, label = ..r.label..),
           method = "pearson", 
           size=5,  r.accuracy = 0.01, label.y = c(3.8,3.82,3.84))+
  labs(x = "Month", y = "Size (mm)")
#ggsave("../Figures/Growth_statsummary.png", width = 15, height = 12, dpi = 300, units = "cm")

png("../Figures/Size_dist.png", width = 8, height = 6, units = "cm", res = 600)
par(cex = 0.5)
hist(features_w$size..mm., breaks = 30, main = "", xlab = "Size (mm)")
dev.off()

mod <- glmer((size..mm.) ~ relevel(Treatment, ref = "amb") + 
                 Event +
                 (1|Tank), 
               data = features_w,
               family = gaussian(link = "log"))

#Random intercepts model?
mod_1 <- glmer((size..mm.) ~ relevel(Treatment, ref = "amb") + 
                 relevel(Event_factor, ref = "0") +
                 (1|Tank), 
               data = features_w,
               family = gaussian(link = "log"))

mod_2 <- glmer((size..mm.) ~ relevel(Treatment, ref = "amb") + 
                 (relevel(Month_factor, ref = "March")) +
                 (1|Tank), 
               data = features_w,
               family = gaussian(link = "log"))

mod_3 <- glmer((size..mm.) ~ relevel(Treatment, ref = "amb") + 
                 poly(Event, 2) +
                 (1|Tank), 
               data = features_w,
               family = gaussian(link = "log"))

mod_4 <- glmer((size..mm.) ~ #relevel(Treatment, ref = "amb") + 
                 #poly(Event,2) +
                 poly(Event,2) * relevel(Treatment, ref = "amb") +
                 (1|Tank), 
               data = features_w,
               family = gaussian(link = "log"))

mod_5 <- glmer((size..mm.) ~ 
                 relevel(Treatment, ref = "amb") + 
                 relevel(Event_factor, ref = "0") +
                 (1|Tank/Treatment), 
               data = features_w,
               family = gaussian(link = "log"),
               control = glmerControl(optimizer = "Nelder_Mead"))

anova(mod, mod_1, mod_2, mod_3, mod_4, mod_5)

Anova(mod_5, type="III")

par(mfrow=c(4,4))
png("../Figures/GLMM_Diagnostics_loglink_test.png", width = 16, height = 14, units = "cm", res = 300)
check_model(mod_1)
#diag_plots <- plot(diagnostics, return_list = TRUE)

dev.off()

summary(mod_5)
plot(acf(resid(mod_2)))
equatiomatic::extract_eq(mod_2, wrap = T)

mydf <- ggpredict(mod_5, terms = c("Event_factor [all]", "Treatment [all]"))

plot(mydf)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, 
                  fill = group, color = NULL), alpha = 0.25)+
  geom_line(size=1.25, linetype = 2)+
  scale_fill_manual(values = rev(cols),
                    labels = c("+ 3 \u00B0C", "+ 1.5 \u00B0C", "Ambient"),
                    name = "Treatment")+
  scale_color_manual(values = rev(cols),
                     labels = c("+ 3 \u00B0C", "+ 1.5 \u00B0C", "Ambient"),
                     name = "Treatment")+
  #geom_point(aes(x = Event, y = size..mm.),alpha = 0.05, color = features_w$Treatment, data = features_w)+
  theme_bw(18)+
  ggtitle("")+
  labs(y = "Size (mm)", x = "Sampling Event")+
  guides(fill = guide_legend(override.aes = list(alpha = 0.45)))
ggsave("../Figures/Growth_glmm_effects.png", width = 15, height = 12, dpi = 300, units = "cm")

