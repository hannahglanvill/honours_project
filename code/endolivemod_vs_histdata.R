
### comparison endotherm model (living) versus taxidermic mount operative temperature measures ###

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

## sun

# join dataframes

sun0_cs_live_tog <- bind_rows(sun0_c1_live_fm, sun0_c2_live_fm, sun0_c3_live_fm)
sun1_cs_live_tog <- bind_rows(sun1_c1_live_fm, sun1_c2_live_fm, sun1_c3_live_fm)
sun2_cs_live_tog <- bind_rows(sun2_c1_live_fm, sun2_c2_live_fm, sun2_c3_live_fm)
sun3_cs_live_tog <- bind_rows(sun3_c1_live_fm, sun3_c2_live_fm, sun3_c3_live_fm)

sun_cs_live_tog <- bind_rows(sun0_c1_live_fm, sun0_c2_live_fm, sun0_c3_live_fm, 
                        sun1_c1_live_fm, sun1_c2_live_fm, sun1_c3_live_fm,
                        sun2_c1_live_fm, sun2_c2_live_fm, sun2_c3_live_fm,
                        sun3_c1_live_fm, sun3_c2_live_fm, sun3_c3_live_fm)

# linear models

summary(lm(MT_av ~ TM_live, data = sun_cs_live_tog))
confint(lm(MT_av ~ TM_live, data = sun_cs_live_tog))

summary(lm(MT_av ~ TSKIN_D.env, data = sun_cs_live_tog))
confint(lm(MT_av ~ TSKIN_D.env, data = sun_cs_live_tog))

summary(lm(MT_av ~ TSKIN_V.env, data = sun_cs_live_tog))
confint(lm(MT_av ~ TSKIN_V.env, data = sun_cs_live_tog))

## dap

# join dataframes

dap0_cs_live_tog <- bind_rows(dap0_c1_live_fm, dap0_c2_live_fm, dap0_c3_live_fm)
dap1_cs_live_tog <- bind_rows(dap1_c1_live_fm, dap1_c2_live_fm, dap1_c3_live_fm)
dap2_cs_live_tog <- bind_rows(dap2_c1_live_fm, dap2_c2_live_fm, dap2_c3_live_fm)
dap3_cs_live_tog <- bind_rows(dap3_c1_live_fm, dap3_c2_live_fm, dap3_c3_live_fm)

dap_cs_live_tog <- bind_rows(dap0_c1_live_fm, dap0_c2_live_fm, dap0_c3_live_fm, 
                             dap1_c1_live_fm, dap1_c2_live_fm, dap1_c3_live_fm, 
                             dap2_c1_live_fm, dap2_c2_live_fm, dap2_c3_live_fm, 
                             dap3_c1_live_fm, dap3_c2_live_fm, dap3_c3_live_fm)

# linear models

summary(lm(MT_av ~ TM_live, data = dap_cs_live_tog))
confint(lm(MT_av ~ TM_live, data = dap_cs_live_tog))

summary(lm(MT_av ~ TSKIN_D.env, data = dap_cs_live_tog))
confint(lm(MT_av ~ TSKIN_D.env, data = dap_cs_live_tog))

summary(lm(MT_av ~ TSKIN_V.env, data = dap_cs_live_tog))
confint(lm(MT_av ~ TSKIN_V.env, data = dap_cs_live_tog))

## shade 

# join dataframes

shade0_cs_live_tog <- bind_rows(shade0_c1_live_fm, shade0_c2_live_fm, shade0_c3_live_fm)
shade1_cs_live_tog <- bind_rows(shade1_c1_live_fm, shade1_c2_live_fm, shade1_c3_live_fm)
shade2_cs_live_tog <- bind_rows(shade2_c1_live_fm, shade2_c2_live_fm, shade2_c3_live_fm)
shade3_cs_live_tog <- bind_rows(shade3_c1_live_fm, shade3_c2_live_fm, shade3_c3_live_fm)

shade_cs_live_tog <- bind_rows(shade0_c1_live_fm, shade0_c2_live_fm, shade0_c3_live_fm,
                               shade1_c1_live_fm, shade1_c2_live_fm, shade1_c3_live_fm,
                               shade2_c1_live_fm, shade2_c2_live_fm, shade2_c3_live_fm, 
                               shade3_c1_live_fm, shade3_c2_live_fm, shade3_c3_live_fm)

# linear models

summary(lm(MT_av ~ TM_live, data = shade_cs_live_tog))
confint(lm(MT_av ~ TM_live, data = shade_cs_live_tog))

summary(lm(MT_av ~ TSKIN_D.env, data = shade_cs_live_tog))
confint(lm(MT_av ~ TSKIN_D.env, data = shade_cs_live_tog))

summary(lm(MT_av ~ TSKIN_V.env, data = shade_cs_live_tog))
confint(lm(MT_av ~ TSKIN_V.env, data = shade_cs_live_tog))

## height categories

# join data frames

height0_cs_live <- bind_rows(sun0_c1_live_fm, dap0_c1_live_fm, shade0_c1_live_fm, 
                        sun0_c2_live_fm, dap0_c2_live_fm, shade0_c2_live_fm, 
                        sun0_c3_live_fm, dap0_c3_live_fm, shade0_c3_live_fm)
height1_cs_live <- bind_rows(sun1_c1_live_fm, dap1_c1_live_fm, shade1_c1_live_fm, 
                        sun1_c2_live_fm, dap1_c2_live_fm, shade1_c2_live_fm, 
                        sun1_c3_live_fm, dap1_c3_live_fm, shade1_c3_live_fm)
height2_cs_live <- bind_rows(sun2_c1_live_fm, dap2_c1_live_fm, shade2_c1_live_fm, 
                        sun2_c2_live_fm, dap2_c2_live_fm, shade2_c2_live_fm, 
                        sun2_c3_live_fm, dap2_c3_live_fm, shade2_c3_live_fm)
height3_cs_live <- bind_rows(sun3_c1_live_fm, dap3_c1_live_fm, shade3_c1_live_fm, 
                        sun3_c2_live_fm, dap3_c2_live_fm, shade3_c2_live_fm, 
                        sun3_c3_live_fm, dap3_c3_live_fm, shade3_c3_live_fm)

# linear models
summary(lm(MT_av ~ TM_live, data = height0_cs_live))
confint(lm(MT_av ~ TM_live, data = height0_cs_live))

summary(lm(MT_av ~ TSKIN_D.env, data = height0_cs_live))
confint(lm(MT_av ~ TSKIN_D.env, data = height0_cs_live))

summary(lm(MT_av ~ TSKIN_V.env, data = height0_cs_live))
confint(lm(MT_av ~ TSKIN_V.env, data = height0_cs_live))



## facet wrap plots

# join to one big data frame 

all_micro_cs_live_tog <- bind_rows(sun0_c1_live_fm %>% mutate(coord = "c1"),
                              sun1_c1_live_fm %>% mutate(coord = "c1"),
                              sun2_c1_live_fm %>% mutate(coord = "c1"),
                              sun3_c1_live_fm %>% mutate(coord = "c1"),
                              sun0_c2_live_fm %>% mutate(coord = "c2"),
                              sun1_c2_live_fm %>% mutate(coord = "c2"),
                              sun2_c2_live_fm %>% mutate(coord = "c2"),
                              sun3_c2_live_fm %>% mutate(coord = "c2"), 
                              sun0_c3_live_fm %>% mutate(coord = "c3"),
                              sun1_c3_live_fm %>% mutate(coord = "c3"),
                              sun2_c3_live_fm %>% mutate(coord = "c3"),
                              sun3_c3_live_fm %>% mutate(coord = "c3"),
                              
                              dap0_c1_live_fm %>% mutate(coord = "c1"),
                              dap1_c1_live_fm %>% mutate(coord = "c1"),
                              dap2_c1_live_fm %>% mutate(coord = "c1"),
                              dap3_c1_live_fm %>% mutate(coord = "c1"),
                              dap0_c2_live_fm %>% mutate(coord = "c2"),
                              dap1_c2_live_fm %>% mutate(coord = "c2"),
                              dap2_c2_live_fm %>% mutate(coord = "c2"),
                              dap3_c2_live_fm %>% mutate(coord = "c2"), 
                              dap0_c3_live_fm %>% mutate(coord = "c3"),
                              dap1_c3_live_fm %>% mutate(coord = "c3"),
                              dap2_c3_live_fm %>% mutate(coord = "c3"),
                              dap3_c3_live_fm %>% mutate(coord = "c3"),
                              
                              shade0_c1_live_fm %>% mutate(coord = "c1"),
                              shade1_c1_live_fm %>% mutate(coord = "c1"),
                              shade2_c1_live_fm %>% mutate(coord = "c1"),
                              shade3_c1_live_fm %>% mutate(coord = "c1"),
                              shade0_c2_live_fm %>% mutate(coord = "c2"),
                              shade1_c2_live_fm %>% mutate(coord = "c2"),
                              shade2_c2_live_fm %>% mutate(coord = "c2"),
                              shade3_c2_live_fm %>% mutate(coord = "c2"), 
                              shade0_c3_live_fm %>% mutate(coord = "c3"),
                              shade1_c3_live_fm %>% mutate(coord = "c3"),
                              shade2_c3_live_fm %>% mutate(coord = "c3"),
                              shade3_c3_live_fm %>% mutate(coord = "c3"))


# linear model

summary(lm(MT_av ~ TM_live, data = all_micro_cs_live_tog))
confint(lm(MT_av ~ TM_live, data = all_micro_cs_live_tog))

summary(lm(MT_av ~ TSKIN_D.env, data = all_micro_cs_live_tog))
confint(lm(MT_av ~ TSKIN_D.env, data = all_micro_cs_live_tog))

summary(lm(MT_av ~ TSKIN_V.env, data = all_micro_cs_live_tog))
confint(lm(MT_av ~ TSKIN_V.env, data = all_micro_cs_live_tog))


# linear regression plot

all_micro_cs_live_tog <- all_micro_cs_live_tog[complete.cases(all_micro_cs_tog$MT_av, all_micro_cs_tog$TM_live), ]

filtered_data <- all_micro_cs_live_tog %>%
  filter(!is.na(exp) & !is.na(height_av))

ggplot(filtered_data, aes(y = MT_av)) +
  geom_point(aes(x = TM_live, color = "body temp (°C)"), size = 1.2, alpha = 0.3) +  
  geom_smooth(aes(x = TM_live, color = "body temp (°C)"), method = "lm", se = F) + 
  geom_point(aes(x = TSKIN_D.env, color = "dorsal skin temp (°C)"), size = 1.2, alpha = 0.3) +  
  geom_smooth(aes(x = TSKIN_D.env, color = "dorsal skin temp (°C)"), method = "lm", se = F) + 
  geom_point(aes(x = TSKIN_V.env, color = "ventral skin temp (°C)"), size = 1.2, alpha = 0.3) +  
  geom_smooth(aes(x = TSKIN_V.env, color = "ventral skin temp (°C)"), method = "lm", se = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 0.6) +
  labs(x = "model operative temperature (°C)", 
       y = "taxidermic mount operative temperature (°C)", 
       color = "model output") +  # Set the legend title here
  scale_x_continuous(limits = c(0, 70)) + 
  scale_y_continuous(limits = c(0, 70)) +
  scale_color_manual(values = c("body temp (°C)" = "orange", "dorsal skin temp (°C)" = "green", "ventral skin temp (°C)" = "blue")) +
  theme_minimal() +  
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 11, family = "NSJ"),
        axis.title.x = element_text(vjust = 0, size = 15, family = "NSJ"),
        axis.title.y = element_text(vjust = 2, size = 15, family = "NSJ"),
        legend.title = element_text(size = 13, family = "NSJ"),  # Control the appearance of the legend title here
        legend.position = "bottom") +
  facet_wrap( ~ height_av, ncol = 2, scales = "free_x") +
  #coord_fixed() +
  theme(strip.text = element_text(size = 12, family = "NSJ", face = "bold"))


# boxplot of difference between model and actual

# add difference column 

filtered_data <- filtered_data %>%
  mutate(modvsact = TM_live - MT_av)

# boxplot

ggplot(filtered_data, aes(x = coord, y = modvsact, fill = coord)) +
  geom_boxplot() +
  labs(x = "microsite category",
       y = "model - taxidermic mount operative temperature (°C)", 
       fill = "coordinate number") +
  #facet_wrap(~ exp, ncol = 3) +
  theme_minimal() +
  theme(strip.text = element_text(color = "black", face = "bold", 
                                  hjust = 0.5, size = 12, family = "NSJ")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"), 
        legend.title = element_text(family = "NSJ",
                                    color = "black",
                                    size = 12)) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 11, family = "NSJ"),
        axis.title.x = element_text(vjust = 0, size = 15, family = "NSJ"),
        axis.title.y = element_text(vjust = 2, size = 15, family = "NSJ")) +
  scale_fill_manual(values = c("salmon", "lightblue", "lightgreen"))+
  scale_y_continuous(limits = c(-20, 20))  

## anova of coordinate effect

summary(aov(modvsact ~ coord*exp, data = filtered_data))
TukeyHSD(aov(modvsact ~ coord*exp, data = filtered_data))

summary(aov(modvsact ~ coord, data = filtered_data))
TukeyHSD(aov(modvsact ~ coord, data = filtered_data))

