### comparison ectotherm model versus blackbulb operative temperature measures for historic data###

library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(lubridate)


## sun

# join dataframes

sun0_cs_tog <- bind_rows(sun0_c1_env, sun0_c2_env, sun0_c3_env)
sun1_cs_tog <- bind_rows(sun1_c1_env, sun1_c2_env, sun1_c3_env)
sun2_cs_tog <- bind_rows(sun2_c1_env, sun2_c2_env, sun2_c3_env)
sun3_cs_tog <- bind_rows(sun3_c1_env, sun3_c2_env, sun3_c3_env)

sun_cs_tog <- bind_rows(sun0_c1_env, sun0_c2_env, sun0_c3_env, 
                        sun1_c1_env, sun1_c2_env, sun1_c3_env,
                        sun2_c1_env, sun2_c2_env, sun2_c3_env,
                        sun3_c1_env, sun3_c2_env, sun3_c3_env)

# linear models

summary(lm(BB_av ~ TC, data = sun_cs_tog))
confint(lm(BB_av ~ TC, data = sun_cs_tog))

## dap

# join dataframes

dap0_cs_tog <- bind_rows(dap0_c1_env, dap0_c2_env, dap0_c3_env)
dap1_cs_tog <- bind_rows(dap1_c1_env, dap1_c2_env, dap1_c3_env)
dap2_cs_tog <- bind_rows(dap2_c1_env, dap2_c2_env, dap2_c3_env)
dap3_cs_tog <- bind_rows(dap3_c1_env, dap3_c2_env, dap3_c3_env)

dap_cs_tog <- bind_rows(dap0_c1_env, dap0_c2_env, dap0_c3_env, 
                        dap1_c1_env, dap1_c2_env, dap1_c3_env, 
                        dap2_c1_env, dap2_c2_env, dap2_c3_env, 
                        dap3_c1_env, dap3_c2_env, dap3_c3_env)

# linear models

summary(lm(BB_av ~ TC, data = dap_cs_tog))
confint(lm(BB_av ~ TC, data = dap_cs_tog))

## shade 

# join dataframes

shade0_cs_tog <- bind_rows(shade0_c1_env, shade0_c2_env, shade0_c3_env)
shade1_cs_tog <- bind_rows(shade1_c1_env, shade1_c2_env, shade1_c3_env)
shade2_cs_tog <- bind_rows(shade2_c1_env, shade2_c2_env, shade2_c3_env)
shade3_cs_tog <- bind_rows(shade3_c1_env, shade3_c2_env, shade3_c3_env)

shade_cs_tog <- bind_rows(shade0_c1_env, shade0_c2_env, shade0_c3_env,
                          shade1_c1_env, shade1_c2_env, shade1_c3_env,
                          shade2_c1_env, shade2_c2_env, shade2_c3_env, 
                          shade3_c1_env, shade3_c2_env, shade3_c3_env)

# linear models

summary(lm(BB_av ~ TC, data = shade_cs_tog))
confint(lm(BB_av ~ TC, data = shade_cs_tog))


## height categories

# join data frames

height0_cs <- bind_rows(sun0_c1_env, dap0_c1_env, shade0_c1_env, 
                        sun0_c2_env, dap0_c2_env, shade0_c2_env, 
                        sun0_c3_env, dap0_c3_env, shade0_c3_env)
height1_cs <- bind_rows(sun1_c1_env, dap1_c1_env, shade1_c1_env, 
                        sun1_c2_env, dap1_c2_env, shade1_c2_env, 
                        sun1_c3_env, dap1_c3_env, shade1_c3_env)
height2_cs <- bind_rows(sun2_c1_env, dap2_c1_env, shade2_c1_env, 
                        sun2_c2_env, dap2_c2_env, shade2_c2_env, 
                        sun2_c3_env, dap2_c3_env, shade2_c3_env)
height3_cs <- bind_rows(sun3_c1_env, dap3_c1_env, shade3_c1_env, 
                        sun3_c2_env, dap3_c2_env, shade3_c2_env, 
                        sun3_c3_env, dap3_c3_env, shade3_c3_env)

# linear models
summary(lm(BB_av ~ TC, data = height3_cs))
confint(lm(BB_av ~ TC, data = height3_cs))


## facet wrap plots

# join to one big data frame 

all_micro_cs_tog <- bind_rows(sun0_c1_env %>% mutate(coord = "c1"),
                              sun1_c1_env %>% mutate(coord = "c1"),
                              sun2_c1_env %>% mutate(coord = "c1"),
                              sun3_c1_env %>% mutate(coord = "c1"),
                              sun0_c2_env %>% mutate(coord = "c2"),
                              sun1_c2_env %>% mutate(coord = "c2"),
                              sun2_c2_env %>% mutate(coord = "c2"),
                              sun3_c2_env %>% mutate(coord = "c2"), 
                              sun0_c3_env %>% mutate(coord = "c3"),
                              sun1_c3_env %>% mutate(coord = "c3"),
                              sun2_c3_env %>% mutate(coord = "c3"),
                              sun3_c3_env %>% mutate(coord = "c3"),
                              
                              dap0_c1_env %>% mutate(coord = "c1"),
                              dap1_c1_env %>% mutate(coord = "c1"),
                              dap2_c1_env %>% mutate(coord = "c1"),
                              dap3_c1_env %>% mutate(coord = "c1"),
                              dap0_c2_env %>% mutate(coord = "c2"),
                              dap1_c2_env %>% mutate(coord = "c2"),
                              dap2_c2_env %>% mutate(coord = "c2"),
                              dap3_c2_env %>% mutate(coord = "c2"), 
                              dap0_c3_env %>% mutate(coord = "c3"),
                              dap1_c3_env %>% mutate(coord = "c3"),
                              dap2_c3_env %>% mutate(coord = "c3"),
                              dap3_c3_env %>% mutate(coord = "c3"),
  
                              shade0_c1_env %>% mutate(coord = "c1"),
                              shade1_c1_env %>% mutate(coord = "c1"),
                              shade2_c1_env %>% mutate(coord = "c1"),
                              shade3_c1_env %>% mutate(coord = "c1"),
                              shade0_c2_env %>% mutate(coord = "c2"),
                              shade1_c2_env %>% mutate(coord = "c2"),
                              shade2_c2_env %>% mutate(coord = "c2"),
                              shade3_c2_env %>% mutate(coord = "c2"), 
                              shade0_c3_env %>% mutate(coord = "c3"),
                              shade1_c3_env %>% mutate(coord = "c3"),
                              shade2_c3_env %>% mutate(coord = "c3"),
                              shade3_c3_env %>% mutate(coord = "c3"))

all_micro_cs_tog <- all_micro_cs_tog[complete.cases(all_micro_cs_tog$BB_av, all_micro_cs_tog$TC), ]

range(all_micro_cs_live_tog$TA)

# linear model

summary(lm(BB_av ~ TC, data = all_micro_cs_tog))
confint(lm(BB_av ~ TC, data = all_micro_cs_tog))
plot(lm(BB_av ~ TC, data = all_micro_cs_tog))


# linear regression plot

ggplot(all_micro_cs_tog, aes(x = TC, y = BB_av)) +
  geom_point(color = "grey30", size = 1.5, alpha = 0.5) +  
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 0.6) +
  labs(x = "model operative temperature (°C)", 
       y = "blackbulb operative temperature (°C)") + 
  scale_x_continuous(limits = c(0, 50)) + 
  scale_y_continuous(limits = c(0, 50)) +
  theme_minimal() +  
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 11, family = "NSJ"),
        axis.title.x = element_text(vjust = 0, size = 15, family = "NSJ"),
        axis.title.y = element_text(vjust = 2, size = 15, family = "NSJ")) +
  facet_wrap(exp ~ height_av, ncol = 4, scales = "free_x") +
  #coord_fixed() +
  theme(strip.text = element_text(size = 12, family = "NSJ", face = "bold")) 


# boxplot of difference between model and actual

# add difference column 

all_micro_cs_tog <- all_micro_cs_tog %>%
  mutate(modvsact = TC - BB_av)

# boxplot

ggplot(all_micro_cs_tog, aes(x = coord, y = modvsact, fill = coord)) +
  geom_boxplot() +
  labs(x = "microsite category",
       y = "model - blackbulb operative temperature (°C)", 
       fill = "coordinate number") +
  #facet_wrap(~ exp, ncol = 3) +
  theme_minimal() +
  theme(strip.text = element_text(color = "black",
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


ggplot(all_micro_cs_tog, aes(x = factor(exp, levels = c("shade", "dappled", "sun")), y = modvsact, fill = exp)) +
  geom_boxplot() +
  labs(x = "microsite categories", y = "model - blackbulb operative temperature (°C)", 
       fill = "exposure category") +
  facet_wrap(~ height_av, ncol = 2, nrow = 2, scales = "free_x") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  theme_minimal() +
  theme(strip.text = element_text(color = "black", face = "bold",
                                  hjust = 0.5, size = 12, family = "NSJ"),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"), 
        legend.title = element_text(family = "NSJ",
                                    color = "black",
                                    size = 15),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 11, family = "NSJ"),
        axis.title.x = element_text(vjust = -2, size = 15, family = "NSJ"),
        axis.title.y = element_text(vjust = 2, size = 15, family = "NSJ"),
        panel.spacing.y = unit(1, "cm"),
        strip.placement = "outside") +  
  scale_fill_manual(values = c("shade" = "grey9", "dappled" = "grey", "sun" = "grey99"), 
                    breaks = c("shade", "dappled", "sun")) +
  scale_y_continuous(limits = c(-20, 20)) +
  theme(panel.spacing = unit(2, "lines")) +  
  theme(axis.title.x = element_text(size = 12),  
        axis.text.x = element_text(size = 10))  


## anova of coordinate effect
summary(aov(modvsact ~ coord, data = all_micro_cs_tog))
TukeyHSD(aov(modvsact ~ coord, data = all_micro_cs_tog))

summary(aov(modvsact ~ height_av, data = all_micro_cs_tog))
TukeyHSD(aov(modvsact ~ height_av, data = all_micro_cs_tog))


# filter to microsite category again for t tests
all_sun0 <- all_micro_cs_tog %>%
  filter(cat == "0sun")
all_sun1 <- all_micro_cs_tog %>%
  filter(cat == "1sun")
all_sun2 <- all_micro_cs_tog %>%
  filter(cat == "2sun")
all_sun3 <- all_micro_cs_tog %>%
  filter(cat == "3sun")

all_dap0 <- all_micro_cs_tog %>%
  filter(cat == "0dappled")
all_dap1 <- all_micro_cs_tog %>%
  filter(cat == "1dappled")
all_dap2 <- all_micro_cs_tog %>%
  filter(cat == "2dappled")
all_dap3 <- all_micro_cs_tog %>%
  filter(cat == "3dappled")

all_shade0 <- all_micro_cs_tog %>%
  filter(cat == "0shade")
all_shade1 <- all_micro_cs_tog %>%
  filter(cat == "1shade")
all_shade2 <- all_micro_cs_tog %>%
  filter(cat == "2shade")
all_shade3 <- all_micro_cs_tog %>%
  filter(cat == "3shade")

# t test whether different to 0

t.test(all_sun0$modvsact)
t.test(all_sun1$modvsact)
t.test(all_sun2$modvsact)
t.test(all_sun3$modvsact)

t.test(all_dap0$modvsact)
t.test(all_dap1$modvsact)
t.test(all_dap2$modvsact)
t.test(all_dap3$modvsact)

t.test(all_shade0$modvsact)
t.test(all_shade1$modvsact)
t.test(all_shade2$modvsact)
t.test(all_shade3$modvsact)

