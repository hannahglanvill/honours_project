### comparison ectotherm model versus blackbulb operative temperature measures for novel data###

library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(lubridate)

# open csv ~ model output with emperical data
nov_sun0_env <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/output/ectotherm_mod_nov/nov_sun0_env.csv")
nov_sun1_env <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/output/ectotherm_mod_nov/nov_sun1_env.csv")
nov_sun2_env <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/output/ectotherm_mod_nov/nov_sun2_env.csv")
nov_sun3_env <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/output/ectotherm_mod_nov/nov_sun3_env.csv")

nov_dap0_env <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/output/ectotherm_mod_nov/nov_dap0_env.csv")
nov_dap1_env <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/output/ectotherm_mod_nov/nov_dap1_env.csv")
nov_dap2_env <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/output/ectotherm_mod_nov/nov_dap2_env.csv")
nov_dap3_env <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/output/ectotherm_mod_nov/nov_dap3_env.csv")

nov_shade0_env <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/output/ectotherm_mod_nov/nov_shade0_env.csv")
nov_shade1_env <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/output/ectotherm_mod_nov/nov_shade1_env.csv")
nov_shade2_env <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/output/ectotherm_mod_nov/nov_shade2_env.csv")
nov_shade3_env <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/output/ectotherm_mod_nov/nov_shade3_env.csv")


## sun

# join dataframes

nov_sun_env_tog <- bind_rows(nov_sun0_env, nov_sun1_env, nov_sun2_env, nov_sun3_env)

# linear models

summary(lm(BBav_av ~ TC, data = nov_sun1_env))
confint(lm(BBav_av ~ TC, data = nov_sun1_env))

## dap

# join dataframes

nov_dap_env_tog <- bind_rows(nov_dap0_env, nov_dap1_env, nov_dap2_env, nov_dap3_env)

# linear models

summary(lm(BBav_av ~ TC, data = nov_dap_env_tog))
confint(lm(BBav_av ~ TC, data = nov_dap_env_tog))

## shade 

# join dataframes

nov_shade_env_tog <- bind_rows(nov_shade0_env, nov_shade1_env, nov_shade2_env, nov_shade3_env)

# linear models

summary(lm(BBav_av ~ TC, data = nov_shade1_env))
confint(lm(BBav_av ~ TC, data = nov_shade1_env))


## height categories

# join data frames

nov_height0 <- bind_rows(nov_sun0_env, nov_dap0_env, nov_shade0_env)

nov_height1 <- bind_rows(nov_sun1_env, nov_dap1_env, nov_shade1_env)

nov_height2 <- bind_rows(nov_sun2_env, nov_dap2_env, nov_shade2_env)

nov_height3 <- bind_rows(nov_sun3_env, nov_dap3_env, nov_shade3_env)

# linear models

summary(lm(BBav_av ~ TC, data = nov_height1))
confint(lm(BBav_av ~ TC, data = nov_height1))


## facet wrap plots

# join to one big data frame 

nov_all_tog <- bind_rows(nov_sun0_env, nov_dap0_env, nov_shade0_env,
                         nov_sun1_env, nov_dap1_env, nov_shade1_env,
                         nov_sun2_env, nov_dap2_env, nov_shade2_env,
                         nov_sun3_env, nov_dap3_env, nov_shade3_env)

nov_all_tog <- nov_all_tog[complete.cases(nov_all_tog$BBav_av, nov_all_tog$TC), ]

# ranges

range(nov_all_tog$TA)

# linear model

summary(lm(BBav_av ~ TC, data = nov_all_tog))
confint(lm(BBav_av ~ TC, data = nov_all_tog))
plot(lm(BBav_av ~ TC, data = nov_all_tog))

# linear regression plot

ggplot(nov_all_tog, aes(x = TC, y = BBav_av)) +
  geom_point(color = "grey30", size = 1.5, alpha = 0.5) +  
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 0.6) +
  labs(x = "model operative temperature (°C)", 
       y = "blackbulb operative temperature (°C)") + 
  scale_x_continuous(limits = c(0, 45)) + 
  scale_y_continuous(limits = c(0, 45)) +
  theme_minimal() +  
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 11, family = "NSJ"),
        axis.title.x = element_text(vjust = 0, size = 15, family = "NSJ"),
        axis.title.y = element_text(vjust = 2, size = 15, family = "NSJ")) +
  facet_wrap( ~ exp, ncol = 3, scales = "free_x") +
  #coord_fixed() +
  theme(strip.text = element_text(size = 12, family = "NSJ", face = "bold")) 


## boxplot of difference between model and actual

# add difference column 

nov_all_tog <- nov_all_tog %>%
  mutate(modvsact = TC - BBav_av)

# boxplot

ggplot(nov_all_tog, aes(x = factor(exp, levels = c("shade", "dappled", "sun")), y = modvsact, fill = exp)) +
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


# anovas

nov_all_tog$height_av <- as.factor(nov_all_tog$height_av)

summary(aov(modvsact ~ height_av, data = nov_all_tog))
TukeyHSD(aov(modvsact ~ height_av, data = nov_all_tog))

# filter to microsite category again for t tests
nov_all_tog_sun0 <- nov_all_tog %>%
  filter(cat == "sun0")
nov_all_tog_sun1 <- nov_all_tog %>%
  filter(cat == "sun1")
nov_all_tog_sun2 <- nov_all_tog %>%
  filter(cat == "sun2")
nov_all_tog_sun3 <- nov_all_tog %>%
  filter(cat == "sun3")

nov_all_tog_dap0 <- nov_all_tog %>%
  filter(cat == "dap0")
nov_all_tog_dap1 <- nov_all_tog %>%
  filter(cat == "dap1")
nov_all_tog_dap2 <- nov_all_tog %>%
  filter(cat == "dap2")
nov_all_tog_dap3 <- nov_all_tog %>%
  filter(cat == "dap3")

nov_all_tog_shade0 <- nov_all_tog %>%
  filter(cat == "shade0")
nov_all_tog_shade1 <- nov_all_tog %>%
  filter(cat == "shade1")
nov_all_tog_shade2 <- nov_all_tog %>%
  filter(cat == "shade2")
nov_all_tog_shade3 <- nov_all_tog %>%
  filter(cat == "shade3")

# t test whether different to 0
t.test(nov_all_tog_sun0$modvsact)
t.test(nov_all_tog_sun1$modvsact)
t.test(nov_all_tog_sun2$modvsact)
t.test(nov_all_tog_sun3$modvsact)

t.test(nov_all_tog_dap0$modvsact)
t.test(nov_all_tog_dap1$modvsact)
t.test(nov_all_tog_dap2$modvsact)
t.test(nov_all_tog_dap3$modvsact)

t.test(nov_all_tog_shade0$modvsact)
t.test(nov_all_tog_shade1$modvsact)
t.test(nov_all_tog_shade2$modvsact)
t.test(nov_all_tog_shade3$modvsact)
