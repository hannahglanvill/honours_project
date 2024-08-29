### historic data cleaning and analyzing ###

library(dplyr)
library(ggplot2)
library(lubridate)
library(showtext)
library(ggtext)
library(purrr) 

#read in data

histdat <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/data/histdata_raw.csv")

#hourly averages for variables

av <- histdat %>%
  mutate(timestamp = as.POSIXct(datetime2)) %>%  
  filter(lubridate::year(timestamp) & lubridate::month(timestamp)) %>%
  group_by(date = lubridate::date(timestamp), hour = lubridate::hour(timestamp), exp, cat) %>%  
  summarise(height_av = mean(height, na.rm = TRUE), 
            Ta_av = mean(Ta, na.rm = TRUE),
            wind_av = mean(wind, na.rm = TRUE),
            BB_av = mean(BB, na.rm = TRUE),
            MT_av = mean(MT, na.rm = TRUE),
            solrad_av = mean(solrad, na.rm = TRUE))

av$datetime <- as.POSIXct(paste(av$date, av$hour), format = "%Y-%m-%d %H")
av <- av[order(av$datetime), ]


range(av$BB_av)


# blackbullb versus taxidermic mount 

# add font

font_add_google("Noto Sans JP", ## name of Google font
                "NSJ")  ## name that will be used in R
showtext_auto()

# plot

ggplot(av, aes(x = MT_av, y = BB_av)) + 
  geom_point(color = "grey30", size = 1.5, alpha = 0.5) +  
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 0.6) +
  labs(x = "taxidermic mount operative temperature (°C)",
       y = "blackbulb operative temperature (°C)") + 
  scale_x_continuous(limits = c(0, 50)) + 
  scale_y_continuous(limits = c(0, 50)) +
  theme_minimal() +  
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 11, family = "NSJ"),
        axis.title.x = element_text(vjust = 0, size = 15, family = "NSJ"),
        axis.title.y = element_text(vjust = 2, size = 15, family = "NSJ")) +
  facet_wrap( ~ exp, ncol = 3, scales = "free_x") +
  #coord_fixed() +
  theme(strip.text = element_text(size = 12, family = "NSJ", face = "bold")) 


# add difference column 

av_dif <- av %>%
  mutate(tmvbb = MT_av - BB_av)

av <- av %>%
  mutate(tmvbb = MT_av - BB_av)

# boxplot of difference

ggplot(av_dif, aes(x = factor(exp, levels = c("shade", "dappled", "sun")), y = tmvbb, fill = exp)) +
  geom_boxplot() +
  labs(x = "microsite categories", y = "taxidermic mount - blackbulb operative temperature (°C)", 
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

# filter for height categories 

av0 <- av %>%
  filter(height_av == "0")

av1 <- av %>%
  filter(height_av == "1")

av2 <- av %>%
  filter(height_av == "2")

av3 <- av %>%
  filter(height_av == "3")

# filter for exposure categories

av_sun <- av %>%
  filter(exp == "sun")

av_dap <- av %>%
  filter(exp == "dappled")

av_shade <- av %>%
  filter(exp == "shade")

# filter for each microsite category

sun0 <- av %>%
  filter(cat == "0sun")
sun1 <- av %>%
  filter(cat == "1sun")
sun2 <- av %>%
  filter(cat == "2sun")
sun3 <- av %>%
  filter(cat == "3sun")

dap0 <- av %>%
  filter(cat == "0dappled")
dap1 <- av %>%
  filter(cat == "1dappled")
dap2 <- av %>%
  filter(cat == "2dappled")
dap3 <- av %>%
  filter(cat == "3dappled")

shade0 <- av %>%
  filter(cat == "0shade")
shade1 <- av %>%
  filter(cat == "1shade")
shade2 <- av %>%
  filter(cat == "2shade")
shade3 <- av %>%
  filter(cat == "3shade")


# lm model

mod_tmvbb <- lm(BB_av ~ MT_av, data = av)
summary(mod_tmvbb)
confint(mod_tmvbb)

summary(lm(BB_av ~ MT_av, data = shade3))
confint(lm(BB_av ~ MT_av, data = shade3))


# anova

summary(aov(tmvbb ~ cat, data = av_dif))
TukeyHSD(aov(tmvbb ~ cat, data = av_dif))

summary(aov(tmvbb ~ exp, data = av_dif))
TukeyHSD(aov(tmvbb ~ exp, data = av_dif)) 

summary(aov(tmvbb ~ height_av, data = av_dif))
TukeyHSD(aov(tmvbb ~ height_av, data = av_dif))

# t test whether different to 0

t.test(sun0$tmvbb)
t.test(sun1$tmvbb)
t.test(sun2$tmvbb)
t.test(sun3$tmvbb)

t.test(dap0$tmvbb)
t.test(dap1$tmvbb)
t.test(dap2$tmvbb)
t.test(dap3$tmvbb)

t.test(shade0$tmvbb)
t.test(shade1$tmvbb)
t.test(shade2$tmvbb)
t.test(shade3$tmvbb)
