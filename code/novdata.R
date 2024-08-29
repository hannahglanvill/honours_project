### novel data cleaning and analyzing ###

library(dplyr)
library(ggplot2)
library(lubridate)
library(showtext)
library(purrr) 

#read in data

novdat <- read.csv("C:/Users/hanna/OneDrive/Documents/honours2024/project/GIT/honours_project/data/noveldata.csv")

#hourly averages for variables

novdat <- novdat %>%
  mutate(datetime = as.character(datetime), 
         timestamp = as.POSIXct(datetime, format = "%Y/%m/%d %H:%M"))

nov_av <- novdat %>%
  filter(lubridate::year(timestamp) & lubridate::month(timestamp)) %>%
  group_by(date = lubridate::date(timestamp), hour = lubridate::hour(timestamp), exp, cat) %>%  
  summarise(height_av = mean(height, na.rm = TRUE), 
            NJtm_av = mean(NJ_tm, na.rm = TRUE),
            NJpl_av = mean(NJ_pl, na.rm = TRUE),
            RLtm_av = mean(RL_tm, na.rm = TRUE),
            BBav_av = mean(BBav, na.rm = TRUE))

#create a datetime column from date and hour for sorting

nov_av$datetime <- as.POSIXct(paste(nov_av$date, nov_av$hour), format = "%Y-%m-%d %H")
nov_av <- nov_av[order(nov_av$datetime), ]

#convert height to factor variable

nov_av$height <- as.factor(nov_av$height_av)

#temp range

range(nov_av$BBav_av)
range(nov_av$NJtm_av)
range(nov_av$NJpl_av)
range(nov_av$RLtm_av)

# blackbullb versus taxidermic mount 

# add font

font_add_google("Noto Sans JP", ## name of Google font
                "NSJ")  ## name that will be used in R
showtext_auto()

# lm plots

ggplot(nov_av, aes(y = BBav_av)) +
  geom_point(aes(x = NJtm_av, color = "nightjar taxidermic mount"), size = 1.5, alpha = 0.5) +  
  geom_smooth(aes(x = NJtm_av, color = "nightjar taxidermic mount"), method = "lm", se = F) + 
  geom_point(aes(x = NJpl_av, color = "3D printed nightjar"), size = 1.5, alpha = 0.5) +  
  geom_smooth(aes(x = NJpl_av, color = "3D printed nightjar"), method = "lm", se = F) + 
  geom_point(aes(x = RLtm_av, color = "red lark taxidermic mount"), size = 1.5, alpha = 0.5) +  
  geom_smooth(aes(x = RLtm_av, color = "red lark taxidermic mount"), method = "lm", se = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 0.6) +
  labs(y = "blackbulb operative temperature (°C)", 
       x = "operative temperature (°C)", 
       color = "emperical model") +  # Set the legend title here
  scale_x_continuous(limits = c(0, 45)) + 
  scale_y_continuous(limits = c(0, 45)) +
  scale_color_manual(values = c("nightjar taxidermic mount" = "#8D1F5C", "3D printed nightjar" = "#76CCCD", "red lark taxidermic mount" = "#F05624")) +
  theme_minimal() +  
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 11, family = "NSJ"),
        axis.title.x = element_text(vjust = 0, size = 15, family = "NSJ"),
        axis.title.y = element_text(vjust = 2, size = 15, family = "NSJ"),
        legend.title = element_text(size = 13, family = "NSJ"),  # Control the appearance of the legend title here
        legend.position = "bottom") +
  facet_wrap(exp ~ height_av, ncol = 4, scales = "free_x") +
  #coord_fixed() +
  theme(strip.text = element_text(size = 12, family = "NSJ", face = "bold"))

# individual lm plots

ggplot(nov_av, aes(x = NJpl_av, y = BBav_av)) + 
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
  #facet_wrap( ~ exp, ncol = 3, scales = "free_x") +
  coord_fixed() +
  theme(strip.text = element_text(size = 12, family = "NSJ", face = "bold")) 

# add difference column 

nov_av_dif <- nov_av %>%
  mutate(NJtmvbb = NJtm_av - BBav_av) %>%
  mutate(NJplvbb = NJpl_av - BBav_av) %>%
  mutate(RLtmvbb = RLtm_av - BBav_av) %>%
  mutate(NJtmvpl = NJtm_av - NJpl_av)

nov_av <- nov_av %>%
  mutate(NJtmvbb = NJtm_av - BBav_av) %>%
  mutate(NJplvbb = NJpl_av - BBav_av) %>%
  mutate(RLtmvbb = RLtm_av - BBav_av) %>%
  mutate(NJtmvpl = NJtm_av - NJpl_av)

# boxplot

# long format for plotting
nov_av_dif_long <- nov_av_dif %>%
  gather(key = "type", value = "value", NJtmvbb, NJplvbb, RLtmvbb)

# height as factor
nov_av_dif$height_av <- as.factor(nov_av_dif$height_av)

# plot
ggplot(nov_av_dif_long, aes(x = factor(exp, levels = c("shade", "dappled", "sun")), fill = exp)) +
  geom_boxplot(aes(y = value, color = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Microsite Categories", y = "Taxidermic Mount/3D Model - Blackbulb Operative Temperature (°C)", 
       fill = "Exposure Category", color = "Type") +
  scale_color_manual(values = c("NJtmvbb" = "#8D1F5C", "NJplvbb" = "#76CCCD", "RLtmvbb" = "#F05624"),
                     labels = c("NJtmvbb" = "Nightjar Taxidermic Mount", "NJplvbb" = "3D Printed Nightjar", "RLtmvbb" = "Red Lark Taxidermic Mount")) +
  facet_wrap(~ height_av, ncol = 2, nrow = 2, scales = "free_x") +
  theme_minimal() +
  theme(strip.text = element_text(color = "black", face = "bold", hjust = 0.5, size = 12, family = "NSJ")) +
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
  scale_fill_manual(values = c("shade" = "grey99", "dappled" = "grey99", "sun" = "grey99"), 
                    breaks = c("shade", "dappled", "sun")) +
  scale_y_continuous(limits = c(-20, 20))

# filter for height categories 

nov_av0 <- nov_av %>%
  filter(height_av == "0")

nov_av1 <- nov_av %>%
  filter(height_av == "1")

nov_av2 <- nov_av %>%
  filter(height_av == "2")

nov_av3 <- nov_av %>%
  filter(height_av == "3")

# filter for exposure categories

nov_av_sun <- nov_av %>%
  filter(exp == "sun")

nov_av_dap <- nov_av %>%
  filter(exp == "dappled")

nov_av_shade <- nov_av %>%
  filter(exp == "shade")

# filter for each microsite category

nov_av_sun0 <- nov_av %>%
  filter(cat == "sun0")
nov_av_sun1 <- nov_av %>%
  filter(cat == "sun1")
nov_av_sun2 <- nov_av %>%
  filter(cat == "sun2")
nov_av_sun3 <- nov_av %>%
  filter(cat == "sun3")

nov_av_dap0 <- nov_av %>%
  filter(cat == "dap0")
nov_av_dap1 <- nov_av %>%
  filter(cat == "dap1")
nov_av_dap2 <- nov_av %>%
  filter(cat == "dap2")
nov_av_dap3 <- nov_av %>%
  filter(cat == "dap3")

nov_av_shade0 <- nov_av %>%
  filter(cat == "shade0")
nov_av_shade1 <- nov_av %>%
  filter(cat == "shade1")
nov_av_shade2 <- nov_av %>%
  filter(cat == "shade2")
nov_av_shade3 <- nov_av %>%
  filter(cat == "shade3")


# lm model

# nightjar tm
summary(lm(BBav_av ~ NJtm_av, data = nov_av))
confint(lm(BBav_av ~ NJtm_av, data = nov_av))

summary(lm(BBav_av ~ NJtm_av, data = nov_av_shade3))
confint(lm(BBav_av ~ NJtm_av, data = nov_av_shade3))

# nightjar pl
summary(lm(BBav_av ~ NJpl_av, data = nov_av))
confint(lm(BBav_av ~ NJpl_av, data = nov_av))

summary(lm(BBav_av ~ NJpl_av, data = nov_av_shade3))
confint(lm(BBav_av ~ NJpl_av, data = nov_av_shade3))

#red lark tm
summary(lm(BBav_av ~ RLtm_av, data = nov_av))
confint(lm(BBav_av ~ RLtm_av, data = nov_av))

summary(lm(BBav_av ~ RLtm_av, data = nov_av_shade3))
confint(lm(BBav_av ~ RLtm_av, data = nov_av_shade3))

# anova

#nightjar tm
summary(aov(NJtmvbb ~ cat, data = nov_av_dif))
TukeyHSD(aov(NJtmvbb ~ cat, data = nov_av_dif))

summary(aov(NJtmvbb ~ exp, data = nov_av_dif))
TukeyHSD(aov(NJtmvbb ~ exp, data = nov_av_dif))

summary(aov(NJtmvbb ~ height_av, data = nov_av_dif))
TukeyHSD(aov(NJtmvbb ~ height_av, data = nov_av_dif))

#nightjar pl
summary(aov(NJplvbb ~ cat, data = nov_av_dif))
TukeyHSD(aov(NJplvbb ~ cat, data = nov_av_dif))

summary(aov(NJplvbb ~ exp, data = nov_av_dif))
TukeyHSD(aov(NJplvbb ~ exp, data = nov_av_dif))

summary(aov(NJplvbb ~ height_av, data = nov_av_dif))
TukeyHSD(aov(NJplvbb ~ height_av, data = nov_av_dif))

#red lark tm
summary(aov(RLtmvbb ~ cat, data = nov_av_dif))
TukeyHSD(aov(RLtmvbb ~ cat, data = nov_av_dif))

summary(aov(RLtmvbb ~ exp, data = nov_av_dif))
TukeyHSD(aov(RLtmvbb ~ exp, data = nov_av_dif))

summary(aov(RLtmvbb ~ height_av, data = nov_av_dif))
TukeyHSD(aov(RLtmvbb ~ height_av, data = nov_av_dif))


## nightjar 3D vs taxidermic mount

ggplot(nov_av, aes(x = NJtm_av, y = NJpl_av)) + 
  geom_point(color = "grey30", size = 1.5, alpha = 0.5) +  
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 0.6) +
  labs(x = "taxidermic mount operative temperature (°C)",
       y = "3D printed operative temperature (°C)") + 
  scale_x_continuous(limits = c(0, 45)) + 
  scale_y_continuous(limits = c(0, 45)) +
  theme_minimal() +  
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 11, family = "NSJ"),
        axis.title.x = element_text(vjust = 0, size = 15, family = "NSJ"),
        axis.title.y = element_text(vjust = 2, size = 15, family = "NSJ")) +
  facet_wrap(exp ~ height_av, ncol = 4, scales = "free_x") +
  #coord_fixed() +
  theme(strip.text = element_text(size = 12, family = "NSJ", face = "bold")) 

# lm
summary(lm(NJpl_av ~ NJtm_av, data = nov_av))
confint(lm(NJpl_av ~ NJtm_av, data = nov_av))

summary(lm(NJpl_av ~ NJtm_av, data = nov_av_shade3))
confint(lm(NJpl_av ~ NJtm_av, data = nov_av_shade3))


# boxplot
ggplot(nov_av, aes(x = factor(exp, levels = c("shade", "dappled", "sun")), y = NJtmvpl, fill = exp)) +
  geom_boxplot() +
  labs(x = "microsite categories", y = "taxidermic mount - 3D model operative temperature (°C)", 
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

# anova
nov_av_dif$height_av <- as.factor(nov_av_dif$height_av)

summary(aov(NJtmvpl ~ height_av, data = nov_av_dif))
TukeyHSD(aov(NJtmvpl ~ height_av, data = nov_av_dif))

# t test whether different to 0

t.test(nov_av_sun0$NJtmvpl)
t.test(nov_av_sun1$NJtmvpl)
t.test(nov_av_sun2$NJtmvpl)
t.test(nov_av_sun3$NJtmvpl)

t.test(nov_av_dap0$NJtmvpl)
t.test(nov_av_dap1$NJtmvpl)
t.test(nov_av_dap2$NJtmvpl)
t.test(nov_av_dap3$NJtmvpl)

t.test(nov_av_shade0$NJtmvpl)
t.test(nov_av_shade1$NJtmvpl)
t.test(nov_av_shade2$NJtmvpl)
t.test(nov_av_shade3$NJtmvpl)


# plot with novel and historic data

av$height_av <- as.numeric(as.character(av$height_av))
hist_and_nov <- bind_rows(av, nov_av)

ggplot(hist_and_nov) +
  geom_point(aes(x = NJtm_av, y = BBav_av, color = "nightjar taxidermic mount"), size = 1.1, alpha = 0.4) +  
  geom_smooth(aes(x = NJtm_av, y = BBav_av, color = "nightjar taxidermic mount"), method = "lm", se = F) + 
  geom_point(aes(x = NJpl_av, y = BBav_av, color = "3D printed nightjar"), size = 1.1, alpha = 0.4) +  
  geom_smooth(aes(x = NJpl_av, y = BBav_av, color = "3D printed nightjar"), method = "lm", se = F) + 
  geom_point(aes(x = RLtm_av, y = BBav_av, color = "red lark taxidermic mount"), size = 1.1, alpha = 0.4) +  
  geom_smooth(aes(x = RLtm_av, y = BBav_av, color = "red lark taxidermic mount"), method = "lm", se = F) +
  geom_point(aes(x = MT_av, y = BB_av, color = "hornbill taxidermic mount"), size = 1.1, alpha = 0.4) +  
  geom_smooth(aes(x = MT_av, y = BB_av, color = "hornbill taxidermic mount"), method = "lm", se = F) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 0.6) +
  labs(y = "blackbulb operative temperature (°C)", 
       x = "operative temperature (°C)", 
       color = "emperical model") +  # Set the legend title here
  scale_x_continuous(limits = c(0, 60)) + 
  scale_y_continuous(limits = c(0, 60)) +
  scale_color_manual(values = c("nightjar taxidermic mount" = "blue", "3D printed nightjar" = "green", "red lark taxidermic mount" = "red", "hornbill taxidermic mount" = "purple")) +
  theme_minimal() +  
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 11, family = "NSJ"),
        axis.title.x = element_text(vjust = 0, size = 15, family = "NSJ"),
        axis.title.y = element_text(vjust = 2, size = 15, family = "NSJ"),
        legend.title = element_text(size = 13, family = "NSJ"),  # Control the appearance of the legend title here
        legend.position = "bottom") +
  #facet_wrap( ~ exp, ncol = 4, scales = "free_x") +
  coord_fixed() +
  theme(strip.text = element_text(size = 12, family = "NSJ", face = "bold"))
