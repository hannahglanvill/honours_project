### novel ectotherm model ###

library(devtools)
library(RNCEP) # load the devtools package
library(NicheMapR)
library(jsonlite)
library(furrr)
library(rnoaa)
library(rgdal)
library(microclima)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyselect)
library(lubridate)

###microclimate

# model for different microclimate categories 

#load model
load('nov_sun1.Rda')
micro1 <- nov_sun1

# dataframes
metout1<-as.data.frame(micro1$metout) # above ground microclimatic conditions, min shade
shadmet1<-as.data.frame(micro1$shadmet) # above ground microclimatic conditions, max shade
soil1<-as.data.frame(micro1$soil) # soil temperatures, minimum shade
shadsoil1<-as.data.frame(micro1$shadsoil) # soil temperatures, maximum shade

# extract dates
dates1 <- micro1$dates
dates2_1 <- micro1$dates2

# convert dates to POSIXct with the current incorrect range
dates1 <- as.POSIXct(dates1, format="%Y-%m-%d %H:%M:%S", tz="Africa/Johannesburg")
dates2_1 <- as.POSIXct(dates2_1, format="%Y-%m-%d %H:%M:%S", tz="Africa/Johannesburg")

# check the current range of dates
range(dates1)

# join mod and dates
plotmetout1<-cbind(dates1, metout1)
plotsoil1<-cbind(dates1, soil1)
plotshadmet1<-cbind(dates1, shadmet1)
plotshadsoil1<-cbind(dates1, shadsoil1)

# extract shade values for plot labelling
minshade1<-micro1$minshade1
maxshade1<-micro1$maxshade1

# plot
plotmetout1$dates1 <- with_tz(plotmetout1$dates1, tzone = "Africa/Johannesburg")

plot(plotmetout1$dates1, plotmetout1$TALOC, pch = 19)


### ectotherm = model blackbulb

Ww_g <- 39.7 # weight (g) of blackbulb
shape <- 2 # shape (1 = cylinder, 2 = ellips) #sets rough shape
alpha_max <- 0.99 # maximum solar absorbtivity (dec %)
alpha_min <- 0.75 # minimum solar absorbtivity (dec %) > 0.7-0.8
k_flesh <- 401 #Based on the blackbulb, conductivity etc. will need to be set
postur <- 1 # postural postion to sun (1 = perpendicular, 2 = parallel, 0 = mix)

#load microclimate data as this will be used in the ectotherm model below (always keep the data named 'micro')
micro <- micro1 #output from example micro_ncep code

##Runs code for a non-living ectotherm approximating an egg##
ecto1 <- ectotherm(live = 0,
                   Ww_g = Ww_g,
                   shape = shape,
                   alpha_max = alpha_max,
                   alpha_min = alpha_min, 
                   k_flesh = k_flesh, 
                   pct_wet = 0,
                   pct_eyes = 0,
                   pct_mouth = 0,
                   pct_cond = 0,
                   pct_touch = 0, 
                   pct_H_P = 0,
                   pct_H_N = 0, 
                   pct_H_X = 0, 
                   K_skin = 0, 
                   postur = postur) 


# retrieve output
environ <- as.data.frame(ecto1$environ) #can change name for dataframes to species & min, max or average if volumes are variable
enbal <- as.data.frame(ecto1$enbal)
masbal <- as.data.frame(ecto1$masbal)
environ <- cbind(environ,metout1$SOLR) # min shade
colnames(environ)[ncol(environ)] <- "Solar"

# append mock dates
environ <- as.data.frame(cbind(dates1,environ))
masbal <- as.data.frame(cbind(dates1,masbal))
enbal <- as.data.frame(cbind(dates1,enbal))


### merge models and hist data

colnames(environ)[colnames(environ) == "dates1"] <- "datetime"
nov_sun1_env <- merge(environ, nov_av_sun1, by = "datetime", all = TRUE) 


### plot data and model timeline

ggplot(nov_sun1_env, aes(x = datetime)) +
  geom_line(aes(y = TA, color = "Air")) +
  geom_line(aes(y = TC, color = "model-blackbulb")) +
  geom_line(aes(y = BBav_av, color = "blackbulb")) +
  geom_line(aes(y = NJtm_av, color = "nightjar taxidermic mount")) + 
  geom_line(aes(y = NJpl_av, color = "3D printed nightjar")) + 
  geom_line(aes(y = RLtm_av, color = "red lark taxidermic mount")) + 
  scale_color_manual(values = c("Air" = "black", "model-blackbulb" = "orange", "blackbulb" = "purple", "nightjar taxidermic mount" = "blue", "3D printed nightjar" = "green", "red lark taxidermic mount" = "red")) +
  labs(x = "Date", y = "Temperature (°C)", title = "Operative temperature of hornbills in the Kalahari - February") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.1, "cm")) +
  theme(panel.grid.major = element_line(color = "darkgray", linetype = "dotted"),  
        plot.background = element_rect(fill = "white"),  
        panel.background = element_rect(fill = "white"),  
        legend.background = element_rect(fill = "white"))+
  scale_x_datetime(date_breaks = "2 day", date_labels = "%d-%m-%Y")


### save as csv

write.csv(nov_sun1_env, file = "D:/ectotherm_mod_nov/nov_sun1_env.csv")
write.csv(nov_shade1_env, file = "D:/ectotherm_mod_nov/nov_shade1_env.csv")

### linear models

summary(lm(BBav_av ~ TC, data = nov_shade3_env))
confint(lm(BBav_av ~ TC, data = nov_shade3_env))
