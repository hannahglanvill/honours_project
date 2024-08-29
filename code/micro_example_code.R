#Micro_ncep example code

install.packages("devtools")
install.packages("RNCEP") # load the devtools package
install.packages("jsonlite")
install.packages("furrr")

#istall rnoaa and rgdal from cran archive

#install_github('ilyamaclean/microclima', force = TRUE)

library(devtools)
library(RNCEP) # load the devtools package
library(NicheMapR)
library(jsonlite)
library(furrr)
library(rnoaa)
library(rgdal)
library(microclima)

loc <- c(21.824656, -26.970877) # set location - decimal degrees 'c(long, lat)' Kuruman River Reserve
#sun: (21.824656, -26.970877)
#dappled: (21.824656, -26.971276)
#shade: (21.824834, -26.971235)

# call the microclimate model, global climate database implementation
dstart <- "17/05/2015"
dfinish <- "27/05/2015"

# set depths and height to simulate
DEP <- c(0, 2.5, 5, 10, 15, 20, 30, 50, 100, 200) # depths to simulate (cm)
Usrhyt <- 3 # height of midpoint of animal/ blackbulb above ground (m)
Refhyt <- 3 # reference height for air temp, wind, humidity data calculated (m)

# additional variables
RUF <-  0.0003 # smooth desert

# run the microclimate model and extract some output for plotting
micro1 <- micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish, DEP = DEP, 
                     soilgrids = 1, runshade = 1, Usrhyt = Usrhyt, RUF = RUF, Refhyt = Refhyt)

# save the results for later
save(micro1, file = 'micro1.Rda')

load('micro1.Rda')

#Height 1#
metout1<-as.data.frame(micro1$metout) # above ground microclimatic conditions, min shade
shadmet1<-as.data.frame(micro1$shadmet) # above ground microclimatic conditions, max shade
soil1<-as.data.frame(micro1$soil) # soil temperatures, minimum shade
shadsoil1<-as.data.frame(micro1$shadsoil) # soil temperatures, maximum shade

# extract dates
dates1 <- micro1$dates
dates2_1<-micro1$dates2
plotmetout1<-cbind(dates1, metout1)
plotsoil1<-cbind(dates1, soil1)
plotshadmet1<-cbind(dates1, shadmet1)
plotshadsoil1<-cbind(dates1, shadsoil1)

# extract shade values for plot labelling
minshade1<-micro1$minshade1
maxshade1<-micro1$maxshade1

plot(plotmetout1$dates1, plotmetout1$TALOC, pch = 19)

