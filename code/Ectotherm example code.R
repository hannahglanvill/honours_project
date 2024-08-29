##############################
###### Blackbulb EG #######
##############################
library(NicheMapR)

Ww_g <- 19.2 # weight (g) of blackbulb
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

#Plot data

# sun
plot(environ$TC ~ environ$dates, type = "l", xlab = "2015", 
     ylab = "Temperature (°C)", main = "Operative temperature of hornbills in the Kalahari", ylim = c(0, 50))
with(environ, {points(environ$TA ~ environ$dates, type = "l", col = "blue")})
with(av, {points(av$BB_av ~ av$datetime, type = "l", col = "green")})
with(av, {points(av$MT_av ~ av$datetime, type = "l", col = "purple")})
legend(x= "bottomleft", y= 75,c("model-blackbulb","Air", "blackbulb", "taxidermic mount"),cex=.5,col=c("black","blue", "green", "purple"),pch=c(18,18))
