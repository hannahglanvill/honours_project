# endotherm model and historical data 


library(devtools)
library(RNCEP) 
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

#load model
load('micro_shade3_C3.Rda')
micro1 <- micro_shade3_c3


#Height 1
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


### endotherm model


#run endoR_devel function

#from micro
metout <- metout1
soil <- soil1
micro <- micro1

TAs <- metout$TALOC  # air temperatures at height of animal (°C)
TAREFs <- metout$TAREF  # air temperatures at reference height (°C)
TSKYs <- metout$TSKYC  # sky temperatures (°C)
TGRDs <- soil$D0cm  # surface temperatures (°C)
VELs <- metout$VLOC  # wind speeds at animal height (m/s)
RHs <- metout$RHLOC  # relative humidity at animal height (%)
QSOLRs <- metout$SOLR  # solar radiation (W/m2)
Zs <- metout$ZEN  # zenith angle of the sun (degrees)
ELEV <- micro$elev  # elevation (m)
ABSSB <- 1 - micro$REFL  # substrate solar absorptivity (%)

# core temperature
TC <- 39.4 # core temperature (deg C)
TC_MAX <- 44.8 # maximum core temperature (°C)
TC_INC <- 0.18 # increment by which TC is elevated (°C)

# size and shape
AMASS <- 0.199 # mass (kg)
SHAPE = 4
SHAPE_B = 2.8
SHAPE_B_MAX <- 2.8 # maximum ratio of length to width/depth, best estimate 
SAMODE<- 1
UNCURL <- 0.1
ANDENS = 500
PVEN = 0.4

# fur/feather properties
DHAIRD = 30E-06 # hair diameter, dorsal (m), estimate 
DHAIRV = 30E-06 # hair diameter, ventral (m), estiamte 
LHAIRD = 0.025 # hair length, dorsal (m)
LHAIRV = 0.0169 # hair length, ventral (m)
ZFURD = 0.0111 # fur depth, dorsal (m)
ZFURV = 0.00596 # fur depth, ventral (m)
RHOD = 15000000 # hair density, dorsal (1/m2), estimate 
RHOV = 15000000 # hair density, ventral (1/m2), estimate 
REFLD = 0.248  # fur reflectivity dorsal (fractional, 0-1), estimate 
REFLV = 0.351  # fur reflectivity ventral (fractional, 0-1), estimate 

# physiological responses
TS = TC - 3 # skin temperature (°C)
PCTBAREVAP = 0.5 # surface area for evaporation that is skin, e.g. licking paws (%)
PCTWET <- 1.8 # base skin wetness (%), estimate
PCTWET_MAX <- 50 # maximum skin wetness (%), estimate
PCTWET_INC <- 10 # intervals by which skin wetness is increased (%)
Q10 <- rep(1, length(TAs)) # Q10 effect of body temperature on metabolic rate
Q10 <- 2.5 #estimate
QBASAL <- 1 #10 ^ (-1.461 + 0.669 * log10(AMASS * 1000)) # basal heat generation (W) (bird formula from McKechnie and Wolf 2004 Phys. & Biochem. Zool. 77:502-521)
DELTAR <- 5 # offset between air temeprature and breath (°C)
EXTREF <- 22 # O2 extraction efficiency (%), estimate
Pant<- 1
PANT_INC <- 0.14 # turns on panting, the value being the increment by which the panting multiplier is increased up to the maximum value, PANTMAX
PANT_MAX <- 5 # maximum panting rate - multiplier on air flow through the lungs above that determined by metabolic rate
PANT_MULT <- 1

#other
ZFUR_INC = 0.001
AK1_INC = 0.2 #turns on thermal conductivity increase (W/mK), the value being the increment by which AK1 is increased per iteration (W/mC)
AK1_MAX = 2.8 #maximum flesh conductivity (W/mK)
AK1 = 0.412
PCOND = 0
THERMOREG <- 1
RESPIRE <- 1
#TREGMODE <- 0

# run the model
ptm <- proc.time()  # start timing
endo.out <- lapply(1:length(TAs), function(x){
        endoR_devel(TA = TAs[x], TAREF = TAREFs[x], TSKY = TSKYs[x], TGRD = TGRDs[x],
        VEL = VELs[x], RH = RHs[x], QSOLR = QSOLRs[x], Z = Zs[x],
        ELEV = ELEV, ABSSB = ABSSB, TC = TC, TC_MAX = TC_MAX,
        AMASS = AMASS, SHAPE = SHAPE, SHAPE_B = SHAPE_B, SHAPE_B_MAX = SHAPE_B_MAX,
        PCTWET = PCTWET, PCTWET_INC = PCTWET_INC, Q10 = Q10,
        QBASAL = QBASAL, DELTAR = DELTAR, DHAIRD = DHAIRD, DHAIRV = DHAIRV,
        LHAIRD = LHAIRD, LHAIRV = LHAIRV, ZFURD = ZFURD, ZFURV = ZFURV,
        RHOD = RHOD, RHOV = RHOV, REFLD = REFLD, TC_INC = TC_INC,
        PANT_INC = PANT_INC, PANT_MAX = PANT_MAX, EXTREF = EXTREF,
        UNCURL = UNCURL, SAMODE = SAMODE, SHADE = 0, PANT_MULT = PANT_MULT, 
        THERMOREG = THERMOREG, RESPIRE = RESPIRE)})
proc.time() - ptm  # end timing

# turn results into data frame
endo.out <- do.call("rbind", lapply(endo.out, data.frame)) 

treg <- endo.out[, grep(pattern = "treg", colnames(endo.out))]
colnames(treg) <- gsub(colnames(treg), pattern = "treg.", replacement = "")
morph <- endo.out[, grep(pattern = "morph", colnames(endo.out))]
colnames(morph) <- gsub(colnames(morph), pattern = "morph.", replacement = "")
enbal <- endo.out[, grep(pattern = "enbal", colnames(endo.out))]
colnames(enbal) <- gsub(colnames(enbal), pattern = "enbal.", replacement = "")
masbal <- endo.out[, grep(pattern = "masbal", colnames(endo.out))]
colnames(masbal) <- gsub(colnames(masbal), pattern = "masbal.", replacement = "")

#Chamber outputs
TAs_cham <- TAs
QGEN_cham <- enbal$QGEN # metabolic rate (W)
H2O_cham <- masbal$H2OResp_g + masbal$H2OCut_g # g/h water evaporated
TFA_D_cham <- treg$TFA_D # dorsal fur surface temperature
TFA_V_cham <- treg$TFA_V # ventral fur surface temperature
TskinD_cham <- treg$TSKIN_D # dorsal skin temperature
TskinV_cham <- treg$TSKIN_V # ventral skin temperature
TCs_cham <- treg$TC # core temperature


### merge models and hist data

#change endo 'TC' to 'TMmod'
colnames(treg)[colnames(treg) == "TC"] <- "TM_live"

#sort endo model
colnames(plotmetout1)[colnames(plotmetout1) == "dates1"] <- "datetime"
colnames(treg)[-1] <- paste0(colnames(treg)[-1], ".env")
micro_endo <- cbind(plotmetout1, treg)

#merge
shade3_c3_live <- merge(shade3_c3_env, micro_endo, by = "datetime", all = TRUE) 

shade3_c3_live_fm <- shade3_c3_live %>%
  mutate(month = month(datetime),
         day = day(datetime)) %>%
  filter((month == 2 & day >= 15 & day <= 28) | (month == 5 & day >= 15 & day <= 28))

### plot data and lm stats

# Plot for February ==2 and May ==5
ggplot(shade3_c3_live_fm %>% filter(month == 5), aes(x = datetime)) +
  geom_line(aes(y = TA, color = "Air")) +
  geom_line(aes(y = TC, color = "ectotherm model")) +
  geom_line(aes(y = BB_av, color = "blackbulb")) +
  geom_line(aes(y = MT_av, color = "taxidermic mount")) +
  geom_line(aes(y = TM_live, color = "endotherm model - body temperature")) +
  geom_line(aes(y = TSKIN_D.env, color = "endotherm model - dorsal skin temperature")) +
  geom_line(aes(y = TSKIN_V.env, color = "endotherm model - ventral skin temperature")) +
  scale_color_manual(values = c("Air" = "black", "ectotherm model" = "orange", "blackbulb" = "red", 
                                "taxidermic mount" = "blue", "endotherm model - body temperature" = "purple", 
                                "endotherm model - dorsal skin temperature" = "darkgreen", "endotherm model - ventral skin temperature" = "darkblue")) +
  labs(x = "Date", y = "Temperature (°C)") +
  theme_minimal() +
  theme(strip.text = element_text(color = "black",
                                  hjust = 0.5, size = 12, family = "Playfair")) +
  theme(legend.position = "right",
        legend.key.size = unit(0.5, "cm"), 
        legend.title = element_text(family = "Playfair",
                                    color = "black",
                                    size = 10)) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 11, family = "Playfair"),
        axis.title.x = element_text(vjust = 0, size = 15, family = "Playfair"),
        axis.title.y = element_text(vjust = 2, size = 15, family = "Playfair")) +
  guides(color = guide_legend(override.aes = list(size = 6))) +
  scale_y_continuous(limits = c(0, 60)) + 
  scale_x_datetime(date_breaks = "2 day", date_labels = "%d-%m-%Y")


### save as csv

write.csv(shade3_c1_live_fm, file = "D:/endotherm_live_mod_hist/shade3_c1_live_fm.csv")
write.csv(shade3_c2_live_fm, file = "D:/endotherm_live_mod_hist/shade3_c2_live_fm.csv")
write.csv(shade3_c3_live_fm, file = "D:/endotherm_live_mod_hist/shade3_c3_live_fm.csv")


