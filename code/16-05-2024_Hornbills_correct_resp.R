###########
library(NicheMapR)
# environment
TAs <- seq(0, 52, 5) # air temperatures (°C)
VEL <- 0.0008 # wind speed (m/s), need to double check. Currently based on best estimate.
RH <- 5 # relative humidity (%)
SOL <- 0 # solar radiation (W/m2), estimate 

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
Q10s <- rep(1, length(TAs)) # Q10 effect of body temperature on metabolic rate
Q10s <- 2.5 #estimate
QBASAL <- 1 #10 ^ (-1.461 + 0.669 * log10(AMASS * 1000)) # basal heat generation (W) (bird formula from McKechnie and Wolf 2004 Phys. & Biochem. Zool. 77:502-521)
DELTAR <- 5 # offset between air temeprature and breath (°C)
EXTREF <- 22 # O2 extraction efficiency (%), estimate
Pant<- 1
PANT_INC <- 0.14 # turns on panting, the value being the increment by which the panting multiplier is increased up to the maximum value, PANTMAX
PANT_MAX <- 5 # maximum panting rate - multiplier on air flow through the lungs above that determined by metabolic rate
PANT_MULT <- 1

ZFUR_INC = 0.001
AK1_INC = 0.2 #turns on thermal conductivity increase (W/mK), the value being the increment by which AK1 is increased per iteration (W/mC)
AK1_MAX = 2.8 #maximum flesh conductivity (W/mK)
AK1 = 0.412
PCOND = 0

ptm <- proc.time() # start timing
endo.out <- lapply(1:length(TAs), 
                   function(x){endoR_devel(TA = TAs[x], VEL = VEL,
                                           TC = TC, TC_MAX = TC_MAX, RH = RH, 
                                           AMASS = AMASS, ANDENS = ANDENS, SHAPE_B = SHAPE_B, PVEN = PVEN,
                                           SHAPE_B_MAX = SHAPE_B_MAX, SHAPE = SHAPE,
                                           PCTWET = PCTWET, PCTWET_INC = PCTWET_INC, SAMODE = SAMODE,
                                           PCTWET_MAX = PCTWET_MAX, Q10 = Q10s, AK1_INC = AK1_INC, AK1_MAX = AK1_MAX, AK1 = AK1,
                                           QBASAL = QBASAL, DELTAR = DELTAR, PCOND = PCOND,
                                           DHAIRD = DHAIRD, DHAIRV = DHAIRV, 
                                           LHAIRD = LHAIRD, LHAIRV = LHAIRV, 
                                           ZFURD = ZFURD, ZFURV = ZFURV, 
                                           RHOD = RHOD, RHOV = RHOV,
                                           REFLD = REFLD, TC_INC = TC_INC, 
                                           PANT_INC = PANT_INC, PANT = Pant, PANT_MULT = PANT_MULT,
                                           PANT_MAX = PANT_MAX, EXTREF = EXTREF)}) # run endoR across environments
proc.time() - ptm # stop timing

endo.out <- do.call("rbind", lapply(endo.out, data.frame)) # turn results into data frame
endo.out

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

