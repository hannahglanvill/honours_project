### novel data microclimate models ###

library(devtools)
library(RNCEP) # load the devtools package
library(NicheMapR)
library(jsonlite)
library(furrr)
library(rnoaa)
library(rgdal)
library(microclima)

# call the microclimate model, global climate database implementation
dstart <- "01/06/2024"
dfinish <- "03/08/2024"

# set depths and height to simulate
DEP <- c(0, 2.5, 5, 10, 15, 20, 30, 50, 100, 200) # depths to simulate (cm)

# additional variables
RUF <-  0.001 # closely mowed grass


## sun
# co-ord: 33.965223°S 18.484317°E ~ (18.484317, -33.965223)

# sun 0
nov_sun0 <- micro_ncep(loc = c(18.484317, -33.965223), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 0, runshade = 1, Usrhyt = 0.0003, RUF = RUF, Refhyt = 0.0003)
save(nov_sun0, file = 'nov_sun0.Rda')

# sun 1
nov_sun1 <- micro_ncep(loc = c(18.484317, -33.965223), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 0, runshade = 1, Usrhyt = 1, RUF = RUF, Refhyt = 1)
save(nov_sun1, file = 'nov_sun1.Rda')

# sun 2
nov_sun2 <- micro_ncep(loc = c(18.484317, -33.965223), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 0, runshade = 1, Usrhyt = 2, RUF = RUF, Refhyt = 2)
save(nov_sun2, file = 'nov_sun2.Rda')

# sun 3
nov_sun3 <- micro_ncep(loc = c(18.484317, -33.965223), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 0, runshade = 1, Usrhyt = 3, RUF = RUF, Refhyt = 3)
save(nov_sun3, file = 'nov_sun3.Rda')




## shade 
#co-ord: 33.965016°S 18.484247°E ~ (18.484247, -33.965016)

# shade 0
nov_shade0 <- micro_ncep(loc = c(18.484247, -33.965016), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                              soilgrids = 0, runshade = 1, Usrhyt = 0.0003, RUF = RUF, Refhyt = 0.0003)
save(nov_shade0, file = 'nov_shade0.Rda')

# shade 1
nov_shade1 <- micro_ncep(loc = c(18.484247, -33.965016), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                              soilgrids = 0, runshade = 1, Usrhyt = 1, RUF = RUF, Refhyt = 1)
save(nov_shade1, file = 'nov_shade1.Rda')

# shade 2
nov_shade2 <- micro_ncep(loc = c(18.484247, -33.965016), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                              soilgrids = 0, runshade = 1, Usrhyt = 2, RUF = RUF, Refhyt = 2)
save(nov_shade2, file = 'nov_shade2.Rda')

# shade 3
nov_shade3 <- micro_ncep(loc = c(18.484247, -33.965016), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                              soilgrids = 0, runshade = 1, Usrhyt = 3, RUF = RUF, Refhyt = 3)
save(nov_shade3, file = 'nov_shade3.Rda')



## dappled 
#co-ord: 33.965347°S 18.484230°E ~ (18.484230, -33.965347)

# dappled 0
nov_dap0 <- micro_ncep(loc = c(18.484230, -33.965347), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 0, runshade = 1, Usrhyt = 0.0003, RUF = RUF, Refhyt = 0.0003)
save(nov_dap0, file = 'nov_dap0.Rda')

# dappled 1
nov_dap1 <- micro_ncep(loc = c(18.484230, -33.965347), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 0, runshade = 1, Usrhyt = 1, RUF = RUF, Refhyt = 1)
save(nov_dap1, file = 'nov_dap1.Rda')

# dappled 2
nov_dap2 <- micro_ncep(loc = c(18.484230, -33.965347), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 0, runshade = 1, Usrhyt = 2, RUF = RUF, Refhyt = 2)
save(nov_dap2, file = 'nov_dap2.Rda')

# dappled 3
nov_dap3 <- micro_ncep(loc = c(18.484230, -33.965347), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 0, runshade = 1, Usrhyt = 3, RUF = RUF, Refhyt = 3)
save(nov_dap3, file = 'nov_dap3.Rda')

