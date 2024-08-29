### historic data microclimate models ###

library(devtools)
library(RNCEP) # load the devtools package
library(NicheMapR)
library(jsonlite)
library(furrr)
library(rnoaa)
library(rgdal)
library(microclima)

# call the microclimate model, global climate database implementation
dstart <- "01/02/2015"
dfinish <- "01/06/2015"

# set depths and height to simulate
DEP <- c(0, 2.5, 5, 10, 15, 20, 30, 50, 100, 200) # depths to simulate (cm)

# additional variables
RUF <-  0.0003 # smooth desert


# microclimate category code for 3 co-ordinates per exposure category

## sun co-ord 1: 26.970877°S 21.824656°E
# sun 0
micro_sun0_c1 <- micro_ncep(loc = c(21.824656, -26.970877), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                     soilgrids = 1, runshade = 1, Usrhyt = 0.0003, RUF = RUF, Refhyt = 0.0003)
save(micro_sun0_c1, file = 'micro_sun0_c1.Rda')

# sun 1
micro_sun1_c1 <- micro_ncep(loc = c(21.824656, -26.970877), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                         soilgrids = 1, runshade = 1, Usrhyt = 1, RUF = RUF, Refhyt = 1)
save(micro_sun1_c1, file = 'micro_sun1_c1.Rda')

# sun 2
micro_sun2_c1 <- micro_ncep(loc = c(21.824656, -26.970877), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                         soilgrids = 1, runshade = 1, Usrhyt = 2, RUF = RUF, Refhyt = 2)
save(micro_sun2_c1, file = 'micro_sun2_c1.Rda')

# sun 3
micro_sun3_c1 <- micro_ncep(loc = c(21.824656, -26.970877), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                         soilgrids = 1, runshade = 1, Usrhyt = 3, RUF = RUF, Refhyt = 3)
save(micro_sun3_c1, file = 'micro_sun3_c1.Rda')


##sun co-ord 2: 26.970918°S 21.823561°E
# sun 0
micro_sun0_c2 <- micro_ncep(loc = c(21.823561, -26.970918), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 0.0003, RUF = RUF, Refhyt = 0.0003)
save(micro_sun0_c2, file = 'micro_sun0_c2.Rda')

# sun 1
micro_sun1_c2 <- micro_ncep(loc = c(21.823561, -26.970918), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 1, RUF = RUF, Refhyt = 1)
save(micro_sun1_c2, file = 'micro_sun1_c2.Rda')

# sun 2
micro_sun2_c2 <- micro_ncep(loc = c(21.823561, -26.970918), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 2, RUF = RUF, Refhyt = 2)
save(micro_sun2_c2, file = 'micro_sun2_c2.Rda')

# sun 3
micro_sun3_c2 <- micro_ncep(loc = c(21.823561, -26.970918), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 3, RUF = RUF, Refhyt = 3)
save(micro_sun3_c2, file = 'micro_sun3_c2.Rda')


## sun co-ord 3: 26.971956°S 21.823894°E
# sun 0
micro_sun0_c3 <- micro_ncep(loc = c(21.823894, -26.971956), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 0.0003, RUF = RUF, Refhyt = 0.0003)
save(micro_sun0_c3, file = 'micro_sun0_c3.Rda')

# sun 1
micro_sun1_c3 <- micro_ncep(loc = c(21.823894, -26.971956), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 1, RUF = RUF, Refhyt = 1)
save(micro_sun1_c3, file = 'micro_sun1_c3.Rda')

# sun 2
micro_sun2_c3 <- micro_ncep(loc = c(21.823894, -26.971956), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 2, RUF = RUF, Refhyt = 2)
save(micro_sun2_c3, file = 'micro_sun2_c3.Rda')

# sun 3
micro_sun3_c3 <- micro_ncep(loc = c(21.823894, -26.971956), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 3, RUF = RUF, Refhyt = 3)
save(micro_sun3_c3, file = 'micro_sun3_c3.Rda')


## shade co-ord 1: 26.971235°S 21.824834°E
# shade 0
micro_shade0_c1 <- micro_ncep(loc = c(21.824834, -26.971235), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                         soilgrids = 1, runshade = 1, Usrhyt = 0.0003, RUF = RUF, Refhyt = 0.0003)
save(micro_shade0_c1, file = 'micro_shade0_c1.Rda')

# shade 1
micro_shade1_c1 <- micro_ncep(loc = c(21.824834, -26.971235), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                           soilgrids = 1, runshade = 1, Usrhyt = 1, RUF = RUF, Refhyt = 1)
save(micro_shade1_c1, file = 'micro_shade1_c1.Rda')

# shade 2
micro_shade2_c1 <- micro_ncep(loc = c(21.824834, -26.971235), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                           soilgrids = 1, runshade = 1, Usrhyt = 2, RUF = RUF, Refhyt = 2)
save(micro_shade2_c1, file = 'micro_shade2_c1.Rda')

# shade 3
micro_shade3_c1 <- micro_ncep(loc = c(21.824834, -26.971235), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                           soilgrids = 1, runshade = 1, Usrhyt = 3, RUF = RUF, Refhyt = 3)
save(micro_shade3_c1, file = 'micro_shade3_c1.Rda')


## shade co-ord 2: 26.971745°S 21.825191°E
# shade 0
micro_shade0_c2 <- micro_ncep(loc = c(21.825191, -26.971745), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                              soilgrids = 1, runshade = 1, Usrhyt = 0.0003, RUF = RUF, Refhyt = 0.0003)
save(micro_shade0_c2, file = 'micro_shade0_c2.Rda')

# shade 1
micro_shade1_c2 <- micro_ncep(loc = c(21.825191, -26.971745), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                              soilgrids = 1, runshade = 1, Usrhyt = 1, RUF = RUF, Refhyt = 1)
save(micro_shade1_c2, file = 'micro_shade1_c2.Rda')

# shade 2
micro_shade2_c2 <- micro_ncep(loc = c(21.825191, -26.971745), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                              soilgrids = 1, runshade = 1, Usrhyt = 2, RUF = RUF, Refhyt = 2)
save(micro_shade2_c2, file = 'micro_shade2_c2.Rda')

# shade 3
micro_shade3_c2 <- micro_ncep(loc = c(21.825191, -26.971745), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                              soilgrids = 1, runshade = 1, Usrhyt = 3, RUF = RUF, Refhyt = 3)
save(micro_shade3_c2, file = 'micro_shade3_c2.Rda')


## shade co-ord 3: 26.972146°S 21.824656°E
# shade 0
micro_shade0_c3 <- micro_ncep(loc = c(21.824656, -26.972146), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                              soilgrids = 1, runshade = 1, Usrhyt = 0.0003, RUF = RUF, Refhyt = 0.0003)
save(micro_shade0_c3, file = 'micro_shade0_c3.Rda')

# shade 1
micro_shade1_c3 <- micro_ncep(loc = c(21.824656, -26.972146), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                              soilgrids = 1, runshade = 1, Usrhyt = 1, RUF = RUF, Refhyt = 1)
save(micro_shade1_c3, file = 'micro_shade1_c3.Rda')

# shade 2
micro_shade2_c3 <- micro_ncep(loc = c(21.824656, -26.972146), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                              soilgrids = 1, runshade = 1, Usrhyt = 2, RUF = RUF, Refhyt = 2)
save(micro_shade2_c3, file = 'micro_shade2_c3.Rda')

# shade 3
micro_shade3_c3 <- micro_ncep(loc = c(21.824656, -26.972146), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                              soilgrids = 1, runshade = 1, Usrhyt = 3, RUF = RUF, Refhyt = 3)
save(micro_shade3_c3, file = 'micro_shade3_c3.Rda')



## dappled co-ord 1: 26.971026°S 21.824970°E
# dappled 0
micro_dap0_c1 <- micro_ncep(loc = c(21.824970, -26.971026), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                           soilgrids = 1, runshade = 1, Usrhyt = 0.0003, RUF = RUF, Refhyt = 0.0003)
save(micro_dap0_c1, file = 'micro_dap0_c1.Rda')

# dappled 1
micro_dap1_c1 <- micro_ncep(loc = c(21.824970, -26.971026), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                         soilgrids = 1, runshade = 1, Usrhyt = 1, RUF = RUF, Refhyt = 1)
save(micro_dap1_c1, file = 'micro_dap1_c1.Rda')

# dappled 2
micro_dap2_c1 <- micro_ncep(loc = c(21.824970, -26.971026), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                         soilgrids = 1, runshade = 1, Usrhyt = 2, RUF = RUF, Refhyt = 2)
save(micro_dap2_c1, file = 'micro_dap2_c1.Rda')

# dappled 3
micro_dap3_c1 <- micro_ncep(loc = c(21.824970, -26.971026), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                         soilgrids = 1, runshade = 1, Usrhyt = 3, RUF = RUF, Refhyt = 3)
save(micro_dap3_c1, file = 'micro_dap3_c1.Rda')


## dappled co-ord 2: 26.972084°S 21.825111°E
# dappled 0
micro_dap0_c2 <- micro_ncep(loc = c(21.825111, -26.972084), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 0.0003, RUF = RUF, Refhyt = 0.0003)
save(micro_dap0_c2, file = 'micro_dap0_c2.Rda')

# dappled 1
micro_dap1_c2 <- micro_ncep(loc = c(21.825111, -26.972084), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 1, RUF = RUF, Refhyt = 1)
save(micro_dap1_c2, file = 'micro_dap1_c2.Rda')

# dappled 2
micro_dap2_c2 <- micro_ncep(loc = c(21.825111, -26.972084), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 2, RUF = RUF, Refhyt = 2)
save(micro_dap2_c2, file = 'micro_dap2_c2.Rda')

# dappled 3
micro_dap3_c2 <- micro_ncep(loc = c(21.825111, -26.972084), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 3, RUF = RUF, Refhyt = 3)
save(micro_dap3_c2, file = 'micro_dap3_c2.Rda')


## dappled co-ord 3: 26.972387°S 21.824147°E
# dappled 0
micro_dap0_c3 <- micro_ncep(loc = c(21.824147, -26.972387), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 0.0003, RUF = RUF, Refhyt = 0.0003)
save(micro_dap0_c3, file = 'micro_dap0_c3.Rda')

# dappled 1
micro_dap1_c3 <- micro_ncep(loc = c(21.824147, -26.972387), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 1, RUF = RUF, Refhyt = 1)
save(micro_dap1_c3, file = 'micro_dap1_c3.Rda')

# dappled 2
micro_dap2_c3 <- micro_ncep(loc = c(21.824147, -26.972387), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 2, RUF = RUF, Refhyt = 2)
save(micro_dap2_c3, file = 'micro_dap2_c3.Rda')

# dappled 3
micro_dap3_c3 <- micro_ncep(loc = c(21.824147, -26.972387), dstart = dstart, dfinish = dfinish, DEP = DEP, 
                            soilgrids = 1, runshade = 1, Usrhyt = 3, RUF = RUF, Refhyt = 3)
save(micro_dap3_c3, file = 'micro_dap3_c3.Rda')


