#test_file(paste(getwd(),"/tests/testthat/test_over_STF.R", sep=""))
#===============================================================================
#
# JUST RUN THIS TEST FILE
#
#===============================================================================

#require(trajaggr)
#require(testthat)
#test_file(paste(getwd(),"/tests/testthat/test_over_STF.R", sep=""))

#-------------------------------------------------------------------------------

#require(sp)
#require(spacetime) # STIDF()
#require(trajectories) # Track()
#
#require(xts)
# 
# # ? welche daten nutzen ???
# ## !!!!!
# ## # Load example data (for direct development)
# #load("tests/testthat/enviroCar_TestData.RData")
# # !!!
# load("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/trajaggr/tests/testthat/enviroCar_TestData.RData", verbose = TRUE)
# #load("tests/testthat/trajectories_exampleTrack_Data_original.RData")
# # !!!!!!!!
#load("trajectories_exampleTrack_Data_original.RData", verbose = TRUE)
#load(paste(getwd(), "/tests/testthat/" , "trajectories_exampleTrack_Data_original.RData", sep = ""), verbose = TRUE)

# !!! Folgendes geht immer (von auf meienm Rehcner!) (testthat oder test_file)!!!

#load("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/trajaggr/tests/testthat/trajectories_exampleTrack_Data_original.RData", verbose = TRUE)
#load("trajectories_exampleTrack_Data_original.RData", verbose = TRUE)
#load("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/trajaggr/tests/testthat/enviroCar_TestData.RData", verbose = TRUE)
#load("enviroCar_TestData.RData", verbose = TRUE)

source("createData_overAndAggTests.R")

#load("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/trajaggr/tests/testthat/trajaggr_TestData.RData", verbose = TRUE)
#load("trajaggr_TestData.RData", verbose = TRUE)

#source("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/movebank_Gyps_ExDataPreparation.R")
#data(vulture_moveStack)

# 
# ## # !!! #
# ## load(paste(getwd(), "/tests/testthat/", "adehabitatLT_ExData.RData", sep=""), verbose = TRUE)
# 
# @load data: 3Möglichkeiten oder mehr ???
# - absoluter Pfad zu RData --> geht nur auf meinem Rechner, geht aber in beiden folgendne Fällen
# - relativer Pfad zu Rdata in tests von wd aus --> wenn ich test_file einzeln aufrufen möchte
# - relativer Pfad zu Rdata in tests von /test/testthat aus --> wenn ich test pkg nutze
# - data nach /data --> imer verfügbar ?!
# ###############################
# 

# # # Preparation
# a1_spdf <- as(A1, "SpatialPointsDataFrame")
# a1_spP <- as(a1_spdf, "SpatialPoints")
# a_spdf <- as(A, "SpatialPointsDataFrame")
# envT <- enviroCar_TracksObj1@tracks[[1]]; class(envT)
# env1_spdf <- as(enviroCar_TracksObj1@tracks[[1]], "SpatialPointsDataFrame")
# #plot(env1_spdf)
# tr_spdf <- as(Tr, "SpatialPointsDataFrame")
# 
# spGridTr <- createSpatialArealObjFromPoints(tr_spdf, desDim=3, out = "SpatialGrid")
# spGridA <- createSpatialArealObjFromPoints(a_spdf, desDim=3, out = "SpatialGrid")
# spGridA1 <- createSpatialArealObjFromPoints(a1_spdf, desDim=3, out = "SpatialGrid")
# spPixA1 <- createSpatialArealObjFromPoints(a1_spdf, desDim=3, out = "SpatialPixels")
# spPolygTr <- createSpatialArealObjFromPoints(tr_spdf, desDim=3, out = "SpatialPolygons")
# spPolygA <- createSpatialArealObjFromPoints(a_spdf, desDim=3, out = "SpatialPolygons")
# spPolygA1 <- createSpatialArealObjFromPoints(a1_spdf, desDim=3, out = "SpatialPolygons")
# spPolygEnv1 <- createSpatialArealObjFromPoints(env1_spdf, desDim=7, out = "SpatialPolygons")
# spPolygEnv3x3 <- createSpatialArealObjFromPoints(env1_spdf, desDim=3, out = "SpatialPolygons")
# 
# # require(raster)
# # rasterLayerEnv1 <- raster(env1_spdf,ncols=7,nrows=7)
# # rasterLayerEnv3x3 <- raster(env1_spdf,ncols=3,nrows=3)
# # rasterLayerTr <- raster(tr_spdf,ncols=3,nrows=3)
# # rasterLayerA <- raster(a_spdf,ncols=3,nrows=3)
# # rasterLayerA1 <- raster(a1_spdf,ncols=3,nrows=3)
# # # rasterLayerA_20 <- raster(a_spdf,ncols=20,nrows=20)
# # spGridTr <- as(rasterLayerTr, "SpatialGrid") # SpatialGrid
# # spGridA <- as(rasterLayerA, "SpatialGrid") # SpatialGrid
# # spGridA1 <- as(rasterLayerA1, "SpatialGrid") # SpatialGrid
# # #fullgrid(spGridA1)#TRUE --> !?!
# # #getGridTopology(spGridA1)
# # # spPixA <- as(rasterLayerA, "SpatialPixels") # # SpatialPixels
# # spPixA1 <- as(rasterLayerA1, "SpatialPixels") # # SpatialPixels
# # #fullgrid(spPixA1)#FALSE --> ?!? ???
# # #getGridTopology(spPixA1)
# # # spPixA_20 <- as(rasterLayerA_20, "SpatialPixels") # # SpatialPixels
# # spPolygTr <- as(rasterLayerTr, "SpatialPolygons")
# # spPolygA <- as(rasterLayerA, "SpatialPolygons")
# # spPolygA1 <- as(rasterLayerA1, "SpatialPolygons")
# # spPolygEnv1 <- as(rasterLayerEnv1, "SpatialPolygons")
# # spPolygEnv3x3 <- as(rasterLayerEnv3x3, "SpatialPolygons")
# # #plot(spPolygEnv1)
# # # #index(Tr@tracksCollection[[1]]@tracks[[1]]@time[1])
# # # 
# 
# 
# polygL <- length(spPolygA1)
# p_df <- data.frame(test = round(runif(polygL) * 3))
# spPolygA1_df <- SpatialPolygonsDataFrame(spPolygA1, p_df, match.ID = FALSE)
# 
# A1_test <- A1
# A1_test@data$test <- round(runif(length(A1)) * 3)
# A1_test@data <- A1_test@data[2]
# 
# A1_ext <- A1
# A1_ext@data$test <- round(runif(length(A1)) * 3)
# A2_ext <- A2
# A2_ext@data$test <- round(runif(length(A2)) * 3)
# 
# A1_ext2 <- A1_ext
# A1_ext2@data$char <- letters[1:nrow(A1@data)]
# A1_ext2@data$fac <- as.factor(letters[1:nrow(A1@data)])
# A1_ext3 <- A1_ext2
# A1_ext3@data$timetest <- zoo::index(A1@time)
# #class(index(A1@time))
# #str(A1_ext2@data)
# 
# A2_ext2 <- A2_ext
# A2_ext2@data$char <- letters[1:nrow(A2@data)]
# A2_ext2@data$fac <- as.factor(letters[1:nrow(A2@data)])
# 
# A1_noD <- A1
# A1_noD@data <- A1_noD@data[0, , drop=FALSE]
# A1_noDnoC <- A1_noD
# A1_noDnoC@data <- A1_noD@data[0, 0, drop=FALSE]
# A1_noC <- A1
# A1_noC@data <- A1_noC@data[ , FALSE, drop=FALSE]
# 
# A2_noD <- A2
# A2_noD@data <- A2_noD@data[0, , drop=FALSE]
# A2_noDnoC <- A2_noD
# A2_noDnoC@data <- A2_noD@data[0, 0, drop=FALSE]
# A2_noC <- A2
# A2_noC@data <- A2_noC@data[ , FALSE, drop=FALSE]
# 
# 
# A_ext <- trajectories::Tracks(list(A1_ext, A2_ext))
# A_ext2 <- trajectories::Tracks(list(A1_ext2, A2_ext2))
# A_ext_inhomogen1 <- trajectories::Tracks(list(A1_noD, A2_ext2)) # !!!
# A_ext_inhomogen2 <- trajectories::Tracks(list(A1, A2_ext))
# A_ext_inhomogen3 <- trajectories::Tracks(list(A1, A2_ext2))
# A_ext_inhomogen4 <- trajectories::Tracks(list(A1_test, A2_ext2))
# A_ext_inhomogen5 <- trajectories::Tracks(list(A1_noDnoC, A2_ext2)) # !!!
# A_ext_inhomogen6 <- trajectories::Tracks(list(A1_noC, A2_ext2))
# A_ext_inhomogen7 <- trajectories::Tracks(list(A1_noD, A2_noD)) # !!!
# A_ext_inhomogen8 <- trajectories::Tracks(list(A1_noD, A2_noC)) # !!!
# A_ext_inhomogen9 <- trajectories::Tracks(list(A1_noD, A2_noDnoC)) # !!!
# A_ext_inhomogen10 <- trajectories::Tracks(list(A1_noDnoC, A2_noDnoC)) # !!! Special case: just complete empty
# A_ext_inhomogen11 <- trajectories::Tracks(list(A1_noC, A2_noC)) # ?!
# 
# # # Check areal features by plot:
# # # plot(spPolygA1, axes=T, xlim = c(2, 8), ylim = c(4.5, 7.5))
# # # plot(spGridA1, axes=T, xlim = c(2, 8), ylim = c(4.5, 7.5))
# # # spPolygFromGrid <- as(spGridA1, "SpatialPolygons")
# # # plot(spPolygFromGrid, axes=T, xlim = c(2, 8), ylim = c(4.5, 7.5))
# # # plot(spPixA1, axes=T, xlim = c(2, 8), ylim = c(4.5, 7.5))
# # # spPolygFromPix <- as(spPixA1, "SpatialPolygons")
# # # plot(spPolygFromPix, axes=T, xlim = c(2, 8), ylim = c(4.5, 7.5))
# # ### --> scheint alles zu passen !?!?
# # 
# # ######
# # # !!!
# require(spacetime)
# t0 <- as.POSIXct("2013-09-30 01:58:00")
# t <- t0 + cumsum(rep(1,6) * 120)
# t0_tz <- as.POSIXct("2013-09-30 01:58:00")#, tzone = "UTC")
# t_tz <- t0_tz + cumsum(rep(1,6) * 120)
# attr(t_tz, "tzone") <- "UTC"
# 
# # time = t, Track  A1
# # STF @sp = Grid
# stfG <- STF(spGridA1, time = t) # Warnmeldung:
# # In ST(sp, time, endTime) : on constructing ST, converted SpatialGrid to SpatialPixels
# # STF @sp = Polyg
# stfP <- STF(spPolygA1, time = t)
# # STF @sp = Polyg from Grid
# plyg <- as(spGridA1, "SpatialPolygons")
# stfP2 <- STF(plyg, time = t) 
# # STF @sp = Pixels
# stfPix <- STF(spPixA1, time = t)
# 
# # STF @sp = Polyg, diff tzone
# stfP_tz <- STF(spPolygA1, time = t_tz)
# 
# # A1 as STF without data
# #A1_stf <- as(STI(A1@sp, A1@time, A1@endTime), "STS")
# #A1_stf <- as(STI(A1@sp, A1@time, A1@endTime), "STS")
# 
# # Achtung  / TODO
# # --> stf mit nur 1 Zeit / 1 sp-unit
# # --> ? aus st::agg: if ("data" %in% slotNames(by0@sp)) --> ???
# 
# stfP_A <- STF(spPolygA, time = t)
# stfG_A <- STF(spGridA, time = t)
# 
# stfP_Tr <- STF(spPolygTr, time = t)
# stfG_Tr <- STF(spGridTr, time = t)
# 
# #!!!!!!!
# df <- data.frame(test = runif(length(t)*length(spPolygA1)))
# stfdfP_A1 <- STFDF(sp = spPolygA1, time = t, data = df)
# # !?!?! --> TODO teste mit speziellen row.names for @data
# 
# # Test addAttrToGeom: geht für STF und für STFDF !!!
# df2 <- data.frame(test2 = runif(length(t)*length(spPolygA1)))
# thisdata_cbind <- cbind(stfdfP_A1@data, df2)
# thisdata_df <- data.frame(stfdfP_A1@data, df2)
# #identical(thisdata_cbind, thisdata_df)#T
# stfdfP2_A1 <- STFDF(sp = spPolygA1, time = t, data = thisdata_df)
# stfdfP3_A1 <- addAttrToGeom(stfdfP_A1, thisdata_df, match.ID = F)
# #identical(stfdfP2_A1, stfdfP3_A1)#T
# 
# df <- data.frame(test = runif(length(t)*length(spPolygTr)))
# stfdfP_Tr <- STFDF(sp = spPolygTr, time = t, data = df)
# 
# dt <- difftime(as.POSIXct(zoo::index(envT@time)[1]), as.POSIXct(tail(zoo::index(envT@time), 1)))
# #index(envT@time)
# te0 <- as.POSIXct(zoo::index(envT@time)[1]) - 120
# te <- te0 + cumsum(rep(1,5) * 240)
# stfEnvP <- STF(spPolygEnv1, time = te)
# stfEnvP2 <- STF(spPolygEnv3x3, time = te)
# 
# #stfEnvP2@time[1:3]
# #stfEnvP2@endTime[1:3]
# #str(envT)
# 
# 
# # ##
# # ## Further preparation
# # ##
# # TrX1_spdf <- as(Tracks_X1, "SpatialPointsDataFrame")
# # require(raster)
# # rasterLayerTrX1 <- raster(TrX1_spdf,ncols=10,nrows=10)
# # spPolygTrX1 <- as(rasterLayerTrX1, "SpatialPolygons")
# # 
# # TrX1.1 <- Tracks_X1@tracks[[1]]
# # TrX1.1_spdf <- as(Tracks_X1@tracks[[1]], "SpatialPointsDataFrame")
# # require(raster)
# # rasterLayerTrX1.1 <- raster(TrX1.1_spdf,ncols=10,nrows=10)
# # spPolygTrX1.1 <- as(rasterLayerTrX1.1, "SpatialPolygons")
# # #str(Tracks_X1)
# # thistime <- TrX1.1@time[c(1, length(TrX1.1@time)/2)]
# # stf_TrX1.1 <- STF(spPolygTrX1.1, thistime) 
# # #str(stf_TrX1.1)
# # 
# # TrcsX1_spdf <- as(Tracks_X1, "SpatialPointsDataFrame")
# # require(raster)
# # rasterLayerTrcsX1 <- raster(TrcsX1_spdf,ncols=7,nrows=7)
# # spPolygTrcsX1 <- as(rasterLayerTrcsX1, "SpatialPolygons")
# # #str(Tracks_X1)
# # #thistime <- TrcsX1@time[c(1, length(TrX1.1@time)/2)]
# # thistimeMin <- min(Tracks_X1@tracksData$tmin) #[c(1, length(TrX1.1@time)/2)]
# # thistimeMax <- max(Tracks_X1@tracksData$tmax) #[c(1, length(TrX1.1@time)/2)]
# # thistime2 <- Tracks_X1@tracksData$tmin[8]#) #[c(1, length(TrX1.1@time)/2)]
# # thistime <- c(thistimeMin,thistime2)
# # attr(thistime, "tzone") <- attr(thistime2, "tzone")
# # stf_TrcsX1 <- STF(spPolygTrcsX1, thistime) 
# # 
# # gyps_TracksColl <- trajectories::TracksCollection(list(Tracks_X1, Tracks_X2, Tracks_X3))
# # ####str(gyps_TracksColl) # timezone problem --> erledigt(ithub traj version)
# # gypsTrColl_spdf <- as(gyps_TracksColl, "SpatialPointsDataFrame")
# # require(raster)
# # rasterLayerGypsTrColl <- raster(gypsTrColl_spdf,ncols=10,nrows=10)
# # spPolygGypsTrColl <- as(rasterLayerGypsTrColl, "SpatialPolygons")
# # thistimeMin <- min(gyps_TracksColl@tracksCollectionData$tmin) #[c(1, length(TrX1.1@time)/2)]
# # thistimeMax <- max(gyps_TracksColl@tracksCollectionData$tmax) #[c(1, length(TrX1.1@time)/2)]
# # thistime2 <- Tracks_X1@tracksData$tmin[8]#) #[c(1, length(TrX1.1@time)/2)]
# # thistime <- c(thistimeMin,thistime2)
# # attr(thistime, "tzone") <- attr(thistime2, "tzone")
# # stf_gypsTrColl <- STF(spPolygGypsTrColl, thistime) 
# 
# 
# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # Alternative methods to produce polygons / grids / pixels
# # A1 # !
# # required: bbox und desiredDim <- 10
# desDim <- 10
# extents <- c(diff(A1@sp@bbox[1,]), diff(A1@sp@bbox[2,]))
# max_ext <- max(extents)
# largeSideIndex <- which(extents == max_ext)
# desiredDim <- desDim
# csize <- extents[largeSideIndex]/desiredDim
# dimX <- ceiling(extents[1]/csize) + 1
# dimY <- ceiling(extents[2]/csize) + 1
# 
# # test
# #ext_test <- c(2, 4)
# #extents <- ext_test
# #max_ext <- max(extents)
# #largeSideIndex <- which(extents == max_ext)
# #csize <- extents[largeSideIndex]/desiredDim
# #dimX <- extents[1]/csize + 1
# #dimY <- extents[2]/csize + 1
# 
# gt <- GridTopology(A1@sp@bbox[ , 1], c(csize,csize), c(dimX, dimY))
# spG <- SpatialGrid(gt, proj4string = A1@sp@proj4string)
# spPoly <- as(spG, "SpatialPolygons")
# stfP_gt <- STF(spPoly, time = t)
# 
# #aggTest <- aggregate(A1, stfP_gt)
# #spplot(aggTest[, 1, "co2"])
# #stplot(aggTest[, 1, "co2", drop=F])



# 
#===============================================================================
#
context("Test of over.STF.Track.R: \n")
#
#===============================================================================
# 
# ###message(paste("#####  ... #####\n", sep=""))
# 
test_that("# Basic Tests of over.STF.Track ...", {
  
  # my over: most simple case
  # A1
  over_stfPix <- over(stfPix, A1)
  over_stfP <- over(stfP, A1)
  over_stfG <- over(stfG, A1)
  # subset of x = stf
  over_stfPix_sub <- over(stfPix[1:3], A1)
  over_stfP_sub <- over(stfP[1:3], A1)
  over_stfG_sub <- over(stfG[1:3], A1)
  # length(x) = 1
  over_stf1 <- over(stfPix[1], A1)
  # --> scheint ok zu sein!!!
  
  # no matching
  over_stfP_B1_rlT <- over(stfP, B1, returnList = T, use.data=F)
  
  # spacetime over mit STIDF mit SpPoints ohne Daten @data slot !?
  # rl = F, fn=mean
  sp_over_test_mean_noRow <- over(stfG_A, as(A1_noD, "STIDF"), returnList=F, fn=mean, na.rm=T)
  #str(sp_over_test_mean_noRow); sp_over_test_mean_noRow$co2 # NA
  sp_over_test2 <- over(stfG_A, as(A1_noDnoC, "STIDF"), returnList=F, fn=mean, na.rm=T)
  #str(sp_over_test2); sp_over_test2$co2 ## complete empty df
  sp_over_test3 <- over(stfG_A, as(A1_noC, "STIDF"), returnList=F, fn=mean, na.rm=T)
  #str(sp_over_test3); sp_over_test3$co2 ## complete empty df
  # rl=T
  sp_over_test <- over(stfG_A, as(A1_noD, "STIDF"), returnList=T, fn=mean, na.rm=T)
  #str(sp_over_test); (sp_over_test[[1]]); sp_over_test$co2 # empty dfs (0rows) mit column in list
  sp_over_test2 <- over(stfG_A, as(A1_noDnoC, "STIDF"), returnList=T, fn=mean, na.rm=T)
  #str(sp_over_test2); sp_over_test2$co2 ## complete empty dfs in list
  sp_over_test3 <- over(stfG_A, as(A1_noC, "STIDF"), returnList=T, fn=mean, na.rm=T)
  #str(sp_over_test3); sp_over_test3$co2 ## complete empty dfs in list
  # rl=F, fn=NULL
  sp_over_test <- over(stfG_A, as(A1_noD, "STIDF"), returnList=F, fn=NULL, na.rm=T)
  #str(sp_over_test); sp_over_test$co2 # NA
  sp_over_test2 <- over(stfG_A, as(A1_noDnoC, "STIDF"), returnList=F, fn=NULL, na.rm=T)
  #str(sp_over_test2); sp_over_test2$co2 ## empty df (obs of 0 var)
  sp_over_test3 <- over(stfG_A, as(A1_noC, "STIDF"), returnList=F, fn=NULL, na.rm=T)
  #str(sp_over_test3); sp_over_test3$co2 ## empty df (obs of 0 var)
  #data.frame(do.call(rbind, sp_over_test2), data.frame(a = 11))
  
  # my trajaggr::over mit Track ohne Daten in @data slot und ohne stopifnot in code
  # rl = F
  my_over_test_mean_noRow <- over(stfG_A, A1_noD, returnList=F, fn=mean, na.rm=T, use.data=T)
  #str(my_over_test_mean_noRow); my_over_test_mean_noRow$co2 # NaN erledigt!
  my_over_test2 <- over(stfG_A, A1_noD, returnList=F, fn=mean, na.rm=T, use.data="co2")
  #str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  my_over_test2 <- over(stfG_A, A1_noDnoC, returnList=F, fn=mean, na.rm=T, use.data=T)
  ### erledigt --> undefined columns selected in lapply(cleanIndexList, function(z) { dat <- y@data[z, use.data, drop = FALSE]
  #str(my_over_test2); my_over_test2$co2 # TODO !!???
  my_over_test2 <- over(stfG_A, A1_noDnoC, returnList=F, fn=mean, na.rm=T, use.data="co2")
  ## erledigt --> undefined columns selected in lapply(cleanIndexList, function(z) { dat <- y@data[z, use.data, drop = FALSE]
  #str(my_over_test2); my_over_test2$co2 # TODO !!???
  my_over_test2 <- over(stfG_A, A1_noC, returnList=F, fn=mean, na.rm=T, use.data=TRUE)
  # erledigt --> undefined columns selected in lapply(cleanIndexList, function(z) { dat <- y@data[z, use.data, drop = FALSE]
  #str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  my_over_test2 <- over(stfG_A, A1_noC, returnList=F, fn=mean, na.rm=T, use.data= "co2")
  # erledigt --> undefined columns selected in lapply(cleanIndexList, function(z) { dat <- y@data[z, use.data, drop = FALSE]
  #str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  # rl = T
  my_over_test <- over(stfG_A, A1_noD, returnList=T, fn=mean, na.rm=T, use.data=T)
  #str(my_over_test); my_over_test$co2 # NaN erledigt!
  my_over_test2 <- over(stfG_A, A1_noD, returnList=T, fn=mean, na.rm=T, use.data="co2")
  #str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  my_over_test <- over(stfG_A, A1_noC, returnList=T, fn=mean, na.rm=T, use.data=T)
  #str(my_over_test); my_over_test$co2 # NaN erledigt!
  my_over_test2 <- over(stfG_A, A1_noC, returnList=T, fn=mean, na.rm=T, use.data="co2")
  #str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  my_over_test <- over(stfG_A, A1_noDnoC, returnList=T, fn=mean, na.rm=T, use.data=T)
  #str(my_over_test); my_over_test$co2 # NaN erledigt!
  my_over_test2 <- over(stfG_A, A1_noDnoC, returnList=T, fn=mean, na.rm=T, use.data="co2")
  #str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  # rl = F, fn=NULL
  my_over_test <- over(stfG_A, A1_noD, returnList=F, fn=NULL, na.rm=T, use.data=T)
  #str(my_over_test); my_over_test$co2 # NaN erledigt!
  my_over_test2 <- over(stfG_A, A1_noD, returnList=F, fn=NULL, na.rm=T, use.data="co2")
  #str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  my_over_test <- over(stfG_A, A1_noC, returnList=F, fn=NULL, na.rm=T, use.data=T)
  #str(my_over_test); my_over_test$co2 # NaN erledigt!
  my_over_test2 <- over(stfG_A, A1_noC, returnList=F, fn=NULL, na.rm=T, use.data="co2")
  #str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  my_over_test <- over(stfG_A, A1_noDnoC, returnList=F, fn=NULL, na.rm=T, use.data=T)
  #str(my_over_test); my_over_test$co2 # NaN erledigt!
  my_over_test2 <- over(stfG_A, A1_noDnoC, returnList=F, fn=NULL, na.rm=T, use.data="co2")
  #str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  
  # identical ? 
  #Tests mit expect_that...
  identical(sp_over_test_mean_noRow, my_over_test_mean_noRow[1])
  expect_that(my_over_test_mean_noRow[1], is_identical_to(sp_over_test_mean_noRow))
  
  # ‘max’ not meaningful for factors
  #myO_gen_stfP_A1_wmean_byT <- over(stfP, A1_ext2, returnList = FALSE, fn = max,
  #                                  na.rm = T, use.data = TRUE, weight.points = NULL)
  
  #x <- Tracks_X1; by <- stf_TrcsX1; FUN = matrixStats::weightedMedian; weight.points = NULL; weight.tracks = NULL
  # just warning about returning NA!!!
  # !!! Achtung: muss gyps data vorher laden
  #myO_gen_stfTrcsX1_TX1_median_byT <- over(stf_TrcsX1, Tracks_X1@tracks[[1]], returnList = FALSE, fn = sum,
  #                                  na.rm = T, use.data = "height_raw", weight.points = NULL) 
  
  #myO_gen_stfTrcsX1_TX1_median_byT
  #myO_gen_stfTrcsX1_TX1_median_byT[1:25,]
  
  # ‘sum’ not meaningful for factors (if weighting fct and no weights)
  #myO_gen_stfTrcsX1_TX1_wmean_byT <- over(stf_TrcsX1, Tracks_X1@tracks[[1]], returnList = FALSE, fn = weighted.mean,
  #                                         na.rm = T, use.data = TRUE, weight.points = NULL) 
  
  
#   myO_gen_stfTrcsX1_TX1_wmean_byT <- over(stf_TrcsX1, Tracks_X1@tracks[[1]], returnList = FALSE, fn = weighted.mean,
#                                           na.rm = T, use.data = TRUE, weight.points = "equal") 
#   myO_gen_stfTrcsX1_TX1_wmean_byT <- over(stf_TrcsX1, Tracks_X1@tracks[[1]], returnList = FALSE, fn = weighted.mean,
#                                           na.rm = T, use.data = TRUE, weight.points = "byDist") 
#   myO_gen_stfTrcsX1_TX1_wmean_byT <- over(stf_TrcsX1, Tracks_X1@tracks[[1]], returnList = FALSE, fn = weighted.mean,
#                                            na.rm = T, use.data = TRUE, weight.points = "byTime") 
#   
  
  #compare old and new simple weighting approach..
  #myO_gen_stfTrcsX1_TX1_wmean_byT_oldWeightApp <- over(stf_TrcsX1, Tracks_X1@tracks[[1]], returnList = FALSE, fn = weighted.mean,
  #                                        na.rm = T, use.data = TRUE, weight.points = "byTime") 
  #myO_gen_stfTrcsX1_TX1_noW <- over(stf_TrcsX1, Tracks_X1@tracks[[1]], returnList = FALSE, fn = mean,
  #                                                     na.rm = T, use.data = TRUE, weight.points = NULL) 
  #myO_gen_stfTrcsX1_TX1_wMedian_byT <- over(stf_TrcsX1, Tracks_X1@tracks[[1]], returnList = FALSE, fn = matrixStats::weightedMedian,
  #                                        na.rm = T, use.data = TRUE, weight.points = "byTime") 
  #myO_gen_stfTrcsX1_TX1_wMedian_byT_newWApp <- over(stf_TrcsX1, Tracks_X1@tracks[[1]], returnList = FALSE, fn = matrixStats::weightedMedian,
  #                                          na.rm = T, use.data = TRUE, weight.points = "byTime") 
  #myO_gen_stfTrcsX1_TX1_Median <- over(stf_TrcsX1, Tracks_X1@tracks[[1]], returnList = FALSE, fn = median,
  #                                          na.rm = T, use.data = "height_above_ellipsoid", weight.points = NULL) 
  #all.equal(myO_gen_stfTrcsX1_TX1_wmean_byT, myO_gen_stfTrcsX1_TX1_wmean_byT_oldWeightApp)
  #identical(myO_gen_stfTrcsX1_TX1_wmean_byT, myO_gen_stfTrcsX1_TX1_wmean_byT_oldWeightApp)
  #diff <- myO_gen_stfTrcsX1_TX1_wmean_byT - myO_gen_stfTrcsX1_TX1_wmean_byT_oldWeightApp
  #diff_2 <- myO_gen_stfTrcsX1_TX1_wmean_byT - myO_gen_stfTrcsX1_TX1_noW
  #diff_3 <- myO_gen_stfTrcsX1_TX1_wmean_byT - myO_gen_stfTrcsX1_TX1_wMedian_byT
  #diff_4 <- myO_gen_stfTrcsX1_TX1_Median - myO_gen_stfTrcsX1_TX1_wMedian_byT$height_above_ellipsoid
  #diff_5 <- myO_gen_stfTrcsX1_TX1_wMedian_byT_newWApp - myO_gen_stfTrcsX1_TX1_wMedian_byT
  # ok!!!
  
#   # ERLEDIGT:Fehler in as.POSIXct.numeric(value) : 'origin' must be supplied
#   # ??? !!!
#   # Aber eigentlcih darf es fehler geben, da unsinnig: no weights, but weighting fcts
#   myO_gen_stfTrcsX1_TX1_wMedian_byT <- over(stf_TrcsX1, Tracks_X1@tracks[[1]], returnList = FALSE, 
#                                             fn = matrixStats::weightedMedian, na.rm = T, 
#                                             use.data = TRUE, weight.points = "equal") 
#   myO_gen_stfTrcsX1_TX1_wMedian_byT <- over(stf_TrcsX1, Tracks_X1@tracks[[1]], returnList = FALSE, 
#                                             fn = matrixStats::weightedMedian, na.rm = T, 
#                                             use.data = TRUE, weight.points = "byDist") 
  
  
  
  myO_gen_stfP_A1_wmean_byT <- over(stfP, A1_ext2, returnList = FALSE, fn = weighted.mean,
                                    na.rm = T, use.data = TRUE, weight.points = "byTime")
  myO_gen_stfP_A1_wmean_byT_newWeightApp <- over(stfP, A1_ext2, returnList = FALSE, fn = weighted.mean,
                                    na.rm = T, use.data = TRUE, weight.points = "byTime")
  expect_that(myO_gen_stfP_A1_wmean_byT, equals(myO_gen_stfP_A1_wmean_byT_newWeightApp))
  all.equal(myO_gen_stfP_A1_wmean_byT$co2, myO_gen_stfP_A1_wmean_byT_newWeightApp$co2)
  #myO_gen_stfP_A1_colwmean_byT <- over(stfP, A1_ext2, returnList = FALSE, 
  #                                     fn = matrixStats::colWeightedMeans, na.rm = T,
  #                                     use.data = TRUE, weight.points = "byTime")
  
  #expect_that(myO_gen_stfP_A1_wmean_byT, is_identical_to(myO_gen_stfP_A1_colwmean_byT))
  #attributes(myO_gen_stfP_A1_colwmean_byT)
  
  
  myO_gen_stfP_envT_wmean_byT <- over(stfEnvP, envT, returnList = FALSE, fn = weighted.mean,
                                      na.rm = T, use.data = TRUE, weight.points = "byTime")
  #myO_gen_stfP_envT_colwmean_byT <- over(stfEnvP, envT, returnList = FALSE, 
  #                                     fn = matrixStats::colWeightedMeans, na.rm = T,
  #                                     use.data = TRUE, weight.points = "byTime")
  # ???
  #expect_that(myO_gen_stfP_envT_wmean_byT, is_identical_to(myO_gen_stfP_envT_colwmean_byT))
  #expect_that(myO_gen_stfP_envT_wmean_byT, equals(myO_gen_stfP_envT_colwmean_byT))
  #all.equal(myO_gen_stfP_envT_wmean_byT, myO_gen_stfP_envT_colwmean_byT)
  
  
  myO_gen_stfP_A_A1_mean <- over(stfP_A, A1, returnList = FALSE, fn = mean,
                                      na.rm = T, use.data = TRUE, weight.points = NULL)
  #str(myO_gen_stfP_A_A1_mean) # ok!!!
  
  
#   #
#   # rl=F, use.data=T/selction, fn!=NULL, weigghts = NULL
#   # data = gyps
#   #
#   myO_gen_stf_TrX1.1_TrX1.1_mean <- over(stf_TrX1.1, TrX1.1, returnList = FALSE, fn = mean,
#                                  na.rm = T, use.data = TRUE, weight.points = NULL)
#   
#   myO_gen_stf_TrX1.1_TrX1.1_max_sel <- over(stf_TrX1.1, TrX1.1, returnList = FALSE, fn = max,
#                                          na.rm = T, use.data = 1, weight.points = NULL)
#   
#   myO_gen_stf_TrX1.1_TrX1.1_min_sel <- over(stf_TrX1.1, Tracks_X1[1], returnList = FALSE, fn = min,
#                                         na.rm = T, use.data = 1:3, weight.points = NULL)
#   
#   myO_gen_stf_TrX1.1_TrX1.1_min_sel2 <- over(stf_TrX1.1, Tracks_X1[1], returnList = FALSE, fn = min,
#                                             na.rm = T, use.data = c("ground_speed", "height_above_ellipsoid"),
#                                             weight.points = NULL)
#   
#   myO_gen_stf_TrX1.1_TrX1.1_wmean_sel <- over(stf_TrX1.1, Tracks_X1[1], returnList = FALSE, 
#                                             fn = weighted.mean,
#                                             na.rm = T, use.data = c("ground_speed", "height_above_ellipsoid"),
#                                             weight.points = NULL)
#   
#   myO_gen_stf_TrX1.1_TrX1.1_wMedian_sel <- over(stf_TrX1.1, Tracks_X1[1], returnList = FALSE, 
#                                             fn = matrixStats::weightedMedian,
#                                             na.rm = T, use.data = c("ground_speed", "height_above_ellipsoid"),
#                                             weight.points = NULL)
#   
#   myO_gen_stf_TrX1.1_TrX1.1_wMedian_sel <- over(stf_TrX1.1, Tracks_X1[1], returnList = FALSE, 
#                                                 fn = matrixStats::weightedMedian,
#                                                 na.rm = T, use.data = c("ground_speed", "height_above_ellipsoid"),
#                                                 weight.points = NULL)
#   
#   # ? Error ?! check / todo
#   myO_gen_stf_TrX1.1_TrX1.1_quant0.9_sel <- over(stf_TrX1.1, Tracks_X1[1], returnList = FALSE, 
#                                                 fn = quantile, probs = 0.75,
#                                                 na.rm = T, use.data = c("ground_speed", "height_above_ellipsoid"),
#                                                 weight.points = NULL)
  
  # data Track example
  myO_gen_stfP_A1_mean_byT_1 <- over(stfP, A1_ext2, returnList = FALSE, 
                                        fn = mean, na.rm = T, 
                                        use.data = TRUE, weight.points = NULL)
  #myO_gen_stfP_A1_mean_byT_2 <- over(stfP, A1_ext2, returnList = FALSE, 
  #                                   fn = mean, na.rm = T, 
  #                                   use.data = TRUE, weight.points = "byTime")
  #myO_gen_stfP_A1_mean_byT_3 <- over(stfP, A1_ext2, returnList = FALSE, 
  #                                   fn = mean, na.rm = T, 
  #                                   use.data = c("co2", "test"), weight.points = "byTime")
  #identical(myO_gen_stfP_A1_mean_byT_1, myO_gen_stfP_A1_mean_byT_2)
  
  # Error: Fehler in sum(x) : ungültiger 'type' (character) des Argumentes:
  myO_gen_stfP_A1_mean_byT_4 <- over(stfP, A1_ext2, returnList = FALSE, 
                                     fn = weighted.mean, na.rm = T, 
                                     use.data = TRUE, weight.points = "byTime")
  # Error: Fehler in x * w : nicht-numerisches Argument für binären Operator:
  myO_gen_stfP_A1_mean_byT_3 <- over(stfP, A1_ext2, returnList = FALSE, 
                                     fn = weighted.mean, na.rm = T, 
                                     use.data = TRUE, weight.points = "byTime")
  myO_gen_stfP_A1_mean_byT_5 <- over(stfP, A1_ext2, returnList = FALSE, 
                                     fn = weighted.mean, na.rm = T, 
                                     use.data = c("co2", "test"), weight.points = "byTime")
  #identical()
})
