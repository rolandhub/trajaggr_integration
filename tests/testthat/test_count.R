#test_file(paste(getwd(),"/tests/testthat/test_count.R", sep=""))
#===============================================================================
#
# JUST RUN THIS TEST FILE
#
#===============================================================================

#require(testthat)
#test_file(paste(getwd(),"/tests/testthat/test_count.R", sep=""))

#-------------------------------------------------------------------------------

#source("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/trajaggr/tests/testthat/_createData_overAndAggTests_old.R")
source("createData_overAndAggTests.R")




# # !? raster hier benutzt, aber nicht in Description (Imp, Dep, Sug)
# 
# # Preparation
# require(raster)
# a1_spdf <- as(A1, "SpatialPointsDataFrame")
# a_spdf <- as(A, "SpatialPointsDataFrame")
# envT <- enviroCar_TracksObj1@tracks[[1]]; class(envT)
# env1_spdf <- as(enviroCar_TracksObj1@tracks[[1]], "SpatialPointsDataFrame")
# tr_spdf <- as(Tr, "SpatialPointsDataFrame")
# rasterLayerEnv1 <- raster(env1_spdf,ncols=7,nrows=7)
# rasterLayerTr <- raster(tr_spdf,ncols=3,nrows=3)
# rasterLayerA <- raster(a_spdf,ncols=3,nrows=3)
# rasterLayerA1 <- raster(a1_spdf,ncols=3,nrows=3)
# rasterLayerA_20 <- raster(a_spdf,ncols=20,nrows=20)
# spGridTr <- as(rasterLayerTr, "SpatialGrid") # SpatialGrid
# spGridA <- as(rasterLayerA, "SpatialGrid") # SpatialGrid
# spGridA1 <- as(rasterLayerA1, "SpatialGrid") # SpatialGrid
# spPixA <- as(rasterLayerA, "SpatialPixels") # # SpatialPixels
# spPixA1 <- as(rasterLayerA1, "SpatialPixels") # # SpatialPixels
# spPixA_20 <- as(rasterLayerA_20, "SpatialPixels") # # SpatialPixels
# spPolygTr <- as(rasterLayerTr, "SpatialPolygons")
# spPolygA <- as(rasterLayerA, "SpatialPolygons")
# spPolygA1 <- as(rasterLayerA1, "SpatialPolygons")
# spPolygEnv1 <- as(rasterLayerEnv1, "SpatialPolygons")
# #index(Tr@tracksCollection[[1]]@tracks[[1]]@time[1])
# 
# A1_ext <- A1
# A1_ext@data$test <- round(runif(length(A1)) * 3)
# A2_ext <- A2
# A2_ext@data$test <- round(runif(length(A2)) * 3)
# 
# A_ext <- trajectories::Tracks(list(A1_ext, A2_ext))
# 
# # Check areal features by plot:
# # plot(spPolygA1, axes=T, xlim = c(2, 8), ylim = c(4.5, 7.5))
# # plot(spGridA1, axes=T, xlim = c(2, 8), ylim = c(4.5, 7.5))
# # spPolygFromGrid <- as(spGridA1, "SpatialPolygons")
# # plot(spPolygFromGrid, axes=T, xlim = c(2, 8), ylim = c(4.5, 7.5))
# # plot(spPixA1, axes=T, xlim = c(2, 8), ylim = c(4.5, 7.5))
# # spPolygFromPix <- as(spPixA1, "SpatialPolygons")
# # plot(spPolygFromPix, axes=T, xlim = c(2, 8), ylim = c(4.5, 7.5))
# ### --> scheint alles zu passen !?!?
# 
# ######
# # !!!
# require(spacetime)
# t0 <- as.POSIXct("2013-09-30 01:58:00")
# t <- t0 + cumsum(rep(1,6) * 120)
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
# stfP_A <- STF(spPolygA, time = t)
# stfG_A <- STF(spGridA, time = t)
# 
# stfP_Tr <- STF(spPolygTr, time = t)
# stfG_Tr <- STF(spGridTr, time = t)
# 
# #!!!!!!!
# df <- data.frame(test = runif(length(t)*length(spPolygA1)))
# stfdfP_A1 <- STFDF(sp = spPolygA1, time = t, data = df)
# 
# dt <- difftime(as.POSIXct(index(envT@time)[1]), as.POSIXct(tail(index(envT@time), 1)))
# #index(envT@time)
# te0 <- as.POSIXct(index(envT@time)[1]) - 120
# te <- te0 + cumsum(rep(1,9) * 120)
# stfEnvP <- STF(spPolygEnv1, time = te)
# 
# 


# Tests

# # count by splines
# c_A1_spPolyg_bySpL <- countBySpL(A1, spPolygA1)
# c_A1_spPolyg_bySpL@data
# c_A1_spGrid_bySpL <- countBySpL(A1, spGridA1)
# c_A1_spGrid_bySpL@data
# #c_A1_spPix_bySpL <- countBySpL(A1, spPixA1)


#===============================================================================
#
context("Test of count: \n")
#
#===============================================================================

test_that("Tests of count ...", {
  
  # count - Test if count by sp is identical to count by STF (1 overall time interval)
  count_A1_sp <- count(A1, spPolygA); count_A1_sp@data
  count_A_sp <- count(A, spPolygA); count_A_sp@data
  count_Tr_sp <- count(Tr, spPolygA); count_Tr_sp@data
  count_Tr_sp_byID <- count(Tr, spPolygA, byID = T); count_Tr_sp_byID@data
  tmin <- Tr@tracksCollectionData$tmin[1]
  tmax <- Tr@tracksCollectionData$tmax[2]
  require(spacetime)
  stfP_A_1time <- STF(spPolygA, time = tmin, endTime = tmax)
  count_A1_stf_1t <- count(A1, stfP_A_1time); class(count_A1_stf_1t); count_A1_stf_1t@data
  count_A_stf_1t <- count(A, stfP_A_1time); class(count_A_stf_1t); count_A_stf_1t@data
  count_Tr_stf_1t <- count(Tr, stfP_A_1time); class(count_Tr_stf_1t); count_Tr_stf_1t@data
  count_Tr_stf_1t_byID <- count(Tr, stfP_A_1time, byID = T); class(count_Tr_stf_1t); count_Tr_stf_1t@data
  expect_that(count_A1_sp@data, is_identical_to(count_A1_stf_1t@data))
  expect_that(count_A_sp@data, is_identical_to(count_A_stf_1t@data))
  expect_that(count_Tr_sp@data, is_identical_to(count_Tr_stf_1t@data))
})

