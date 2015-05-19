#test_file(paste(getwd(),"/tests/testthat/test_over_sp.R", sep=""))
#===============================================================================
#
# JUST RUN THIS TEST FILE
#
#===============================================================================

#require(testthat)
#test_file(paste(getwd(),"/tests/testthat/test_over_sp.R", sep=""))

#-------------------------------------------------------------------------------

 
# Need to load data...
#source("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/trajaggr/tests/testthat/_createData_overAndAggTests_old.R")

source("createData_overAndAggTests.R")

# 
# 
# 
#===============================================================================
#
context("Test of over_sp.R: \n")
#
#===============================================================================

test_that("# Basic Tests of over_sp ...", {
  
  
  # my over, if no matchings 
  over_spPoly_B1_rlT <- over(spPoly, B1, returnList = T, use.data=F); over_spPoly_B1_rlT # erledigt --> list() --> TODO !!!
  over_spPoly_B1_rlF <- over(spPoly, B1, returnList = F, use.data=F); over_spPoly_B1_rlF
  over_spPoly_B1_rlT_udT <- over(spPoly, B1, returnList = T, use.data=T); over_spPoly_B1_rlT_udT
  over_spPoly_B1_rlF_udT <- over(spPoly, B1, returnList = F, use.data=T); over_spPoly_B1_rlF_udT # ? ok!? ???
  over_spPoly_B1_rlF_udT_fn <- over(spPoly, B1, returnList = F, use.data=T, fn=mean); over_spPoly_B1_rlF_udT_fn
  # erledigt --> all shit !!! TODO Achtung !! ????
  
  # matching
  over_spPoly_B2_rlT <- over(spPoly, B2, returnList = T, use.data=F); over_spPoly_B2_rlT # erledigt --> list() --> TODO !!!
  over_spPoly_B2_rlF <- over(spPoly, B2, returnList = F, use.data=F); over_spPoly_B2_rlF
  over_spPoly_B2_rlT_udT <- over(spPoly, B2, returnList = T, use.data=T); over_spPoly_B2_rlT_udT
  over_spPoly_B2_rlF_udT <- over(spPoly, B2, returnList = F, use.data=T); over_spPoly_B2_rlF_udT # ? ok!? ???
  over_spPoly_B2_rlF_udT_fn <- over(spPoly, B2, returnList = F, use.data=T, fn=mean); over_spPoly_B2_rlF_udT_fn
  
  over_spPoly_A1_rlT <- over(spPoly, A1, returnList = T, use.data=F); over_spPoly_A1_rlT # erledigt --> list() --> TODO !!!
  over_spPoly_A1_rlF <- over(spPoly, A1, returnList = F, use.data=F); over_spPoly_A1_rlF
  over_spPoly_A1_rlT_udT <- over(spPoly, A1, returnList = T, use.data=T); over_spPoly_A1_rlT_udT
  over_spPoly_A1_rlF_udT <- over(spPoly, A1, returnList = F, use.data=T); over_spPoly_A1_rlF_udT # ? ok!? ???
  over_spPoly_A1_rlF_udT_fn <- over(spPoly, A1, returnList = F, use.data=T, fn=mean); over_spPoly_A1_rlF_udT_fn
  
    
  # test mit anderer x geoemtry
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # Alternative methods to produce polygons / grids / pixels
  # Initialize Variables
  desDim <- 3 # desired spatial dimension in one direction
  track <- A1 # desired spatial extent, defined by Track object
  desTimeIntervals <- 4 # desired number of time intervals for the STF object
  # Geometry preparation
  extents <- c(diff(track@sp@bbox[1,]), diff(track@sp@bbox[2,]))
  max_ext <- max(extents)
  largeSideIndex <- which(extents == max_ext)
  csize <- extents[largeSideIndex]/desDim
  dimX <- ceiling(extents[1]/csize) + 1
  dimY <- ceiling(extents[2]/csize) + 1
  # Create grid topolgy
  gt <- GridTopology(track@sp@bbox[ , 1], c(csize,csize), c(dimX, dimY))
  spG <- SpatialGrid(gt, proj4string = track@sp@proj4string)
  spPix <- as(spG, "SpatialPixels")
  spPoly <- as(spG, "SpatialPolygons")
  # Create stf object
  t0 <- index(track@time[1])
  t_diff_sec <- 60 * ((track@endTime[length(track)] + 1) - t0)
  t <- c(t0, t0 + cumsum(rep(1, desTimeIntervals - 1) * ceiling(as.numeric(t_diff_sec)/(desTimeIntervals))))
  t_end <- t0 + cumsum(rep(1, desTimeIntervals) * ceiling(as.numeric(t_diff_sec)/(desTimeIntervals)))
  stfP_gt <- STF(spPoly, time = t, endTime = t_end)
  stfG_gt <- STF(spG, time = t, endTime = t_end)
  stfPix_gt <- STF(spPix, time = t, endTime = t_end)
  stfP_1t_gt <- STF(spPoly, time = t0, endTime = t_end[length(t_end)])
  stfG_1t_gt <- STF(spG, time = t0, endTime = t_end[length(t_end)])
  stfPix_1t_gt <- STF(spPix, time = t0, endTime = t_end[length(t_end)])
  ## test time #
  #identical(index(stfG_gt@time[4]), stfG_gt@endTime[3])
  #all.equal(index(stfG_gt@time[4]), stfG_gt@endTime[3])
  #difft <- index(stfG_gt@time[4]) - stfG_gt@endTime[3]
  #a1_stidf <- as(A1, "STIDF")
  #difft2 <- index(stfG_gt@time[1]) - index(a1_stidf@time)[1]
  ## --> ok
  ###

  # my over
  # my over-stf, indexList
  my_over_stfP_gt_A1_rlT <- over(stfP_gt, A1, returnList=T); class(my_over_stfP_gt_A1_rlT)
  my_over_stfP_gt_A1_rlT
  # my over-stf, indexVector
  my_over_stfP_gt_A1_rlF <- over(stfP_gt, A1, returnList=F); class(my_over_stfP_gt_A1_rlF)
  my_over_stfP_gt_A1_rlF
  # my over-stf, returnList=T und use.data=T|selctin
  my_over_stfP_gt_A1_rlT_udT <- over(stfP_gt, A1, returnList=T, use.data=T); class(my_over_stfP_gt_A1_rlT_udT)
  my_over_stfP_gt_A1_rlT_udT
  # my over-stf, one time
  my_over_stfG_1t_gt_A1_rlF <- over(stfG_1t_gt, A1, returnList=F); class(my_over_stfG_1t_gt_A1_rlF)
  my_over_stfG_1t_gt_A1_rlF
  # --> ok, gleiches ergebnis wie sp-over !!!
  # my over-stf, several times
  my_over_stfG_gt_A1_rlF <- over(stfG_gt, A1, returnList=F); class(my_over_stfG_gt_A1_rlF)
  my_over_stfG_gt_A1_rlF
  my_over_stfP_gt_A1_rlF <- over(stfP_gt, A1, returnList=F); class(my_over_stfP_gt_A1_rlF)
  my_over_stfP_gt_A1_rlF
  spPDF <- SpatialPolygonsDataFrame(spPoly, data = data.frame(rep(1, length(spPoly))), match.ID=F)
  spplot(spPDF, sp.layout = list("sp.points", a1_spdf, col = "red", cex=1.90, pch=5), scales=list(draw=TRUE))#, sp_points_NotNA)}else NULL, )
  # --> ok !?
  # my over-stf, 1 spatial geoemtry
  my_over_stfG_1g_gt_A1_rlF <- over(stfG_gt[1,], A1, returnList=F); class(my_over_stfG_1g_gt_A1_rlF)
  my_over_stfG_1g_gt_A1_rlF
  # --> ok! length = 4 --> 4 times
  # !!!
  # my over-sp !!! 20141214: noch nicht korrekt, siehe 8 zeieln weiter unten: my_over_spPoly_1g_A1_rlF
  my_over_spGrid_A1_rlF <- over(spGridA1[], A1, returnList=F, weigt.points="byTime"); class(my_over_spGrid_A1_rlF)
  my_over_spGrid_A1_rlF <- over(spGridA1[], A1, returnList=F); class(my_over_spGrid_A1_rlF)
  my_over_spGrid_A1_rlF
  # 1 gridcell --> ??? Problem with subsetting spGrid !?!
  #my_over_spGrid_1g_A1_rlF <- over(spGridA1[1], A1, returnList=F); class(my_over_spGrid_1g_A1_rlF)
  #my_over_spGrid_1g_A1_rlF
  # 1 polyg --> problem
  my_over_spPoly_1g_A1_rlF <- over(spPolygA1[1], A1, returnList=F); class(my_over_spPoly_1g_A1_rlF)
  my_over_spPoly_1g_A1_rlF # [1] NA NA NA NA NA NA  7
  # --> problem: wenn nur 1 point matches, dann sollte auch nur 1 point im erg sein !?!?
  # --> TODO !!! ???
  # !!!
  
  # But: Problem with Grid... abe rnur die erzeugung schient problem zumachen...!!!
  # my_overAltern_spGrid_1g_A1_rlF <- over_sp_Track_alternativ(spGridA1[1], A1)
  # Neuer test mit neuen Objects
  
#   a1_spdf_sub <- a1_spdf[1:3, ]
#   spPolyA1_sub <- createSpatialArealObjFromPoints(a1_spdf_sub, 1, "SpatialPolygons"); class(spPolyA1_sub)
#   spGrdA1_sub <- createSpatialArealObjFromPoints(a1_spdf_sub, 1, "SpatialGrid"); class(spGrdA1_sub)
#   spPxA1_sub <- createSpatialArealObjFromPoints(a1_spdf_sub, 1, "SpatialPixels"); class(spPxA1_sub)
#   
#   spPDF <- SpatialPolygonsDataFrame(spPolyA1_sub, data = data.frame(rep(1, length(spPolyA1_sub))), match.ID=F)
#   spplot(spPDF, sp.layout = list("sp.points", a1_spdf, col = "red", cex=1.90, pch=5), scales=list(draw=TRUE))#, sp_points_NotNA)}else NULL, )
#   # passt !?
#   spPolyA1_sub2 <- createSpatialArealObjFromPoints(a1_spdf_sub, 2, "SpatialPolygons"); class(spPolyA1_sub2)
#   spGrdA1_sub2 <- createSpatialArealObjFromPoints(a1_spdf_sub, 2, "SpatialGrid"); class(spGrdA1_sub2)
#   spPxA1_sub2 <- createSpatialArealObjFromPoints(a1_spdf_sub, 2, "SpatialPixels"); class(spPxA1_sub2)
#   
#   spPDF <- SpatialPolygonsDataFrame(spPolyA1_sub2, data = data.frame(rep(1, length(spPolyA1_sub2))), match.ID=F)
#   spplot(spPDF, sp.layout = list("sp.points", a1_spdf, col = "red", cex=1.90, pch=5), scales=list(draw=TRUE))#, sp_points_NotNA)}else NULL, )
#   # passt !?
#   spPolyA1_3 <- createSpatialArealObjFromPoints(a1_spdf, 3, "SpatialPolygons"); class(spPolyA1_3)
#   spGrdA1_3 <- createSpatialArealObjFromPoints(a1_spdf, 3, "SpatialGrid"); class(spGrdA1_3)
#   spPxA1_3 <- createSpatialArealObjFromPoints(a1_spdf, 3, "SpatialPixels"); class(spPxA1_3)
  
  
  
  over_sp_rlT <- over(spPolygA1[1], A1, returnList = T)
  over_sp_rlT_ud <- over(spPolygA1[1], A1, returnList = T, use.data = T)
  over_sp_rlF_ud <- over(spPolygA1, A1, returnList = F, use.data = T)
  over_sp_rlF_ud_fn.mean <- over(spPolygA1, A1, returnList = F, fn = mean, use.data = T)
  
  
  over_sp_rlF <- over(stfG, A1, use.data=T)
  
  over_sp_rlF <- over(spPolygA1[1], A1)
  over_sp_rlT <- over(spPolygA1[1], A1, returnList = T)
  over_sp_rlT_ud <- over(spPolygA1[1], A1, returnList = T, use.data = T)
  over_sp_rlF_ud <- over(spPolygA1, A1, returnList = F, use.data = T)
  over_sp_rlF_ud_fn.mean <- over(spPolygA1, A1, returnList = F, fn = mean, use.data = T)

  
  over_sp_rlF <- over(spPolygA1, A1)
  over_sp_rlT <- over(spPolygA1, A1, returnList = T)
  over_sp_rlT_ud <- over(spPolygA1, A1, returnList = T, use.data = T)
  
  
})