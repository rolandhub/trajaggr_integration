#test_file(paste(getwd(),"/tests/testthat/test_aggregate_by_sp.R", sep=""))
#===============================================================================
#
# JUST RUN THIS TEST FILE
#
#===============================================================================

#require(testthat)
#test_file(paste(getwd(),"/tests/testthat/test_aggregate_by_sp.R", sep=""))

#-------------------------------------------------------------------------------

#require(spacetime)
#require(xts)

# Need to load data...
#source("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/trajaggr/tests/testthat/createData_overAndAggTests.R")
#source("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/trajaggr/tests/testthat/_createData_overAndAggTests_old.R")

source("createData_overAndAggTests.R")


# # test mit anderer x geoemtry
# # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # Alternative methods to produce polygons / grids / pixels
# # Initialize Variables
# desDim <- 3 # desired spatial dimension in one direction
# track <- A1 # desired spatial extent, defined by Track object
# desTimeIntervals <- 4 # desired number of time intervals for the STF object
# # Geometry preparation
# extents <- c(diff(track@sp@bbox[1,]), diff(track@sp@bbox[2,]))
# max_ext <- max(extents)
# largeSideIndex <- which(extents == max_ext)
# csize <- extents[largeSideIndex]/desDim
# dimX <- ceiling(extents[1]/csize) + 1
# dimY <- ceiling(extents[2]/csize) + 1
# # Create grid topolgy
# gt <- GridTopology(track@sp@bbox[ , 1], c(csize,csize), c(dimX, dimY))
# spG <- SpatialGrid(gt, proj4string = track@sp@proj4string)
# spPix <- as(spG, "SpatialPixels")
# spPoly <- as(spG, "SpatialPolygons")
# # Create stf object
# t0 <- index(track@time[1])
# t_diff_sec <- 60 * ((track@endTime[length(track)] + 1) - t0)
# t <- c(t0, t0 + cumsum(rep(1, desTimeIntervals - 1) * ceiling(as.numeric(t_diff_sec)/(desTimeIntervals))))
# t_end <- t0 + cumsum(rep(1, desTimeIntervals) * ceiling(as.numeric(t_diff_sec)/(desTimeIntervals)))
# stfP_gt <- STF(spPoly, time = t, endTime = t_end)
# stfG_gt <- STF(spG, time = t, endTime = t_end)
# stfPix_gt <- STF(spPix, time = t, endTime = t_end)
# stfP_1t_gt <- STF(spPoly, time = t0, endTime = t_end[length(t_end)])
# stfG_1t_gt <- STF(spG, time = t0, endTime = t_end[length(t_end)])
# stfPix_1t_gt <- STF(spPix, time = t0, endTime = t_end[length(t_end)])

#

#===============================================================================
#
context("Test of aggregate_Track_sp in aggregate_by_sp.R: \n")
#
#===============================================================================
# 
# ###message(paste("#####  ... #####\n", sep=""))
#
# 
test_that("# Tests of aggregate_Track_sp with populated data.frame objects in data slot ...", {
  
  #
  # agg(Track,sp)
  #
  # all points intersecting spatial geometries
  agg_A1_spGridA1 <- aggregate(A1, spGridA1, use.data=c("co2")); #class(agg_A1_spGridA1)
  agg_A1_spGridA1 <- aggregate(A1, spGridA1); #class(agg_A1_spGridA1)
  expect_that(length(agg_A1_spGridA1), equals(length(spGridA1)* length(A1)))
  
  agg_A1_spPolygA1 <- aggregate(A1, spPolygA1); #class(agg_A1_spGridA1)
  expect_that(length(agg_A1_spPolygA1), equals(length(spPolygA1)* length(A1)))
  expect_that(agg_A1_spPolygA1@data, is_identical_to(agg_A1_spGridA1@data))
  
  agg_A1_spPolygA1_co2_wp <- aggregate(A1, spPolygA1, FUN=mean, use.data = "co2", weight.points="byDist")
  expect_that(agg_A1_spPolygA1@data, is_identical_to(agg_A1_spPolygA1_co2_wp@data))
  
  agg_A1_spPolygA1_co2_wp_wf <- aggregate(A1, spPolygA1, FUN=weighted.mean, use.data = "co2", weight.points="byDist")
  expect_that(agg_A1_spPolygA1_co2_wp_wf@data, is_identical_to(agg_A1_spPolygA1_co2_wp@data))
  
  
  # Not all points intersecting spatial geometry
  # Just one spatial geoemtry
  a1_spdf_sub <- a1_spdf[1:3, ]
  spPolyA1_sub <- createSpatialArealObjFromPoints(a1_spdf_sub, 1, "SpatialPolygons")
  spGrdA1_sub <- createSpatialArealObjFromPoints(a1_spdf_sub, 1, "SpatialGrid")
  spPxA1_sub <- createSpatialArealObjFromPoints(a1_spdf_sub, 1, "SpatialPixels")
    # simplify = F
  agg_spPolyA1s_1g_A1_ <- aggregate(A1, spPolyA1_sub, simplify=F)
  # !!! Probl --> TODO timeIndex not starting with 1... !!! ???
  #agg_spPolyA1s_1g_A1_udF <- aggregate(A1, spPolyA1_sub, use.data = F, simplify=F)
  # --> use.data = F --> ??? TODO check
  #expect_that(agg_spPolyA1s_1g_A1_@data, is_identical_to(agg_spPolyA1s_1g_A1_udF@data))
  agg_spGrdA1s_1g_A1_ <- aggregate(A1, spGrdA1_sub, simplify=F)
  expect_that(agg_spPolyA1s_1g_A1_@data, is_identical_to(agg_spGrdA1s_1g_A1_@data))
  agg_spPxA1s_1g_A1_ <- aggregate(A1, spPxA1_sub, simplify=F)
  expect_that(agg_spGrdA1s_1g_A1_@data, is_identical_to(agg_spPxA1s_1g_A1_@data))
  # !!
  expect_that(agg_spPxA1s_1g_A1_, is_a("STF"))
  
    # simplify = TRUE
  agg_spPolyA1s_1g_A1_smply <- aggregate(A1, spPolyA1_sub, simplify=T)
  # !!! Probl --> TODO timeIndex not starting with 1... !!! ???
  #agg_spPolyA1s_1g_A1_udF_smply <- aggregate(A1, spPolyA1_sub, use.data = F, simplify=T)
  # --> usedata = F --> ??? TODO
  #expect_that(agg_spPolyA1s_1g_A1_smply, is_identical_to(agg_spPolyA1s_1g_A1_udF_smply))
  agg_spGrdA1s_1g_A1_smply <- aggregate(A1, spGrdA1_sub, simplify=T)
  # check --> ok, in createSPatialAreal... dim + 1 causes non-identity
  expect_that(agg_spPolyA1s_1g_A1_smply, is_identical_to(agg_spGrdA1s_1g_A1_smply))
  expect_that(agg_spPolyA1s_1g_A1_smply, is_equivalent_to(agg_spGrdA1s_1g_A1_smply))
  agg_spPxA1s_1g_A1_smply <- aggregate(A1, spPxA1_sub, simplify=T)
  expect_that(agg_spGrdA1s_1g_A1_smply, is_identical_to(agg_spPxA1s_1g_A1_smply))
  # !!
  expect_that(agg_spPxA1s_1g_A1_smply, is_a("xts"))
  
  
  # Some spatial geometries, not intersecting all points
  spPolyA1_3 <- createSpatialArealObjFromPoints(a1_spdf, 3, "SpatialPolygons"); 
  spGrdA1_3 <- createSpatialArealObjFromPoints(a1_spdf, 3, "SpatialGrid"); 
  spPxA1_3 <- createSpatialArealObjFromPoints(a1_spdf, 3, "SpatialPixels");
  agg_spPolyA1_3_A1 <- aggregate(A1, spPolyA1_3)
  agg_spGrdA1_3_A1 <- aggregate(A1, spGrdA1_3)
  expect_that(agg_spPolyA1_3_A1@data, is_identical_to(agg_spGrdA1_3_A1@data))
  
  agg_spPxA1_3_A1 <- aggregate(A1, spPxA1_3)
  expect_that(agg_spPolyA1_3_A1@data, is_identical_to(agg_spPxA1_3_A1@data))
  
  
  #...
  #...
  
})


#===============================================================================
#
context("Test of aggregate_Tracks_sp in aggregate_by_sp.R: \n")
#
#===============================================================================
#
# 
test_that("# Tests of aggregate_Tracks_sp with populated data.frame objects in data slot ...", {
  
  #
  # agg(Tracks,sp)
  #
  # all points intersecting spatial geometries
  agg_A_spGridA <- aggregate(A, spGridA); #class(agg_A1_spGridA1)
  expect_that(length(agg_A_spGridA), equals(12*9)) #
  
  agg_A_spGridA_m <- aggregate(A, spGridA, na.rm=T); #class(agg_A1_spGridA1)
  agg_A_spGridA_m@data
  
  agg_A_spPolygA <- aggregate(A, spPolygA); #class(agg_A1_spGridA1)
  expect_that(length(agg_A_spPolygA), equals(12*9))
  expect_that(agg_A_spPolygA@data, is_identical_to(agg_A_spGridA@data))
  
  agg_A_spPolygA_co2_wp <- aggregate(A, spPolygA, FUN=mean, use.data = "co2", weight.points="byDist")
  expect_that(agg_A_spPolygA@data, is_identical_to(agg_A_spPolygA_co2_wp@data))
  
  agg_A_spPolygA_co2_wp_wf <- aggregate(A, spPolygA, FUN=weighted.mean, use.data = "co2", weight.points="byDist")
  expect_that(agg_A_spPolygA_co2_wp_wf@data, is_identical_to(agg_A_spPolygA_co2_wp@data))

  
  # Not all points intersecting spatial geometry
  # Just one spatial geoemtry
  # Achtung: evtl spdf anpassen...! (bishe rnur a1 berücksichtigt!!!)
  a1_spdf_sub <- a1_spdf[1:3, ]
  spPolyA1_sub <- createSpatialArealObjFromPoints(a1_spdf_sub, 1, "SpatialPolygons")
  spGrdA1_sub <- createSpatialArealObjFromPoints(a1_spdf_sub, 1, "SpatialGrid")
  spPxA1_sub <- createSpatialArealObjFromPoints(a1_spdf_sub, 1, "SpatialPixels")
    # simplify = F
  agg_spPolyA1s_1g_A_ <- aggregate(A, spPolyA1_sub, simplify=F)
  #agg_spPolyA1s_1g_A_udF <- aggregate(A, spPolyA1_sub, use.data = F, simplify=F)
  # ??? check --> was apssierrt bei use.data = F ????
  # Fehler in z[, mdNames] : falsche Anzahl von Dimensionen, s.u; use.data = F
  #expect_that(agg_spPolyA1s_1g_A_@data, is_identical_to(agg_spPolyA1s_1g_A_udF@data))
  agg_spGrdA1s_1g_A_ <- aggregate(A, spGrdA1_sub, simplify=F)
  expect_that(agg_spPolyA1s_1g_A_@data, is_identical_to(agg_spGrdA1s_1g_A_@data))
  agg_spPxA1s_1g_A_ <- aggregate(A, spPxA1_sub, simplify=F)
  expect_that(agg_spGrdA1s_1g_A_@data, is_identical_to(agg_spPxA1s_1g_A_@data))
  # !!
  expect_that(agg_spPxA1s_1g_A_, is_a("STF"))
  
    # simplify = TRUE
  agg_spPolyA1s_1g_A_smply <- aggregate(A, spPolyA1_sub, simplify=T)
  #agg_spPolyA1s_1g_A_udF_smply <- aggregate(A, spPolyA1_sub, use.data = F, simplify=T)
  # ??? check --> was apssierrt bei use.data = F ????
  # Error: Fehler in z[, mdNames] : falsche Anzahl von Dimensionen
  # -->
  #expect_that(agg_spPolyA1s_1g_A1_smply, is_identical_to(agg_spPolyA1s_1g_A1_udF_smply))
  agg_spGrdA1s_1g_A_smply <- aggregate(A, spGrdA1_sub, simplify=T)
  #  check: --> ok, in createSpatialAreal... dim + 1 causes non-identity
  expect_that(agg_spPolyA1s_1g_A_smply, is_identical_to(agg_spGrdA1s_1g_A_smply))
  expect_that(agg_spPolyA1s_1g_A_smply, is_equivalent_to(agg_spGrdA1s_1g_A_smply))
  agg_spPxA1s_1g_A_smply <- aggregate(A, spPxA1_sub, simplify=T)
  expect_that(agg_spGrdA1s_1g_A_smply, is_identical_to(agg_spPxA1s_1g_A_smply))
  # !!
  expect_that(agg_spPxA1s_1g_A_smply, is_a("xts"))

})