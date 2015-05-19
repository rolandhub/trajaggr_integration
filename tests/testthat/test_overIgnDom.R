#test_file(paste(getwd(),"/tests/testthat/test_overIgnDom.R", sep=""))
#===============================================================================
#
# JUST RUN THIS TEST FILE
#
#===============================================================================

#require(testthat)
#test_file(paste(getwd(),"/tests/testthat/test_overIgnDom.R", sep=""))

#-------------------------------------------------------------------------------


# Need to load data...
#source("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/trajaggr/tests/testthat/_createData_overAndAggTests_old.R")
source("createData_overAndAggTests.R")

# 
# 
# 
#===============================================================================
#
context("Test of overIgnDom.R: \n")
#
#===============================================================================

test_that("# Basic Tests of overIgnDom ...", {
  
  # use.data = F
  
  # returnList = F
  # x = SpPolygons, Grid or Pixels
  o_A1_by_spPolyg <- sp::over(spPolygA1, A1@sp, returnList = FALSE)
  #myO_A1_by_spPolyg <- over.sp.Track(spPolygA1, A1, returnList = FALSE)
  myO_A1_by_spPolyg <- overIgnDom(spPolygA1, A1, returnList = FALSE)
  myO_A1_by_spPolyg <- overIgnDom(spPolygA1, A1, returnList = FALSE)
  myO_A1_by_spPolyg2 <- overIgnDom(spPolygA1, A1, returnList = TRUE)
  myO_A1_by_spPolyg2 <- overIgnDom(spPolygA1[3], A1, returnList = TRUE, use.data=F)
  myO_A1_by_spPolyg2 <- overIgnDom(spPolygA1[3], A1, returnList = FALSE, use.data=F)
  myO_A1_by_spPolyg2 <- overIgnDom(spPolygA1[3], A1, returnList = TRUE, use.data=T, fn=mean)
  myO_A1_by_spPolyg2 <- overIgnDom(spPolygA1[3], A1, returnList = FALSE, use.data=T, fn=mean)
  myO_A1_by_spGrid <- overIgnDom(spGridA1, A1, returnList = FALSE)
  myO_A1_by_spPix <- overIgnDom(spPixA1, A1, returnList = FALSE)
  #expect_that(myO_A1_by_spPolyg, is_identical_to(o_A1_by_spPolyg))
  #expect_that(myO_A1_by_spPolyg, is_identical_to(myO_A1_by_spGrid))
  #expect_that(myO_A1_by_spPolyg, is_identical_to(myO_A1_by_spPix))
  expect_that(myO_A1_by_spPolyg, is_equivalent_to(o_A1_by_spPolyg))
  expect_that(myO_A1_by_spPolyg, is_equivalent_to(myO_A1_by_spGrid))
  expect_that(myO_A1_by_spPolyg, is_equivalent_to(myO_A1_by_spPix))
  # x = SpPolygDF
  o_A1_by_spPolygDF <- sp::over(spPolygA1_df, A1@sp, returnList = FALSE)
  myO_A1_by_spPolygDF <- overIgnDom(spPolygA1_df, A1, returnList = FALSE)
  myO_A1_by_spPolygDF <- overIgnDom(spPolygA1_df, A1, returnList = FALSE)
  #expect_that(myO_A1_by_spPolygDF, is_identical_to(o_A1_by_spPolygDF))
  expect_that(myO_A1_by_spPolygDF, is_equivalent_to(o_A1_by_spPolygDF))
  # SpPoints - !!! drelete ???
  o_A1_by_spPoints <- sp::over(a1_spP, A1@sp, returnList = FALSE)
  #myO_A1_by_spPoints <- overIgnDom(a1_spP, A1, returnList = FALSE)
  #myO_A1_by_spPoints <- overIgnDom(a1_spP, A1, returnList = FALSE) # no Method
  #all.equal(myO_A1_by_spPoints, o_A1_by_spPoints) # T
  #expect_that(myO_A1_by_spPoints, is_identical_to(o_A1_by_spPoints)) # F
  #expect_that(myO_A1_by_spPoints, equals(o_A1_by_spPoints)) # T
  
  # returnList = T
  o_A1_by_spPolyg_rl <- sp::over(spPolygA1, A1@sp, returnList = TRUE)
  myO_A1_by_spPolyg_rl <- overIgnDom(spPolygA1, A1, returnList = TRUE)
  myO_A1_by_spGrid_rl <- overIgnDom(spGridA1, A1, returnList = TRUE)
  myO_A1_by_spPix_rl <- overIgnDom(spPixA1, A1, returnList = TRUE)
  #expect_that(myO_A1_by_spPolyg_rl, is_identical_to(o_A1_by_spPolyg_rl))
  #expect_that(myO_A1_by_spPolyg_rl, is_identical_to(myO_A1_by_spGrid_rl))
  #expect_that(myO_A1_by_spPolyg_rl, is_identical_to(myO_A1_by_spPix_rl))
  expect_that(myO_A1_by_spPolyg_rl, is_equivalent_to(o_A1_by_spPolyg_rl))
  expect_that(myO_A1_by_spPolyg_rl, is_equivalent_to(myO_A1_by_spGrid_rl))
  expect_that(myO_A1_by_spPolyg_rl, is_equivalent_to(myO_A1_by_spPix_rl))
  #expect_that(myO_A1_by_spPolyg_rl, is_equivalent_to(o_A1_by_spPolyg_rl))
  # SpPoints
  #o_A1_by_spPoints_rl <- sp::over(a1_spP, A1@sp, returnList = TRUE)
  #myO_A1_by_spPoints_rl <- overIgnDom(a1_spP, A1, returnList = TRUE)
  #all.equal(myO_A1_by_spPoints_rl, o_A1_by_spPoints_rl) # T
  ##expect_that(myO_A1_by_spPoints, is_identical_to(o_A1_by_spPoints)) # F
  #expect_that(myO_A1_by_spPoints_rl, equals(o_A1_by_spPoints_rl)) # T
  
  # use.data = TRUE
  
  # returnList = TRUE, fn = NULL
  o_A1_by_spPolyg_rl_data <- sp::over(spPolygA1, as(A1, "SpatialPointsDataFrame"), returnList = TRUE)
  myO_A1_by_spPolyg_rl_data <- overIgnDom(spPolygA1, A1, returnList = TRUE, use.data = TRUE)
  for (i in seq_along(o_A1_by_spPolyg_rl_data)) {
    if (nrow(o_A1_by_spPolyg_rl_data[[i]]) > 0) {
      # i <- 2
      expect_that(o_A1_by_spPolyg_rl_data[[i]]$co2, 
                  is_equivalent_to(zoo::coredata(myO_A1_by_spPolyg_rl_data[[i]])[ ,"co2"]))
    }
    if (!is.na(myO_A1_by_spPolyg_rl_data[[i]][[1]])) {
      expect_that(o_A1_by_spPolyg_rl_data[[i]]$co2, 
                  is_equivalent_to(zoo::coredata(myO_A1_by_spPolyg_rl_data[[i]])[ ,"co2"]))
    }
  }
  myO_gen_A1_by_spPolyg_rl_data <- overIgnDom(spPolygA1, A1, returnList = TRUE, use.data = TRUE)
  #expect_that(myO_A1_by_spPolyg_rl_data, is_identical_to(myO_gen_A1_by_spPolyg_rl_data))
  expect_that(myO_A1_by_spPolyg_rl_data, is_equivalent_to(myO_gen_A1_by_spPolyg_rl_data))
  
  
  # returnList = FALSE, fn = NULL
  o_A1_by_spPolyg_data <- sp::over(spPolygA1, as(A1, "SpatialPointsDataFrame"), returnList = FALSE)
  myO_A1_by_spPolyg_data <- overIgnDom(spPolygA1, A1, returnList = FALSE, use.data=TRUE)
  identical(o_A1_by_spPolyg_data, myO_A1_by_spPolyg_data) # F --> row.names
  attr(o_A1_by_spPolyg_data, "row.names")
  attr(myO_A1_by_spPolyg_data, "row.names")
  # ...
  
  # returnList = FALSE, fn = !NULL 
  myO_gen_spPolyg_A1_mean_byT_1 <- overIgnDom(spPolygA1, A1_ext2, returnList = FALSE, 
                                        fn = mean, na.rm = T, 
                                        use.data = TRUE, weight.points = NULL)
  
  # Tests with vulture_moveStack data: huge computation time...
#   myO_gen_sp.stfTrcsX1_TX1_mean_byT_1 <- overIgnDom(stf_TrcsX1@sp, Tracks_X1[1], returnList = FALSE, 
#                                               fn = mean, na.rm = T, 
#                                               use.data = c("ground_speed"), weight.points = NULL) 
#   
#   myO_gen_sp.stfTrcsX1_TX1_mean_byT_2 <- overIgnDom(stf_TrcsX1@sp, Tracks_X1[1], returnList = FALSE, 
#                                               fn = mean, na.rm = T, 
#                                               use.data = c(1), weight.points = NULL) 
#   
#   myO_gen_sp.stfTrcsX1_TX1_mean_byT_3 <- overIgnDom(stf_TrcsX1@sp, Tracks_X1[1], returnList = FALSE, 
#                                               fn = mean, na.rm = T, 
#                                               use.data = 1, weight.points = NULL) 
#   
#   identical(myO_gen_sp.stfTrcsX1_TX1_mean_byT_1, myO_gen_sp.stfTrcsX1_TX1_mean_byT_2)
#   
#   myO_gen_sp.TrX1.1P_TX1_mean_byT_1 <- overIgnDom(spPolygTrX1.1, Tracks_X1[1], returnList = FALSE, 
#                                             fn = mean, na.rm = T, 
#                                             use.data = 1, weight.points = NULL) 
#   
#   myO_gen_sp.TrX1.1P_TX1_mean_byT_2 <- overIgnDom(spPolygTrX1.1, Tracks_X1[1], returnList = FALSE, 
#                                             fn = mean, na.rm = T, 
#                                             use.data = c(1:3), weight.points = NULL) 
#   
#   myO_gen_sp.TrX1.1P_TX1_mean_byT_3 <- overIgnDom(spPolygTrX1.1, Tracks_X1[1], returnList = FALSE, 
#                                             fn = mean, na.rm = T, 
#                                             use.data = c("ground_speed", "height_raw"), weight.points = NULL) 
#   
#   #myO_gen_sp.TrX1.1P_TX1_mean_byT_4 <- overIgnDom(spPolygTrX1.1, Tracks_X1[1], returnList = FALSE, 
#   #                                          fn = mean, na.rm = T, 
#   #                                          use.data = c("ground_speed", "fail", "height_raw"), weight.points = NULL) 
#   
#   myO_gen_sp.TrX1.1P_TX1_max_byT <- overIgnDom(spPolygTrX1.1, Tracks_X1[1], returnList = FALSE, 
#                                          fn = max, na.rm = T, 
#                                          use.data = c("ground_speed", "height_raw"), weight.points = NULL) 
#   
#   myO_gen_sp.TrX1.1P_TX1_median_byT <- overIgnDom(spPolygTrX1.1, Tracks_X1[1], returnList = FALSE, 
#                                             fn = median, na.rm = T, 
#                                             use.data = c("ground_speed", "height_above_ellipsoid"), weight.points = NULL) 
#   
#   myO_gen_sp.TrX1.1P_TX1_min_byT <- overIgnDom(spPolygTrX1.1, Tracks_X1[1], returnList = FALSE, 
#                                          fn = min, na.rm = T, 
#                                          use.data = c("ground_speed", "height_above_ellipsoid"), weight.points = NULL) 
#   
#   
#   # Fehler in as.POSIXct.numeric(value) : 'origin' must be supplied (Erledigt)
#   # Aber es darf eigentlcihauch fehelr gebn, da unsinning: no weights but weightinf fcts!!?!
#   myO_gen_sp.stfTrcsX1_TX1_wMedian_byT <- overIgnDom(stf_TrcsX1@sp, Tracks_X1[1], returnList = FALSE, 
#                                                fn = matrixStats::weightedMedian, na.rm = T, 
#                                                use.data = c("ground_speed"), weight.points = NULL) 
#   myO_gen_sp.stfTrcsX1_TX1_wMean_byT <- overIgnDom(stf_TrcsX1@sp, Tracks_X1[1], returnList = FALSE, 
#                                              fn = weighted.mean, na.rm = T, 
#                                              use.data = c("ground_speed"), weight.points = NULL) 
  
  ###
  # my trajaggr::over mit Track ohne Daten in @data slot und ohne stopifnot in code
  # rl = F
  my_over_test_mean_noRow <- overIgnDom(spPolygA, A1_noD, returnList=F, fn=mean, na.rm=T, use.data=T)
  str(my_over_test_mean_noRow); my_over_test_mean_noRow$co2 # NaN erledigt!
  my_over_test2 <- overIgnDom(spPolygA, A1_noD, returnList=F, fn=mean, na.rm=T, use.data="co2")
  str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  my_over_test2 <- overIgnDom(spPolygA, A1_noDnoC, returnList=F, fn=mean, na.rm=T, use.data=T)
  ## erledigt --> undefined columns selected in lapply(cleanIndexList, function(z) { dat <- y@data[z, use.data, drop = FALSE]
  str(my_over_test2); my_over_test2$co2 # TODO !!???
  my_over_test2 <- overIgnDom(spPolygA, A1_noDnoC, returnList=F, fn=mean, na.rm=T, use.data="co2")
  ## erledigt --> undefined columns selected in lapply(cleanIndexList, function(z) { dat <- y@data[z, use.data, drop = FALSE]
  str(my_over_test2); my_over_test2$co2 # TODO !!???
  my_over_test2 <- overIgnDom(spPolygA, A1_noC, returnList=F, fn=mean, na.rm=T, use.data=TRUE)
  # erledigt --> undefined columns selected in lapply(cleanIndexList, function(z) { dat <- y@data[z, use.data, drop = FALSE]
  str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  my_over_test2 <- overIgnDom(spPolygA, A1_noC, returnList=F, fn=mean, na.rm=T, use.data= "co2")
  # erledigt --> undefined columns selected in lapply(cleanIndexList, function(z) { dat <- y@data[z, use.data, drop = FALSE]
  str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  # rl = T
  my_over_test <- overIgnDom(spPolygA, A1_noD, returnList=T, fn=mean, na.rm=T, use.data=T)
  str(my_over_test); my_over_test[[1]]$co2 # 
  my_over_test2 <- overIgnDom(spPolygA, A1_noD, returnList=T, fn=mean, na.rm=T, use.data="co2")
  str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  my_over_test <- overIgnDom(spPolygA, A1_noC, returnList=T, fn=mean, na.rm=T, use.data=T)
  str(my_over_test); my_over_test[[1]]; class(my_over_test[[1]]) # NaN erledigt!
  my_over_test2 <- overIgnDom(spPolygA, A1_noC, returnList=T, fn=mean, na.rm=T, use.data="co2")
  str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  my_over_test <- overIgnDom(spPolygA, A1_noDnoC, returnList=T, fn=mean, na.rm=T, use.data=T)
  str(my_over_test); my_over_test$co2 # NaN erledigt!
  my_over_test2 <- overIgnDom(spPolygA, A1_noDnoC, returnList=T, fn=mean, na.rm=T, use.data="co2")
  str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  # rl = F, fn=NULL
  my_over_test <- overIgnDom(spPolygA, A1_noD, returnList=F, fn=NULL, na.rm=T, use.data=T)
  str(my_over_test); my_over_test$co2 # NaN erledigt!
  my_over_test2 <- overIgnDom(spPolygA, A1_noD, returnList=F, fn=NULL, na.rm=T, use.data="co2")
  str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  my_over_test <- overIgnDom(spPolygA, A1_noC, returnList=F, fn=NULL, na.rm=T, use.data=T)
  str(my_over_test); my_over_test # NaN erledigt!
  my_over_test2 <- overIgnDom(spPolygA, A1_noC, returnList=F, fn=NULL, na.rm=T, use.data="co2")
  str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  my_over_test <- overIgnDom(spPolygA, A1_noDnoC, returnList=F, fn=NULL, na.rm=T, use.data=T)
  str(my_over_test); my_over_test$co2 # NaN erledigt!
  my_over_test2 <- overIgnDom(spPolygA, A1_noDnoC, returnList=F, fn=NULL, na.rm=T, use.data="co2")
  str(my_over_test2); my_over_test2$co2 # NaN erledigt!
  
})
# 
