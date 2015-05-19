#test_file(paste(getwd(),"/tests/testthat/test_over_results.R", sep=""))
#===============================================================================
#
# JUST RUN THIS TEST FILE
#
#===============================================================================

#require(testthat)
#test_file(paste(getwd(),"/tests/testthat/test_over_results.R", sep=""))

#-------------------------------------------------------------------------------

# Need to load data...
#load("trajaggr_TestData.RData", verbose=TRUE)
#load("tests/testthat/trajaggr_TestData.RData", verbose=TRUE)

load(system.file("extdata","trajaggr_TestData.RData", 
                 package = "trajaggr"), verbose = TRUE)

#===============================================================================
#
context("Test of over with x = STF object from over.R: \n")
#
#===============================================================================

test_that("Tests of over with populated data.frame objects in data slot ...", {
  
  # stf_Polys_4t, Track
  Track_A1@time
  stf_Polys_4t@time
  ovr_indV <- over(stf_Polys_4t, Track_A1, returnList = FALSE)
  ovr_indL <- over(stf_Polys_4t, Track_A1, returnList = TRUE)
  ovr_indL[1:3]
  ovr_data <- over(stf_Polys_4t, Track_A1, returnList = TRUE, use.data = TRUE)
  ovr_data[1:3]
  ovr_agg <- over(stf_Polys_4t, Track_A1, returnList = FALSE, fn = mean, use.data = TRUE)
  ovr_agg[1:3, ]
  ovr_agg_weighted <- over(stf_Polys_4t, Track_A1, returnList = FALSE, 
                           fn = weighted.mean, use.data = TRUE, 
                           weight.points = "byTime")
  ovr_agg_weighted[1:3, ]

})