#test_file(paste(getwd(),"/tests/testthat/test_aggregate_by_STF_results.R", sep=""))
#===============================================================================
#
# JUST RUN THIS TEST FILE
#
#===============================================================================

#require(testthat)
#test_file(paste(getwd(),"/tests/testthat/test_aggregate_by_STF_results.R", sep=""))

#-------------------------------------------------------------------------------

# Need to load data...
#load("trajaggr_TestData.RData", verbose=TRUE)
#load("tests/testthat/trajaggr_TestData.RData", verbose=TRUE)

load(system.file("extdata","trajaggr_TestData.RData", 
                 package = "trajaggr"), verbose = TRUE)

#===============================================================================
#
context("Test of aggregate_Track_STF in aggregate_by_STF.R: \n")
#
#===============================================================================

test_that("Tests of aggregate_Track_STF with populated data.frame objects in data slot ...", {
  
  # 1 Track, 1 cell - Erste simple Versuche
  # Track_A1, P3
  agg <- aggregate(Track_A1, stf_P3_1t, mean, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(6))
  agg <- aggregate(Track_A1, stf_P3_1t, mean, simplify = FALSE, use.data = "co2", 
                   weight.tracks = "equal")
  expect_that(agg@data$co2, is_identical_to(6))
  agg <- aggregate(Track_A1, stf_P3_1t, weighted.mean, simplify = FALSE, 
                   weight.points = "byTime")
  expect_that(agg@data$co2, is_identical_to(48/10))
  agg <- aggregate(Track_A1, stf_P3_1t, median, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(6))
  agg <- aggregate(Track_A1, stf_P3_1t, max, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(8))
  agg <- aggregate(Track_A1, stf_P3_1t, min, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(4))
  
  # Track_A2, P2
  agg <- aggregate(Track_A2, stf_P2_1t, mean, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(8))
  agg <- aggregate(Track_A2, stf_P2_1t, median, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(8))
  agg <- aggregate(Track_A2, stf_P2_1t, max, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(12))
  
  # Div
  agg <- aggregate(Track_A2, stf_P3_1t, mean, na.rm=T, simplify = FALSE)
  expect_that(is.na(agg@data$co2), is_identical_to(TRUE))
  agg <- aggregate(Track_B1, stf_P2_1t, mean, na.rm=T, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(4))
  
  # Several cells
  # Track_A1, Polys
  agg <- aggregate(Track_A1, stf_Polys_1t, mean, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(c(8,12,6,4)))
  agg <- aggregate(Track_A1, stf_Polys_2t, mean, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(c(8,12,6,4,NA,NA,NA,NA)))
  agg <- aggregate(Track_A1, stf_Polys_4t, mean, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(c(8,NA,6,NA,NA,12,NA,4,NA,NA,NA,NA,NA,NA,NA,NA)))
  
  
  # MUELL
  agg <- aggregate(Track_A1, stf_P3_1t, simplify = FALSE, weight.tracks = "byTime")
  expect_that(agg@data$co2, is_identical_to(6))
  agg <- aggregate(Track_A1, stf_P3_1t, simplify = FALSE, weight.tracks = "equal")
  expect_that(agg@data$co2, is_identical_to(6))
    
})
  
test_that("Tests of aggregate_Tracks_STF with populated data.frame objects in data slot ...", {
  
  # Tracks_A, 1 cell
  agg <- aggregate(Tracks_A, stf_P3_1t, mean, na.rm=T, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(c(6)))
  agg <- aggregate(Tracks_A, stf_P4_1t, mean, na.rm=T, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(c(4)))
  agg <- aggregate(Tracks_A, stf_P4_1t, mean, simplify = FALSE)
  expect_that(is.na(agg@data$co2), is_identical_to(TRUE))
  agg <- aggregate(Tracks_A, stf_P2_1t, mean, na.rm=T, simplify = FALSE)
  expect_that(agg@data$co2, is_identical_to(c(10)))
  agg <- aggregate(Tracks_A, stf_P2_1t, weighted.mean, na.rm=T, 
                   simplify = FALSE, weight.tracks = "byTime")
  expect_that(agg@data$co2, is_identical_to(208/22))
  agg <- aggregate(Tracks_A, stf_P2_1t, weighted.mean, na.rm=T, 
                   simplify = FALSE, weight.points = "byTime")
  expect_that(agg@data$co2, is_identical_to((104/14 + 12)/2))
  
  
})