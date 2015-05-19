#test_file(paste(getwd(),"/tests/testthat/test_aggregate_by_STF.R", sep=""))
#===============================================================================
#
# JUST RUN THIS TEST FILE
#
#===============================================================================

#require(testthat)
#test_file(paste(getwd(),"/tests/testthat/test_aggregate_by_STF.R", sep=""))

#-------------------------------------------------------------------------------

# Need to load data...
#load("trajaggr_TestData.RData")
#load("tests/testthat/trajaggr_TestData.RData")
#source("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/trajaggr/tests/testthat/createData_overAndAggTests.R")
#source("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/trajaggr/tests/testthat/_createData_overAndAggTests_old.R")

source("createData_overAndAggTests.R")


# aggA1 <- aggregate(A1, stfP)
# aggA1_w_byTime <- aggregate(A1, stfP, weighted.mean, weight.points = "byTime")
# aggA1_w_byDist <- aggregate(A1, stfP, weighted.mean, weight.points = "byDist")
# str(aggA1@data)
# str(aggA1_w_byTime@data)
# str(aggA1_w_byLength@data)
# identical(aggA1_w_byTime@data, aggA1_w_byLength@data)
# aggA1_w_byTime@data[1:10,]
# aggA1_w_byLength@data[1:10,]

######
# by = sp
agg_A1_sp_metad <- aggregate(A1, spPolygTr, mean, na.rm=T, use.data = TRUE)
agg_A1_sp_metad@data # --> check rownames
agg_1Tr_sp_metad <- aggregate(Tr[1][1], spPolygTr, mean, na.rm=T, use.data = TRUE)
class(spPolygTr)
agg_1Tr_sp_metad@data # --> check rownames
agg_Trcs_sp_metad <- aggregate(Tr[1], spPolygTr, mean, na.rm=T, use.data = TRUE)
agg_Trcs_sp_metad@data # --> rownames ok!
agg_TrColl_sp_metad <- aggregate(Tr, spPolygTr, mean, na.rm=T, use.data = TRUE, byID=F)
agg_TrColl_sp_metad@data # --> rownames ok!
agg_TrColl_sp_byID_metad <- aggregate(Tr, spPolygTr, mean, na.rm=T, use.data = TRUE, byID=T)
agg_TrColl_sp_byID_metad@data # --> rownames ok!
agg_TrColl_spA_metad <- aggregate(Tr, spPolygA, mean, na.rm=T, use.data = TRUE, byID=F)
agg_TrColl_spA_metad@data # --> rownames ok!
agg_TrColl_spA_byID_metad <- aggregate(Tr, spPolygA, mean, na.rm=T, use.data = TRUE, byID=T)
agg_TrColl_spA_byID_metad@data # --> rownames ok!

# by = stf
aggA1_stfP_A <- aggregate(A1, stfP_A)
aggA1_stfP_A@data
print(class(aggA1_stfP_A))
aggA_stfP_A <- aggregate(A, stfP_A, na.rm=T)
aggA_stfP_A@data

# !!! erledigt: warning
aggTr_stfP_A <- aggregate(Tr, stfP_A, na.rm=T, byID=F) # --> warning ?!
aggTr_stfP_A@data
## ERLEDIGT: --> number of rows of result is not a multiple of vector length (arg 3)
# !!!
#withCallingHandlers(aggregate(Tr, stfP_A, na.rm=T, byID=F), warning=function(c) recover())
aggA_stfP_A@data

# !!! erledigt: error!!!
#length(stfP_A)
aggTr_stfP_A <- aggregate(Tr, stfP_A, na.rm=T, byID=T) # --> ERROR ?!
aggTr_stfP_A@data
##ERLEDIGT: Fehler in data.frame(res_df, metadata, row.names = rNames) : arguments imply differing number of rows: 45, 54
#o <- over(stfP_A, Tr[2][1], returnList = FALSE, fn = mean, na.rm=T, use.data = T)
#nrow(o); length(stfP_A) # --> 45, 54 --> ???
#o2 <- over(stfP_A, Tr[2][1], returnList = TRUE, fn = mean, na.rm=T, use.data = T)
#length(o2) # 45 ???
#oind <- over(stfP_A, Tr[2][1], returnList = F, fn = mean, na.rm=T, use.data = F)
#length(oind) # 45
#oindrl <- over(stfP_A, Tr[2][1], returnList = T, fn = mean, na.rm=T, use.data = F)
#length(oindrl) # 45
###

aggA1_stfP_A <- aggregate(A1_noD, stfP_A)
aggA1_stfP_A <- aggregate(A1_noC, stfP_A)
aggA1_stfP_A <- aggregate(A1_noDnoC, stfP_A)
aggA1_stfP_A <- aggregate(A1, stfP_A)

##plot(as(A1, "SpatialPointsDataFrame"))
##plot(spPolygA)

# test_t1_sp <- aggA1_stfP_A[ , 1, "co2"]
# class(test_t1_sp)
# spplot(test_t1_sp)
# test_t1_sp@data
# 
# test_t1_st <- aggA1_stfP_A[ , 1, "co2", drop = FALSE]
# class(test_t1_st)
# stplot(test_t1_st)
# test_t1_st@data
# 
# stplot(aggA1_stfP_A)
# aggA1_stfP_A@data


###
#
# Testing of aggregation of Tracks with different variables in track data
#
#
#===============================================================================
#
context("Test of aggregate_Tracks_STF in aggregate_by_STF.R: \n")
#
#===============================================================================
# 
# ###message(paste("#####  ... #####\n", sep=""))
#
# 
test_that("# Tests of aggregate_Tracks_STF with populated data.frame objects in data slot ...", {
  
  # Without Weighting; just "legal" / populated dfs in data slot
  # A_ext_inhomogen2 # co2 vs co2, test
  agg_A_udT_inh2 <- aggregate(A_ext_inhomogen2, stfP_A, mean, na.rm=T, use.data=T)
  agg_A_udcharall_inh2 <- aggregate(A_ext_inhomogen2, stfP_A, mean, na.rm=T, use.data=c("co2", "test"))
  expect_that(agg_A_udT_inh2@data, is_identical_to(agg_A_udcharall_inh2@data))
  agg_A_udcharco2_inh2 <- aggregate(A_ext_inhomogen2, stfP_A, mean, na.rm=T, use.data=c("co2"))
  expect_that(agg_A_udT_inh2@data$co2, is_identical_to(agg_A_udcharco2_inh2@data$co2))
  agg_A_udchartest_inh2 <- aggregate(A_ext_inhomogen2, stfP_A, mean, na.rm=T, use.data=c("test"))  
  expect_that(agg_A_udT_inh2@data$test, is_identical_to(agg_A_udchartest_inh2@data$test))
  expect_that(agg_A_udT_inh2@data$approx_duration,
              is_identical_to(agg_A_udcharall_inh2@data$approx_duration))
  expect_that(agg_A_udT_inh2@data$approx_duration, 
              is_identical_to(agg_A_udcharco2_inh2@data$approx_duration))
  expect_that(agg_A_udT_inh2@data$approx_duration, 
              is_identical_to(agg_A_udchartest_inh2@data$approx_duration))
  # Error
  #agg_A_udcharall_inh2 <- aggregate(A_ext_inhomogen2, stfP_A, mean, na.rm=T, use.data=c("co2", "t"))
  
  # A_ext_inhomogen3 # co2 vs co2, test, char, fac
  agg_A_udT_inh3 <- aggregate(A_ext_inhomogen3, stfP_A, mean, na.rm=T, use.data=T)
  agg_A_udcharall_inh3 <- aggregate(A_ext_inhomogen3, stfP_A, mean, na.rm=T, use.data=c("co2", "test", "char", "fac"))
  expect_that(agg_A_udT_inh3@data, is_identical_to(agg_A_udcharall_inh3@data))
  agg_A_udcharco2_inh3 <- aggregate(A_ext_inhomogen3, stfP_A, mean, na.rm=T, use.data=c("co2"))
  expect_that(agg_A_udT_inh3@data$co2, is_identical_to(agg_A_udcharco2_inh3@data$co2))
  agg_A_udchartest_inh3 <- aggregate(A_ext_inhomogen3, stfP_A, mean, na.rm=T, use.data=c("test"))
  expect_that(agg_A_udT_inh3@data$test, is_identical_to(agg_A_udchartest_inh3@data$test))
  expect_that(agg_A_udT_inh3@data$approx_duration,
              is_identical_to(agg_A_udcharall_inh3@data$approx_duration))
  expect_that(agg_A_udT_inh3@data$approx_duration, 
              is_identical_to(agg_A_udcharco2_inh3@data$approx_duration))
  expect_that(agg_A_udT_inh3@data$approx_duration, 
              is_identical_to(agg_A_udchartest_inh3@data$approx_duration))
  # Error
  #agg_A_udcharall_inh3 <- aggregate(A_ext_inhomogen3, stfP_A, mean, na.rm=T, use.data=c("co2", "t", "char", "fac"))
  
  # A_ext_inhomogen4 # test vs co2, test, char, fac
  agg_A_udT_inh4 <- aggregate(A_ext_inhomogen4, stfP_A, mean, na.rm=T, use.data=T)
  agg_A_udcharall_inh4 <- aggregate(A_ext_inhomogen4, stfP_A, mean, na.rm=T, use.data=c("co2", "test", "char", "fac"))
  expect_that(agg_A_udT_inh4@data$co2, is_identical_to(agg_A_udcharall_inh4@data$co2))
  expect_that(agg_A_udT_inh4@data$test, is_identical_to(agg_A_udcharall_inh4@data$test))
  expect_that(agg_A_udT_inh4@data[3:7], is_identical_to(agg_A_udcharall_inh4@data[3:7]))
  agg_A_udcharco2_inh4 <- aggregate(A_ext_inhomogen4, stfP_A, mean, na.rm=T, use.data=c("co2"))
  expect_that(agg_A_udT_inh4@data$co2, is_identical_to(agg_A_udcharco2_inh4@data$co2))
  agg_A_udchartest_inh4 <- aggregate(A_ext_inhomogen4, stfP_A, mean, na.rm=T, use.data=c("test"))
  expect_that(agg_A_udT_inh4@data$test, is_identical_to(agg_A_udchartest_inh4@data$test))
  expect_that(agg_A_udT_inh4@data$approx_duration,
              is_identical_to(agg_A_udcharall_inh4@data$approx_duration))
  expect_that(agg_A_udT_inh4@data$approx_duration, 
              is_identical_to(agg_A_udcharco2_inh4@data$approx_duration))
  expect_that(agg_A_udT_inh4@data$approx_duration, 
              is_identical_to(agg_A_udchartest_inh4@data$approx_duration))
  # Error
  #agg_A_udcharall_inh4 <- aggregate(A_ext_inhomogen4, stfP_A, mean, na.rm=T, use.data=c("co2", "t", "char", "fac"))
  
})

#
# Without Weighting
#
#agg_A_udT_inh1 <- aggregate(A_ext_inhomogen1, stfP_A, mean, na.rm=T, use.data=T)
#agg_A_udT_inh1@data[1:5,]
## !!!
# (bedingt erledigt) !!! wenn nrow=0 Tracks nicht ausgeschlossen:
# Error: Fehler in `[<-.data.frame`(`*tmp*`, allNames[w[i]], value = NA) : Ersetzung hat 1 Zeile, Daten haben 0
# erledigt: Außerdem Warnings: leerer Df; --> keine metadata für 1. Track
# ERLEDIGT ###[Error in `[.data.frame`(z, curName) : undefined columns selected: in z[curName]] 
# My Warning: Track obj excluded / Empty data slot in Track object. Empty data.frame is used.
# --> co2 + test für 2. track: 
# ok
agg_A_udT_inh2 <- aggregate(A_ext_inhomogen2, stfP_A, mean, na.rm=T, use.data=T)
# ERLEDIGT ###Error in `[.data.frame`(z, curName) : undefined columns selected: in z[curName]
# ok
agg_A_udT_inh3 <- aggregate(A_ext_inhomogen3, stfP_A, mean, na.rm=T, use.data=T)
agg_A_udT_inh3@data[1:5,]
# ERLEDIGT ###Error in `[.data.frame`(z, curName) : undefined columns selected: in z[curName]
# ok
agg_A_udT_inh4 <- aggregate(A_ext_inhomogen4, stfP_A, mean, na.rm=T, use.data=T)
agg_A_udT_inh4@data[1:5,]
#agg_A_udT_inh5 <- aggregate(A_ext_inhomogen5, stfP_A, mean, na.rm=T, use.data=T)
#agg_A_udT_inh5@data[1:5,]
# !!! 
# wenn nrow=0 Tracks nicht ausgeschlossen:
# Außerdem Warnings: leerer Df; --> keine metadata für 1. Track
# Error: Fehler in `[<-.data.frame`(`*tmp*`, allNames[w[i]], value = NA) : Ersetzung hat 1 Zeile, Daten haben 0
agg_A_udT_inh6 <- aggregate(A_ext_inhomogen6, stfP_A, mean, na.rm=T, use.data=T) # no cols!!!
agg_A_udT_inh6@data[1:5,]
sum(A_ext_inhomogen6@tracksData$n) # Summe n = 12
sum(agg_A_udT_inh6@data$nlocs) # 12 (mit / ohne Ausschluss von 0-row-track obj.)
sum(agg_A_udT_inh6@data$approx_duration) # 211,7859 (mit / ohne Ausschluss von 0-row-track obj.)
length(agg_A_udT_inh6@data); head(agg_A_udT_inh6@data)
# ok !?

#agg_A_udchar_inh1 <- aggregate(A_ext_inhomogen1, stfP_A, mean, na.rm=T, use.data=c("co2", "test"))
# wenn nrow=0 Tracks nicht ausgeschlossen:
# Error: Fehler in `[<-.data.frame`(`*tmp*`, allNames[w[i]], value = NA) : Ersetzung hat 1 Zeile, Daten haben 0
# Außerdem Warnings: leerer Df; --> keine meadata fuer 1. Track
# ERLEDIGT ###Error in `[.data.frame`(z, curName) : undefined columns selected: in z[curName]
# My Warning: Empty data slot in Track object. Empty data.frame is used.
# --> co2 + test für 2. track: 
# ok
agg_A_udchar_inh2 <- aggregate(A_ext_inhomogen2, stfP_A, mean, na.rm=T, use.data=c("co2", "test"))
# !! Error in `[.data.frame`(y@data, z, use.data, drop = FALSE) : undefined columns selected: in
# `[.data.frame`(y@data, z, use.data, drop = FALSE) at a_over.R#624
# --> TODO !!! ???
agg_A_udchar_inh3 <- aggregate(A_ext_inhomogen3, stfP_A, mean, na.rm=T, use.data=c("co2", "test"))
# !! Error in [.data.frame`(y@data, z, use.data, drop = FALSE) : undefined columns selected: in
# `[.data.frame`(y@data, z, use.data, drop = FALSE) at a_over.R#624
# --> TODO !!! ???
agg_A_udchar_inh4 <- aggregate(A_ext_inhomogen4, stfP_A, mean, na.rm=T, use.data=c("co2", "test"))
#agg_A_udchar_inh5 <- aggregate(A_ext_inhomogen5, stfP_A, mean, na.rm=T, use.data=c("co2", "test"))
# s.o. !!!!
# wenn nrow=0 Tracks nicht ausgeschlossen:
# Error: Fehler in `[<-.data.frame`(`*tmp*`, allNames[w[i]], value = NA) : Ersetzung hat 1 Zeile, Daten haben 0
agg_A_udchar_inh6 <- aggregate(A_ext_inhomogen6, stfP_A, mean, na.rm=T, use.data=c("co2", "test"))

#agg_A_udchar2_inh1 <- aggregate(A_ext_inhomogen1, stfP_A, mean, na.rm=T, use.data=c("co2"))
# 4 warnings !!! --> ??? ua. sollte nicht mehr vorkommen !? --> Check: 
# ERLEDIGT ###Error in `[.data.frame`(z, curName) : undefined columns selected: in z[curName]
# My Warning: Empty data slot in Track object. Empty data.frame is used.
# --> co2 für 2. track: 
# ok
agg_A_udchar2_inh2 <- aggregate(A_ext_inhomogen2, stfP_A, mean, na.rm=T, use.data=c("co2"))
# ok
agg_A_udchar2_inh3 <- aggregate(A_ext_inhomogen3, stfP_A, mean, na.rm=T, use.data=c("co2"))
# ok
agg_A_udchar2_inh4 <- aggregate(A_ext_inhomogen4, stfP_A, mean, na.rm=T, use.data=c("co2"))
#agg_A_udchar2_inh5 <- aggregate(A_ext_inhomogen5, stfP_A, mean, na.rm=T, use.data=c("co2"))
# wenn nrow=0 Tracks nicht ausgeschlossen:
# Error: Fehler in `[<-.data.frame`(`*tmp*`, allNames[w[i]], value = NA) : Ersetzung hat 1 Zeile, Daten haben 0
agg_A_udchar2_inh6 <- aggregate(A_ext_inhomogen6, stfP_A, mean, na.rm=T, use.data=c("co2"))


#
# Weighting!!
#
# use.data=T
#agg_A_udT_inh1_w <- aggregate(A_ext_inhomogen1, stfP_A, weighted.mean, na.rm=T, use.data=T, 
#                              weight.points = "byTime", weight.tracks = "byTime")
# !!!! Error in in x * w : nicht-numerisches Argument für binären Operator in 
# weighted.mean.default(zz, weightsList[[z]], ...) --> ??? evtl weights aber keine Daten !?
# My Warning: Empty data slot in Track object. Empty data.frame is used.
# --> TODO ?!
agg_A_udT_inh2_w <- aggregate(A_ext_inhomogen2, stfP_A, weighted.mean, na.rm=T, use.data=T, 
                              weight.points = "byTime", weight.tracks = "byTime")
# !!!! Error in Fehler in weighted.mean.default(attrMatrix[z, ], weightsList[[z]], ...) : 
# 'x' and 'w' must have the same length in lapply(1:nrow(attrMatrix), function(z)...
# --> TODO ?!
agg_A_udT_inh3_w <- aggregate(A_ext_inhomogen3, stfP_A, weighted.mean, na.rm=T, use.data=T, 
                              weight.points = "byTime", weight.tracks = "byTime")
# !!!! Error in Fehler in weighted.mean.default(attrMatrix[z, ], weightsList[[z]], ...) : 
# 'x' and 'w' must have the same length in lapply(1:nrow(attrMatrix), function(z)...
# --> TODO ?!
agg_A_udT_inh4_w <- aggregate(A_ext_inhomogen4, stfP_A, weighted.mean, na.rm=T, use.data=T, 
                              weight.points = "byTime", weight.tracks = "byTime")
# !!!! Error in Fehler in weighted.mean.default(attrMatrix[z, ], weightsList[[z]], ...) : 
# 'x' and 'w' must have the same length in lapply(1:nrow(attrMatrix), function(z)...
# --> TODO ?!
#agg_A_udT_inh5_w <- aggregate(A_ext_inhomogen5, stfP_A, weighted.mean, na.rm=T, use.data=T, 
#                              weight.points = "byTime", weight.tracks = "byTime")
agg_A_udT_inh6_w <- aggregate(A_ext_inhomogen6, stfP_A, weighted.mean, na.rm=T, use.data=T, 
                              weight.points = "byTime", weight.tracks = "byTime")
#agg_A_udT_inh7_w <- aggregate(A_ext_inhomogen7, stfP_A, weighted.mean, na.rm=T, use.data=T, 
#                              weight.points = "byTime", weight.tracks = "byTime")
# --> Error: colSums(z, na.rm = T) at a_aggregate_by_STF.R#480: stop("'x' must be an array of at least two dimensions")
#agg_A_udT_inh8_w <- aggregate(A_ext_inhomogen8, stfP_A, weighted.mean, na.rm=T, use.data=T, 
#                              weight.points = "byTime", weight.tracks = "byTime")
#agg_A_udT_inh8_w@data[1:5,]
# --> just metadata in 1 Track
#agg_A_udT_inh8_w@data
#agg_A_udT_inh9_w <- aggregate(A_ext_inhomogen9, stfP_A, weighted.mean, na.rm=T, use.data=T, 
#                              weight.points = "byTime", weight.tracks = "byTime")
# --> Error: colSums(z, na.rm = T) at a_aggregate_by_STF.R#480: stop("'x' must be an array of at least two dimensions")
#agg_A_udT_inh10_w <- aggregate(A_ext_inhomogen10, stfP_A, weighted.mean, na.rm=T, use.data=T, 
#                              weight.points = "byTime", weight.tracks = "byTime")
# (erledigt) !! stop: duerfte nicht mehr vorommen???!!!
agg_A_udT_inh11_w <- aggregate(A_ext_inhomogen11, stfP_A, weighted.mean, na.rm=T, use.data=T, 
                               weight.points = "byTime", weight.tracks = "byTime")
##agg_A_udT_inh11_w <- aggregate(A_ext_inhomogen11, stfP_A, weighted.mean, na.rm=T, use.data= c("co2"), 
##                               weight.points = "byTime", weight.tracks = "byTime")
agg_A_udT_inh11_w@data
# --> just metadata of 2 / all Tracks

# use.data=c("co2", "test")
#agg_A_udchar_inh1_w <- aggregate(A_ext_inhomogen1, stfP_A, weighted.mean, na.rm=T, 
#                                 use.data=c("co2", "test"), weight.points = "byTime", weight.tracks = "byTime")
# My Warning: Empty data slot in Track object. Empty data.frame is used.
# --> co2 + test für 2. track: 
# ok
agg_A_udchar_inh2_w <- aggregate(A_ext_inhomogen2, stfP_A, weighted.mean, na.rm=T, 
                                 use.data=c("co2", "test"), weight.points = "byTime", weight.tracks = "byTime")
# !!!! Error in [.data.frame`(y@data, cleanIndexList[[z]], use.data, drop = FALSE) : undefined columns selected
# in lapply(seq_along(cleanIndexList), function(z) {...
# --> TODO !!! ???
agg_A_udchar_inh3_w <- aggregate(A_ext_inhomogen3, stfP_A, weighted.mean, na.rm=T, 
                                 use.data=c("co2", "test"), weight.points = "byTime", weight.tracks = "byTime")
# !!!! Error in [.data.frame`(y@data, cleanIndexList[[z]], use.data, drop = FALSE) : undefined columns selected
# in lapply(seq_along(cleanIndexList), function(z) { ...
# --> TODO !!! ???
agg_A_udchar_inh4_w <- aggregate(A_ext_inhomogen4, stfP_A, weighted.mean, na.rm=T, 
                                 use.data=c("co2", "test"), weight.points = "byTime", weight.tracks = "byTime")


#agg_A_udchar2_inh1_w <- aggregate(A_ext_inhomogen1, stfP_A, weighted.mean, na.rm=T, 
#                                 use.data=c("co2"), weight.points = "byTime", weight.tracks = "byTime")
# My Warning: Empty data slot in Track object. Empty data.frame is used.
# --> co2 für 2. track: 
# ok
agg_A_udchar2_inh2_w <- aggregate(A_ext_inhomogen2, stfP_A, weighted.mean, na.rm=T, 
                                  use.data=c("co2"), weight.points = "byTime", weight.tracks = "byTime")
# ok
agg_A_udchar2_inh3_w <- aggregate(A_ext_inhomogen3, stfP_A, weighted.mean, na.rm=T, 
                                  use.data=c("co2"), weight.points = "byTime", weight.tracks = "byTime")
# ok


#
# Test STF with pixel/grid vs polygons
#
agg_A_STFPolyg_w <- aggregate(A_ext2, stfP_A, weighted.mean, na.rm=T, 
                                  use.data=c("co2", "test"), weight.points = "byTime", weight.tracks = "byTime")
agg_A_STFGridPix_w <- aggregate(A_ext2, stfG_A, weighted.mean, na.rm=T, 
                              use.data=c("co2", "test"), weight.points = "byTime", weight.tracks = "byTime")
#identical(agg_A_STFPolyg_w@data, agg_A_STFGridPix_w@data) # TRUE !

agg_A_STFPolyg_w <- aggregate(Tr, stfP_Tr, weighted.mean, na.rm=T, 
                              use.data=TRUE, weight.points = "byTime", weight.tracks = "byTime")
#######################################################
#------------------------------------------------------
# Test aggregate("Tracks", ...)
#------------------------------------------------------

# tests sieh auch movebank_gyps_exdatacreation_prep..

agg_A_stfP_A_mean <- aggregate(A, stfP_A, mean, na.rm = TRUE); agg_A_stfP_A_mean@data[1:6,]
agg_A_stfP_A_wmean <- aggregate(A, stfP_A, weighted.mean, na.rm = TRUE); agg_A_stfP_A_wmean@data[1:6,]
#agg_A_stfP_A_cwmean <- aggregate(A, stfP_A, 
#                                 matrixStats::weightedMedian, na.rm = T); agg_A_stfP_A_cwmean@data[1:6,]
# --> 20141125: colWeightedMeans funzt nicht, aber weightedMedian! (und weighted.mean)

#muell
#agg_envT_stfEnvP_mean <- aggregate(envT, stfEnvP, mean, na.rm = TRUE); agg_A_stfP_A_mean@data[1:6,]
#agg_A_stfP_A_wmean <- aggregate(A, stfP_A, weighted.mean, na.rm = TRUE); agg_A_stfP_A_wmean@data[1:6,]
#agg_A_stfP_A_cwmean <- aggregate(A, stfP_A, 
#                                 matrixStats::weightedMedian, na.rm = T); agg_A_stfP_A_cwmean@data[1:6,]

#!!!
# test weighting with equal weights against unweighted
agg_TestWeighting1_unw <- aggregate(A_ext, stfP_A, FUN = mean, na.rm=T, simplify = TRUE, use.data = TRUE,
                                weight.points = NULL, weight.tracks = NULL)
agg_TestWeighting1_wmean <- aggregate(A_ext, stfP_A, FUN = weighted.mean, na.rm=T, simplify = TRUE, use.data = TRUE,
                                weight.points = "equal", weight.tracks = "equal")
agg_TestWeighting1_wmean_noW <- aggregate(A_ext, stfP_A, FUN = weighted.mean, na.rm=T, simplify = TRUE, use.data = TRUE,
                                      weight.points = NULL, weight.tracks = NULL)
#expect_that(agg_TestWeighting1_unw, is_identical_to(agg_TestWeighting1_wmean))
expect_that(agg_TestWeighting1_unw, equals(agg_TestWeighting1_wmean))
#expect_that(agg_TestWeighting1_unw, is_identical_to(agg_TestWeighting1_wmean_noW))
expect_that(agg_TestWeighting1_unw, equals(agg_TestWeighting1_wmean_noW))


#######################################################
#------------------------------------------------------
# Test aggregate("TracksCollection", ...)
#------------------------------------------------------

agg_Tr_stfP_Tr_byID <- aggregate(Tr, stfP_Tr, mean, byID=T)
