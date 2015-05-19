#test_file(paste(getwd(),"/tests/testthat/test_moveCoercion.R", sep=""))
#===============================================================================
#
# JUST RUN THIS TEST FILE
#
#===============================================================================

require(sp) 
#require(testthat)
#test_file(paste(getwd(),"/tests/testthat/test_moveCoercion.R", sep=""))

#-------------------------------------------------------------------------------

print(Sys.time())

# devtools::use_data(leroy, leroy) # ok
# devtools::use_data(leroy_bursts_unique, leroy_bursts_unique) # ok
# devtools::use_data(leroy_bursts_rep, leroy_bursts_rep) # ok
# devtools::use_data(leroy_shift, leroy_shift) # ok
# devtools::use_data(leroyMoveStack, leroyMoveStack) # ok


#require(move)

#require(spacetime) # STIDF()
#require(trajectories) # Track()
# #???
# #require(xts)
# #require(circular) # --> summary(moveobject)

#require(testthat)


# ####
# 
# #!!! nur wenn ich skript direkt ausführen möchte:
# #(sonst automatisch!)
# require(testthat)
# 
# #!!! nur wenn ich skript direkt ausführen möchte:
# #(sonst automatisch)
# source("/home/harry/BSc_Thesis_Traj/R/R_wd/Exploring_PkgCreation/trajcoer_test/trajcoert01/R/moveCoercion.R")
# 
# 
# !!!
# #Load example data
# #movebank_BCl_Ocelot_ExData.RData for direct use (development)
# load("/home/harry/BSc_Thesis_Traj/R/R_wd/Exploring_PkgCreation/trajcoer_test/trajcoert01/tests/testthat/movebank_BCl_Ocelot_ExData.RData")
# ##move_leroy_extendedOffExData.RData
# ##load("/home/harry/BSc_Thesis_Traj/R/R_wd/Exploring_PkgCreation/trajcoer_test/trajcoert01/tests/testthat/move_leroy_extendedOffExData.RData")
# # !!! # move_pkg_ExData.RData
# load(paste(getwd(), "/tests/testthat/", "move_pkg_ExData.RData", sep=""))
#
# ####


#####

# Load example data from package move
# leroy <- move::move(system.file("extdata", "leroy.csv.gz", package = "move"))

#####

# !!!!!
# Load test data --> testthat needs this data for tests! (Achtung pfad !?)
# Achtung: bobbyMove etc eignet sich nicht für finales package wg copyright !?!?!
# --> TODO ! ??? !
#load("move_pkg_ExData.RData")
#load("movebank_BCl_Ocelot_ExData.RData")
#load("move_leroy_extendedOffExData.RData")

#####


########################


###save
#(leroy, leroyBursts1, leroyBursts2, leroyBursts3, file="move_leroy_extendedOffExData.RData")

#(bobbyMove, bclOcelotMoveStack, file="movebank_BCl_Ocelot_ExData.RData")
#load("movebank_BCl_Ocelot_ExData.RData")
##load(paste("/home/harry/BSc_Thesis_Traj/R/R_wd/Exploring_PkgCreation/trajcoer_test/trajcoert01",
##           "/tests/testthat/", "movebank_BCl_Ocelot_ExData.RData", sep=""), verbose = TRUE)
#load(paste(getwd(), "/tests/testthat/", "movebank_BCl_Ocelot_ExData.RData", sep=""), verbose = TRUE)

load("move_pkg_ExData.RData", verbose=T)
#load(paste(getwd(), "/tests/testthat/", "move_pkg_ExData.RData", sep=""), verbose = TRUE)

#load("/home/harry/BSc_Thesis_Traj/R/R_wd/Exploring_PkgCreation/trajcoer_test/trajcoert01/tests/testthat/move_pkg_ExData.RData")
# Loading objects: leroy leroy_bursts_unique leroy_bursts_rep ricky martesPennantiMoveStack leroy_shift leroyMoveStack

data(vulture_moveStack)
#length(vulture_moveStack[[2]])
#length(vulture_moveStack@burstId)
#####



#===============================================================================
#
# PREPARATION
#
#===============================================================================

# TODO /delete
# Creation of further move objects for testing relating to NA values and infolocs

# # delete:
# bclOcelotMoveStack_4TrColl <- bclOcelotMoveStack
# bclOcelotMoveStack_4TrCollTrackIdchar <- as.character(bclOcelotMoveStack_4TrColl@trackId)
# bclOcelotMoveStack_4TrCollTrackIdchar[c(6:10)] <- "Moteado_2"
# bclOcelotMoveStack_4TrCollTrackIdchar[c(2260:2288)] <- "Estrella_2"
# bclOcelotMoveStack_4TrCollTrackIdFact <- as.factor(bclOcelotMoveStack_4TrCollTrackIdchar)
# bclOcelotMoveStack_4TrColl@trackId <- bclOcelotMoveStack_4TrCollTrackIdFact
# class(bclOcelotMoveStack_4TrColl) # MoveStack
# --> macht Problem: idData passt nicht zu trackIds !!!
# --> delete

exMoveStack <- move::moveStack(list(leroy,ricky))
#identical(exMoveStack, martesPennantiMoveStack)#T
#str(exMoveStack)


# # Create another MoveBurst
v_X1_Move <- vulture_moveStack[[1]]
#datesX1 <- as.Date(v_move_1@timestamps)
#datesX1_1 <- datesX1 == datesX1[1]
#day1 <- which(datesX1_1)
day1 <- which(as.Date(v_X1_Move@timestamps) == as.Date(v_X1_Move@timestamps[1]))
#v_X1_Move@data$ground_speed[w]
#sort(v_X1_Move@data$ground_speed[w])
#w_fly <- which(v_X1_Move@data$ground_speed[day1] > 5)
#behav <- vector(mode = "character", length = length(w))
behav <- rep("on_ground", length(day1))
#length(behav)
#behav[w_fly] <- "flying"
behav[which(v_X1_Move@data$ground_speed[day1] > 5)] <- "flying_high"

v_X1_1_Move <- v_X1_Move[day1]

v_X1_1_mb <- move::burst(v_X1_1_Move, f = behav[1:length(behav) - 1])
#length(v_X1_1_mb)
# !!
#v_X1_1_mb_Tracks <- as.Tracks(v_X1_1_mb)
#v_X1_1_mb_Tracks@tracksData

#-------------------------------------------------------------------------------



#===============================================================================
#
# CREATION OF A LIST OF "move" OBJECTS TO BE TESTED
#
#===============================================================================

movePkgObjectsList <- list(vulture_moveStack,
                           vulture_moveStack[[1]],
                           v_X1_1_mb,
                           leroy,
                           leroy_bursts_unique,
                           leroy_bursts_rep,
                           leroy_shift,
                           leroyMoveStack,
                           ##, ### ??
                           martesPennantiMoveStack,
                           ##, ### to be deleted (because of file size)
                           ricky,
                           ##, ### to be deleted, from load("...RData")
                           #bobbyMove,
                           #bclOcelotMoveStack,
                           ##, ### additionally created
                           exMoveStack# identical to martesPennantiMoveStack
                           ##, ### additionally created and to be deleted...
                           #bclOcelotMoveStack_4TrColl
                           )

movePkgObjNamesList <- list("vulture_moveStack",
                            "vulture_moveStack[[1]]",
                            "v_X1_1_mb",
                            "leroy",
                            "leroy_bursts_unique",
                            "leroy_bursts_rep",
                            "leroy_shift",
                            "leroyMoveStack",
                            ##, ### ??
                            "martesPennantiMoveStack",
                            ###, ### to be deleted (because of file size)
                            "ricky",
                            ###, ### to be deleted, from load("...RData")
                            #"bobbyMove",
                            #"bclOcelotMoveStack",
                            ##, ### additionally created
                            "exMoveStack"
                            ##, ### additionally created and to be deleted...
                            #"bclOcelotMoveStack_4TrColl"
                            )


stopifnot(length(movePkgObjNamesList) == length(movePkgObjectsList))


# !!!ACHTUNG: delete
#movePkgObjectsList <- list(ricky)
#movePkgObjNamesList <- list("ricky")



#===============================================================================
#===============================================================================
#
#
# TESTING ...
#
#
#===============================================================================
#===============================================================================



for (i in 1:length(movePkgObjectsList)) {
  
  #delete
  #ltr <- ltrajObjectsList[[1]]
  #ltrName <- ltrajObjNamesList[[1]]
  
  moveObj <- movePkgObjectsList[[i]]
  moveObjName <- movePkgObjNamesList[[i]]
  
  #delete
  # moveObj <- bclOcelotMoveStack
  # moveObjName <- "bclOcelotMoveStack"
  
  #===============================================================================
  #-------------------------------------------------------------------------------
  #
  # TESTING COERCION FROM MOVE TO TRACK
  #
  #-------------------------------------------------------------------------------
  #===============================================================================
  
  
  if (is(moveObj, "Move")) {
    
    #===============================================================================
    #
    context(paste("moveCoercion.R: TEST COERCION FROM MOVE (", moveObjName,
                  ") TO TRACK OBJECT:", sep = ""))
    #
    #===============================================================================
    
    
    test_that(paste("Test coercion of object ", moveObjName, " to Track object:", sep=""), {
      
      ##
      # Test as(moveObj,"Track") (Move object from example data to Track object)
      myTrack <- as(moveObj, "Track")
      
      # Test classes
      expect_that(moveObj, is_a("Move"))
      expect_that(myTrack, is_a("Track"))
      # Test length
      expect_that(length(moveObj), is_identical_to(length(myTrack)))
      # Test data
      expect_that(moveObj@data[,1], is_identical_to(myTrack@data[,1]))
      expect_that(moveObj@data, is_identical_to(myTrack@data))
      # Test time
      expect_that(moveObj@timestamps, is_equivalent_to(zoo::index(myTrack@time))) # ignores attributes
      #expect_that(moveObj@timestamps, is_identical_to(index(myTrack@time))) # Diff: attributes: < Length mismatch: comparison on first 1 components > 
      myTime <- zoo::index(myTrack@time)
      attr(myTime, "tclass") <- NULL
      expect_that(moveObj@timestamps, is_identical_to(myTime)) 
      
      ## delete
      #expect_that(moveObj@timestamps, is_identical_to(
      #  as.POSIXct(strptime(as.POSIXct(zoo::index(myTrack@time)), 
      #                      format = format(as.POSIXct(zoo::index(myTrack@time))))))) # Diff: attributes: < Length mismatch: comparison on first 1 components > 
      #moveObj@timestamps[1:5]
      #as.POSIXct(strptime(as.character(zoo::index(myTrack@time)), 
      #                    format = format(as.POSIXct(zoo::index(myTrack@time)))))[1:5]
      #as.POSIXct(strptime(as.character(zoo::index(myTrack@time)),
      #                               format = format(zoo::index(myTrack@time))[1]))
      #format = format(zoo::index(myTrack@time))[1]
      ### ???

      # Test geometry
      #expect_that(geometry(moveObj), is_equivalent_to(geometry(myTrack))) # ignores attributes
      #expect_that(geometry(moveObj), is_identical_to(geometry(myTrack))) # ignores attributes
      expect_that(geometry(moveObj), is_identical_to(geometry(myTrack@sp)))
    
    })

    
  } # Finish if (is(obj, Move))
  
  
  #===============================================================================
  #-------------------------------------------------------------------------------
  #
  # TESTING COERCION FROM MOVESTACK TO TRACKS AND TRACKSCOLLECTION
  #
  #-------------------------------------------------------------------------------
  #===============================================================================
  
  
  if (is(moveObj, "MoveStack")) {
    
    #===============================================================================
    #
    context(paste("moveCoercion.R: TEST COERCION FROM MOVESTACK (", moveObjName,
                  ") TO TRACKS OBJECT:", sep = ""))
    #
    #===============================================================================
    
    
    test_that(paste("Test coercion of object ", moveObjName, " to Tracks object:", sep=""), {
      
      # ??? !!!
      # delete
      #moveObj <- martesPennantiMoveStack
      #moveObj <- vulture_moveStack
      #Tr_alternativ <- moveStackToTracks(moveObj)
      
      # Test as(moveObj,"Tracks") (MoveStack object to Tracks object)
      myTracks <- as(moveObj, "Tracks")
      
#       # ??? !!!
#       #delete
#       expect_that(Tr_alternativ, is_equivalent_to(myTracks)) # T
#       #attr(moveObj@coords, "dimnames")[[2]] # [1] "location.long" "location.lat" 
#       #attr(myTracks@tracks[[1]]@sp@coords, "dimnames")[[2]] # [1] "location.long" "location.lat" 
#       #attr(Tr_alternativ@tracks[[1]]@sp@coords, "dimnames")[[2]] # [1] "coords.x1" "coords.x2"
#       # row.names in @data identisch!?
#       #attr(moveObj@idData, "row.names") # 
#       #attr(myTracks@tracksData, "row.names") # 
#       #attr(Tr_alternativ@tracksData, "row.names") # 
#       # ...
#       #expect_that(Tr_alternativ, equals(myTracks)) # 
#       #expect_that(Tr_alternativ, is_identical_to(myTracks))
#       # TODO Problems in names, dimnames, ? factor?, rownames?, 
#       
#       
#       # Test names (TrackNames)
#       #expect_that(names(Tr_alternativ@tracks), is_identical_to(names(myTracks@tracks)))
#       # TrackNames: Leroy vs. Track1_Leroy --> was ist besser???
#       # But in general names are ok!
#       
#       # Test row.names
#       expect_that(row.names(Tr_alternativ@tracks[[1]]@data), 
#                   is_identical_to(row.names(myTracks@tracks[[1]]@data)))
#       #expect_that(row.names(Tr_alternativ@tracksData), 
#       #            is_identical_to(row.names(myTracks@tracksData)))
#       # --> not identical, wegen konsequenzen aus names (s.o.)
#       # But in general names are ok!
#       
#       #!!! --> (bisher) keine (gravierenden) Unterschiede in Tests
#       # --> !?!?! Tests nicht gut genug? What about factor data, dimnames, ...?
#       myTracks <- Tr_alternativ
      
      # Test classes
      expect_that(moveObj, is_a("MoveStack"))
      expect_that(myTracks, is_a("Tracks"))
      
      # Test length
      trackIds <- unique(moveObj@trackId)
      nrTrId1 <- 
        length(moveObj@trackId[moveObj@trackId == trackIds[1]])
      nrTrId2 <- 
        length(moveObj@trackId[moveObj@trackId == trackIds[2]])
      expect_that(length(moveObj),
                  is_identical_to(sum(myTracks@tracksData$n)))
      expect_that(nrTrId1, is_identical_to(myTracks@tracksData$n[1]))
      expect_that(nrTrId1, is_identical_to(length(myTracks@tracks[[1]])))
      expect_that(nrTrId2, is_identical_to(myTracks@tracksData$n[2]))
      expect_that(nrTrId2, is_identical_to(length(myTracks@tracks[[2]])))
      expect_that(length(unique(moveObj@trackId)),
                  is_identical_to(length(myTracks@tracks)))
      
      # Test data
      expect_that(moveObj@data[1,1],
                  is_identical_to(myTracks@tracks[[1]]@data[1,1]))
      expect_that(moveObj@data[1:2, ],
                  is_identical_to(myTracks@tracks[[1]]@data[1:2, ]))
      # Further data testing needs helping objects:
      # delete: warum, wo kommen diese attribute her??? --> Tracks()
      dataList <- lapply(myTracks@tracks, function(x) x@data)
      dataTrcs <- do.call(rbind, dataList)
      expect_that(moveObj@data, is_equivalent_to(dataTrcs))
      #expect_that(moveObj@data, is_identical_to(dataTrcs)) # Diff: rownames
      
      ##
      # Test time (time reordered in case of as(..., "xts")!)
      expect_that(as.numeric(moveObj@timestamps),
                  is_equivalent_to(unlist(lapply(myTracks@tracks,
                                                 function(x) zoo::index(x@time))))) # ignores attributes
      #expect_that(as.numeric(moveObj@timestamps), 
      #    is_identical_to(unlist(sapply(myTracks@tracks, function(x) index(x@time))))) 
      # -->  Diff: names for target but not for current
      
      expect_that(moveObj@timestamps[1],
                  is_equivalent_to(zoo::index(myTracks@tracks[[1]]@time)[1])) # ignores attributes
      
      expect_that(moveObj@timestamps[1],
                  is_equivalent_to(zoo::index(as(myTracks@tracks[[1]], "xts")[1]))) # ignores attributes
      #expect_that(moveObj@timestamps[1], is_identical_to(index(as(myTracks, "xts")[1]))) 
      # --> Diff: Attributes: < Length mismatch: comparison on first 1 components >
      
      # not always equivalent if different Track objects are tracked synchron at similar / overlapping periods
      tracks_len_time <- length(unlist(lapply(myTracks@tracks, function(x) zoo::index(x@time)))) # 1838
      moveStack_len_time <- length(moveObj[[1]]@timestamps)
      #expect_that(moveObj@timestamps[moveStack_len_time],
      #            is_equivalent_to(zoo::index(as(myTracks, "xts")[tracks_len_time]))) # ignores attributes
      
      #expect_that(moveObj@timestamps[moveStack_len_time],
      #            is_identical_to(zoo::index(as(myTracks, "xts")[tracks_len_time]))) 
      
      myTime <- zoo::index(as(myTracks, "xts")[tracks_len_time])
      attr(myTime, "tclass") <- NULL
      #expect_that(moveObj@timestamps[moveStack_len_time], equals(myTime))
      myTime <- zoo::index(as(myTracks[1], "xts"))
      attr(myTime, "tclass") <- NULL
      expect_that(moveObj[[1]]@timestamps, equals(myTime))
      #expect_that(moveObj@timestamps[moveStack_len_time], is_identical_to(myTime)) # nicht immer !?!
      # delete: problem bei: leroyMoveStack, martesPennantiMoveStack
      
      myl <- lapply(myTracks@tracks, function(x) zoo::index(x@time))
      v <- myl[[1]]
      if (length(myl) > 1){
        for (i in 2:length(myl)) { v <- c(v, myl[[i]]) }
      }
      expect_that(moveObj@timestamps, is_equivalent_to(v))
      expect_equal(moveObj@timestamps, v)

      ##
      # Test geometry
      expect_that(geometry(moveObj),
                  is_equivalent_to(geometry(as(myTracks, "Spatial")))) # ignores attributes
      #expect_that(geometry(moveObj), is_identical_to(geometry(as(myTracks, "Spatial")))) 
      # --> Attributes dimnames
      
      #Test tracksData
      moveColNames <- colnames(moveObj@idData)
      #myTrColNames <- colnames(myTracks@tracksData)
      expect_that(moveObj@idData, is_equivalent_to(myTracks@tracksData[ , moveColNames]))
      #expect_that(moveObj@idData, is_identical_to(myTracks@tracksData[ , moveColNames]))
      # --> row.names adjusted related to Track names
      #delete:
      #attr(moveObj@idData, "row.names")
      #attr(myTracks@tracksData, "row.names")
      
      
    })


    
    #===============================================================================
    #
    context(paste("moveCoercion.R: TEST COERCION FROM MOVESTACK (", moveObjName,
                  ") TO TRACKSCOLLECTION OBJECT:", sep = ""))
    #
    #===============================================================================
    
    
    test_that(paste("Test coercion of object ", moveObjName, " to TracksCollection object:", sep=""), {
      
      # !!!
      # Changed order of current und target : myTrColl ..is_ident... to moveObj
      # !!!
      
      #moveObj <- bclOcelotMoveStack
      #moveObj <- martesPennantiMoveStack
      #moveObj <- vulture_moveStack
      #TrColl_alternativ <- moveStackToTracksColl(moveObj)
      
      # Test as(moveObj,"TracksCollection") (MoveStack object to TracksCollection object)
      myTrColl <- as(moveObj, "TracksCollection")
      
#       expect_that(TrColl_alternativ, is_equivalent_to(myTrColl)) # T
#       #expect_that(TrColl_alternativ, is_identical_to(myTrColl)) # F
#       #Attributes: < Component “tracksCollection”: Names: 2 string mismatches
#       # ...bbox und coords dimnames !?!? und weiteres (Ausgabe erscheint abgeschniten?!?)?
#       
#       # Test names (TrackNames, TracksNames)
#       # Just equivalent because (still) different TracksNames
#       expect_that(sapply(TrColl_alternativ@tracksCollection, function(x) names(x@tracks)), 
#                   is_equivalent_to(sapply(myTrColl@tracksCollection, function(x) names(x@tracks))))
#       #expect_that(names(TrColl_alternativ@tracksCollection), 
#       #            is_equivalent_to(names(myTrColl@tracksCollection)))
#       # But in general names are ok!
#       
#       # Test row.names
#       expect_that(row.names(TrColl_alternativ@tracksCollection[[1]]@tracks[[1]]@data), 
#                   is_identical_to(row.names(myTrColl@tracksCollection[[1]]@tracks[[1]]@data)))
#       #expect_that(row.names(TrColl_alternativ@tracksCollectionData), 
#       #            is_identical_to(row.names(myTrColl@tracksCollectionData))) # wie oben bei names!!!
#       expect_that(row.names(TrColl_alternativ@tracksCollection[[1]]@tracksData), 
#                   is_identical_to(row.names(myTrColl@tracksCollection[[1]]@tracksData)))
#       # # But in general names are ok!
#       
#       ## Achtung TODO or delete: in new approach with move::split not identical /
#       ## equivalent any more...!!! ??? !!!
#       ## Factors in TracksData: u.U. mehr Levles/Factors als Ausprägungen
#       ##str(TrColl_alternativ@tracksCollection[[1]]@tracksData) #--> ok!
#       #for (i in seq_along(TrColl_alternativ@tracksCollection)) {
#       #  for (j in seq_along(TrColl_alternativ@tracksCollection[[i]]@tracksData))
#       #    if (is.factor(TrColl_alternativ@tracksCollection[[i]]@tracksData[[j]])) {
#       #      expect_that(length(levels(TrColl_alternativ@tracksCollection[[i]]@tracksData[[j]])),
#       #                  is_identical_to(
#       #                    length(unique(TrColl_alternativ@tracksCollection[[i]]@tracksData[[j]]))))
#       #    }
#       #}
# 
#       
#       #!!! --> (bisher) keine (gravierenden) Unterschiede in Tests
#       # --> !?!?! Tests nicht gut genug? What about factor data, dimnames, ...?
#       myTrColl <- TrColl_alternativ
      
      
      # Test classes
      expect_that(moveObj, is_a("MoveStack"))
      expect_that(myTrColl, is_a("TracksCollection"))
      # Test length
      trackIds <- unique(moveObj@trackId)
      nrTrId1 <- 
        length(moveObj@trackId[moveObj@trackId == trackIds[1]])
      nrTrId2 <- 
        length(moveObj@trackId[moveObj@trackId == trackIds[2]])
      expect_that(sum(sapply(myTrColl@tracksCollection,
                             function(x) x@tracksData$n)),
                  is_identical_to(length(moveObj)))
      #delete
      #expect_that(length(moveObj),
      #            is_identical_to(sum(sapply(myTrColl@tracksCollection,
      #                                       function(x) x@tracksData$n))))
      
      expect_that(myTrColl@tracksCollection[[1]]@tracksData$n[1],
                  is_identical_to(nrTrId1))
      expect_that(length(myTrColl@tracksCollection[[1]]@tracks[[1]]),
                  is_identical_to(nrTrId1))
      expect_that(myTrColl@tracksCollection[[2]]@tracksData$n,
                  is_identical_to(nrTrId2))
      expect_that(length(myTrColl@tracksCollection[[2]]@tracks[[1]]),
                  is_identical_to(nrTrId2))
      expect_that(length(myTrColl@tracksCollection),
                  is_identical_to(length(unique(moveObj@trackId))))
      expect_that(nrow(myTrColl@tracksCollectionData),
                  is_identical_to(length(unique(moveObj@trackId))))
      
      # !!! delete: Order of cur , target changed
      #expect_that(nrTrId1,
      ##            is_identical_to(
      #              myTrColl@tracksCollection[[1]]@tracksData$n[1]))
      #expect_that(nrTrId1,
      #            is_identical_to(
      #              length(myTrColl@tracksCollection[[1]]@tracks[[1]])))
      #expect_that(nrTrId2,
      #            is_identical_to(
      #              myTrColl@tracksCollection[[2]]@tracksData$n))
      #expect_that(nrTrId2,
      #            is_identical_to(
      #              length(myTrColl@tracksCollection[[2]]@tracks[[1]])))
      #expect_that(length(unique(moveObj@trackId)),
      #            is_identical_to(length(myTrColl@tracksCollection)))
      #expect_that(length(unique(moveObj@trackId)),
      #            is_identical_to(nrow(myTrColl@tracksCollectionData)))
      
      # ...
      
      # Test data
      expect_that(myTrColl@tracksCollection[[1]]@tracks[[1]]@data[1,1],
                  is_identical_to(moveObj@data[1,1]))
      #expect_that(moveObj@data[1,1],
      #            is_identical_to(
      #              myTrColl@tracksCollection[[1]]@tracks[[1]]@data[1,1]))
      
      # Further data testing needs helping objects:
      origDataColnames <- colnames(moveObj@data)
      myTrColl_df <- as(myTrColl, "data.frame")
      myTrColl_df_clean <- 
        myTrColl_df[!is.na(myTrColl_df[origDataColnames[1]]),
                                origDataColnames]  
      #expect_that(myTrColl_df_clean,
      #            is_identical_to(moveObj@data))
      # --> not due to row.names
      expect_that(myTrColl_df_clean,
                  is_equivalent_to(moveObj@data))
      #expect_that(moveObj@data,
      #            is_equivalent_to(myTrColl_df_clean))
      
      
      #expect_that(moveObj@data, is_identical_to(myTrColl_df_clean)) # Diff: rownames
      #attr(moveObj@data, "row.names") # numeric 1:2288
      #attr(myTrColl_df_clean, "row.names")# "Tracks_Estrella.Track1_Estrella.361"
      # werden bei TracksColl() gesetzt!
      ##
      
      # Test time (time reordered in case of as(..., "xts")!)
      expect_that(
        unlist(lapply(myTrColl@tracksCollection, function(x) {
          zoo::index(x@tracks[[1]]@time) })),
                  is_equivalent_to(as.numeric(moveObj@timestamps))) 
      #expect_that(as.numeric(moveObj@timestamps),
      #            is_equivalent_to(
      #              unlist(lapply(myTrColl@tracksCollection, function(x) {
      #                zoo::index(x@tracks[[1]]@time) })))) # ignores attributes
      
      #expect_that(as.numeric(moveObj@timestamps), is_identical_to(unlist(sapply(myTrColl@tracksCollection, function(x) zoo::index(x@tracks[[1]]@time))))) # Diff: names for target but not for current
      #names(zoo::index(myTrColl@tracksCollection[[1]]@tracks[[1]]@time)) # --> NULL ??
      #names(moveObj@timestamps)#, "row.names")# --> NULL ??
      #expect_that(zoo::index(as(myTrColl, "xts")[1]),
      #            is_equivalent_to(moveObj@timestamps[1])) # ignores attributes
      expect_that(zoo::index(as(myTrColl[1], "xts")),
                  is_equivalent_to(moveObj[[1]]@timestamps)) # ignores attributes
      
      #expect_that(moveObj@timestamps[1],
      #            is_equivalent_to(zoo::index(as(myTrColl, "xts")[1]))) # ignores attributes
      
      #expect_that(moveObj@timestamps[1], is_identical_to(zoo::index(as(myTrColl, "xts")[1]))) # Diff: Attributes: < Length mismatch: comparison on first 1 components >
      
      #trColl_len_time <- length(unlist(lapply(
      #  myTrColl@tracksCollection, function(x) { 
      #    zoo::index(x@tracks[[1]]@time) }))) # 1838                                        
      #moveStack_len_time <- length(moveObj@timestamps)
      #expect_that(zoo::index(as(myTrColl, "xts")[trColl_len_time]),
      #            is_equivalent_to(moveObj@timestamps[moveStack_len_time])) # ignores attributes
      
      #expect_that(moveObj@timestamps[moveStack_len_time],
      #            is_equivalent_to(zoo::index(as(myTrColl, "xts")[trColl_len_time]))) # ignores attributes
      
      #myl <- sapply(myTrColl@tracksCollection,
      #              function(x) index(x@tracks[[1]]@time))
      #v <- myl[[1]]
      #if (length(myl) > 1) {
      #  for (i in 2:length(myl)) { v <- c(v, myl[[i]]) }
      #}
      myl <- lapply(myTrColl@tracksCollection,
                    function(x) zoo::index(x@tracks[[1]]@time))
      v <- myl[[1]]
      if (length(myl) > 1) {
        for (i in 2:length(myl)) { v <- c(v, myl[[i]]) }
      }
      expect_that(v, is_equivalent_to(moveObj@timestamps))
      expect_equal(v, moveObj@timestamps)
      #expect_that(moveObj@timestamps, is_equivalent_to(v))
      #expect_equal(moveObj@timestamps, v)
      ##
      
      # Test geometry
      expect_that(geometry(as(myTrColl, "Spatial")),
                  is_equivalent_to(geometry(moveObj))) # ignores attributes
      #expect_that(geometry(moveObj),
      #            is_equivalent_to(geometry(as(myTrColl, "Spatial")))) # ignores attributes
      #expect_equal(geometry(moveObj), geometry(as(myTrColl, "Spatial"))) # ...
      #expect_that(geometry(moveObj), is_identical_to(as(myTrColl, "Spatial"))) # ...
      
      #Test tracksData
      moveColNames <- colnames(moveObj@idData)
      #myTrColNames <- colnames(myTracks@tracksData)
      expect_that(myTrColl@tracksCollectionData[ , moveColNames], is_equivalent_to(moveObj@idData))
      #expect_that(myTrColl@tracksCollectionData[ , moveColNames], is_identical_to(moveObj@idData))
      # --> row.names adjusted related to Track names
      #expect_that(moveObj@idData, is_equivalent_to(myTracks@tracksData[ , moveColNames]))
      #expect_that(moveObj@idData, is_identical_to(myTracks@tracksData[ , moveColNames]))
      
    })
    
  } # Finish if (is(obj, MoveStack))
  
  
  
  #===============================================================================
  #-------------------------------------------------------------------------------
  #
  # TESTING COERCION FROM MOVEBURST TO TRACK AND TRACKS
  #
  #-------------------------------------------------------------------------------
  #===============================================================================
  
  
  if (is(moveObj, "MoveBurst")) {
    
    #===============================================================================
    #
    context(paste("moveCoercion.R: TEST COERCION FROM MOVEBURST (", moveObjName,
                  ") TO TRACK OBJECT:", sep = ""))
    #
    #===============================================================================
    
        
    test_that(paste("Test coercion of object ", moveObjName, " to Track object:", sep=""), {
      
      ##
      # Test as(MoveBurst, "Track") (MoveBurst object to Track object)
      myTrackFromBursts <- as(moveObj, "Track")
      
      # Test classes
      expect_that(moveObj, is_a("MoveBurst"))
      expect_that(myTrackFromBursts, is_a("Track"))
      # Test length
      expect_that(length(moveObj), is_identical_to(length(myTrackFromBursts)))
      expect_that(length(moveObj@burstId),
                  is_identical_to(nrow(myTrackFromBursts@connections)))
      # Test data
      expect_that(moveObj@data[,1], is_identical_to(myTrackFromBursts@data[,1]))
      expect_that(moveObj@data, is_identical_to(myTrackFromBursts@data))
      # Test time
      expect_that(moveObj@timestamps,
                  is_equivalent_to(zoo::index(myTrackFromBursts@time))) # ignores attributes
      #expect_that(moveObj@timestamps, is_identical_to(zoo::index(myTrackFromBursts@time))) # Diff: attributes: < Length mismatch: comparison on first 1 components > 
      ##
      # Test geometry
      expect_that(geometry(moveObj),
                  is_equivalent_to(geometry(myTrackFromBursts))) # ignores attributes
      expect_that(geometry(moveObj), is_identical_to(geometry(myTrackFromBursts@sp))) # Diff: Attributes
      expect_equal(geometry(moveObj), geometry(myTrackFromBursts@sp)) 
      expect_that(geometry(moveObj),
                  is_equivalent_to(geometry(myTrackFromBursts@sp)))
      
    })
    
    
    #===============================================================================
    #
    context(paste("moveCoercion.R: TEST COERCION FROM MOVEBURST (", moveObjName,
                  ") TO TRACKS OBJECT:", sep = ""))
    #
    #===============================================================================
    

    test_that(paste("Test coercion of object ", moveObjName, " to Tracks object:", sep=""), {
    
      #Alternativer Ansatz
      #moveObj <- leroy_bursts_rep
      #moveObjName <- "leroy_bursts_rep"
      #moveObj <- v_X1_1_mb
      #moveObjName <- "v_X1_1_mb"
      
      nrOfBursts <- switch(moveObjName, leroy_bursts_rep = 5, leroy_bursts_unique = 4,
                           v_X1_1_mb = 3)
      seqOfBursts <- switch(moveObjName, 
                            leroy_bursts_rep = factor(c("A", "B", "C", "A", "C")), 
                            leroy_bursts_unique = factor(c("part_1", "part_2", "part_3", "part_4")),
                            v_X1_1_mb = factor(c("on_ground", "flying_high", "on_ground")))
      
      nrOfBurstTypes <- length(unique(move::burst(moveObj)))
      
      ######################
      # Test as(MoveBurst, "Tracks") (MoveBurst object from example data to Tracks object)
      # Burst IDs might not be unique. That means that a burst type may appear several times.
      
      #str(leroy_bursts_rep)
      #moveObjName <- "leroy_bursts_unique"
      #moveObj <- leroy_bursts_unique
      #length(moveObj) # 919
      #length(moveObj@burstId) # 918
      #uniqueBurstId <- unique(moveObj@burstId) # length = 4
      
      #sum(myTracksOfBursts@tracksData$n) # 922
      
      #nrOfBurstTypes <- 3
      #nrOfBursts <- 5
      #######################
      
      # ALternative (alter / erster Ansatz): 
      # Approaches differ just in implementation and not in results
      
      #TrOfMoveBurst_alternativ <- moveBurstToTracks(moveObj)
      
      #TrOfMoveBurst_alternativ@tracksData
      #row.names(TrOfMoveBurst_alternativ@tracksData)
      
      # Test as(moveObj,"Tracks") (MoveBurst object to Tracks object)
      myTracksOfBursts <- as(moveObj,"Tracks")
      
#       expect_that(TrOfMoveBurst_alternativ, is_equivalent_to(myTracksOfBursts))
#       #expect_that(TrOfMoveBurst_alternativ, equals(myTracksOfBursts))
#       #expect_that(TrOfMoveBurst_alternativ, is_identical_to(myTracksOfBursts))
#       # --> Fehler in as.POSIXct.numeric(current) : 'origin' must be supplied
#       # --> !!? wieso nur hier und nicht bei den anderen coercions???
#       
#       # Test names
#       # Achtung uNterschied: 1.Impl: names verändert --> e.g. "Burst1_part_1" 
#       # ... 2. impl. (zuerst: names = burstIds) aber burstIds könne mehrfach auftreten,
#       # daher muss ich die anpassen..!
#       expect_that(names(TrOfMoveBurst_alternativ@tracks), is_identical_to(names(myTracksOfBursts@tracks)))
#       expect_that(TrOfMoveBurst_alternativ@tracksData$burstId, is_identical_to(seqOfBursts))
#       
#       # Test row.names
#       expect_that(row.names(TrOfMoveBurst_alternativ@tracks[[1]]@data), 
#                   is_identical_to(row.names(myTracksOfBursts@tracks[[1]]@data)))
#       expect_that(row.names(TrOfMoveBurst_alternativ@tracksData), 
#                   is_identical_to(row.names(myTracksOfBursts@tracksData))) # wie oben bei names!!!
#       expect_that(TrOfMoveBurst_alternativ@tracksData$burstId, is_identical_to(seqOfBursts))
      
      
      ############
      # !!!!!!!!!!!!
      # This decides which approach is further tested..!!!
      # !!! ???
      
      #myTracksOfBursts <- TrOfMoveBurst_alternativ
      ###########
      
      # Test classes
      expect_that(moveObj, is_a("MoveBurst"))
      expect_that(myTracksOfBursts, is_a("Tracks"))
      # Test length
      expect_that(length(moveObj),
                  equals(sum(myTracksOfBursts@tracksData$n)
                         - nrOfBursts + 1))
      expect_that(length(moveObj@burstId),
                  is_identical_to(sum(sapply(myTracksOfBursts@tracks, function(x) {
                    nrow(x@connections)
                  }))))
      expect_that(nrOfBurstTypes,
                  equals(
                    length(unique(myTracksOfBursts@tracksData$burst))))
      expect_that(nrOfBursts,
                  equals(length(myTracksOfBursts@tracksData$burst)))
      expect_that(nrOfBursts,
                  equals(length(myTracksOfBursts@tracks)))
      # Test data
      expect_that(moveObj@data[1,1],
                  is_identical_to(myTracksOfBursts@tracks[[1]]@data[1,1]))
      expect_that(moveObj@data[nrow(moveObj@data),1],
                  is_identical_to(
                    myTracksOfBursts@tracks[[nrOfBursts]]@data
                    [nrow(myTracksOfBursts@tracks[nrOfBursts][[1]]@data),1]))
      origDataLength <- length(moveObj@data)
      expect_that(moveObj@data[nrow(moveObj@data),origDataLength],
                  is_identical_to(
                    myTracksOfBursts@tracks[[nrOfBursts]]@data
                    [nrow(myTracksOfBursts@tracks[nrOfBursts][[1]]@data),
                     origDataLength]))
      expect_that(moveObj@data[1,],
                  is_identical_to(myTracksOfBursts@tracks[[1]]@data[1,]))
      
      # Test time
      expect_that(moveObj@timestamps[1],
                  is_equivalent_to(zoo::index(myTracksOfBursts@tracks[[1]]@time[1]))) # ignores attributes
      expect_that(as.numeric(moveObj@timestamps),
                  is_equivalent_to(
                    unique(unlist(sapply(myTracksOfBursts@tracks, function(x) {
                      zoo::index(x@time)
                    }))))) # ignores attributes                                           
      myl <- sapply(myTracksOfBursts@tracks, function(x) zoo::index(x@time))
      v <- myl[[1]]
      if (length(myl) > 1) {
        for (i in 2:length(myl)) { v <- c(v, myl[[i]]) }
      }
      v_unique <- unique(v)
      expect_that(moveObj@timestamps, is_equivalent_to(v_unique))
      
      # Test geometry
      expect_that(class(geometry(moveObj)),
                  is_identical_to(
                    class(geometry(as(myTracksOfBursts, "Spatial"))))) 
      expect_that(bbox(moveObj),
                  is_equivalent_to(bbox(as(myTracksOfBursts, "Spatial")))) # ignores attributes
      tr_x_coords <- unlist(sapply(
        myTracksOfBursts@tracks, function(x) x@sp@coords[ ,1]))
      tr_y_coords <- unlist(sapply(
        myTracksOfBursts@tracks, function(x) x@sp@coords[ ,2]))
      names(tr_x_coords) <- names(tr_y_coords) <- NULL
      expect_that(unique(moveObj@coords[ ,1]),
                  is_identical_to(
                    unique(unlist(sapply(myTracksOfBursts@tracks, function(x) {
                      x@sp@coords[ ,1]
                    })))))
      expect_that(unique(moveObj@coords[ ,2]),
                  is_identical_to(
                    unique(unlist(sapply(myTracksOfBursts@tracks, function(x) {
                      x@sp@coords[ ,2]
                    })))))
      
      # Test burst names in tracksData
      expect_that(myTracksOfBursts@tracksData[ , "burstId"], is_identical_to(seqOfBursts))
      
    })
        
  } # Finish if (is(moveObj, "MoveBurst"))
  
} # Finish for loop

print(Sys.time())