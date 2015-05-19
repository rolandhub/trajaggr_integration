#test_file(paste(getwd(),"/tests/testthat/test_trajToMoveCoercion.R", sep=""))
#===============================================================================
#
# JUST RUN THIS TEST FILE
#
#===============================================================================

require(sp)
#require(testthat)
#test_file(paste(getwd(),"/tests/testthat/test_trajToMoveCoercion.R", sep=""))

#-------------------------------------------------------------------------------



#!!!!!!!!!!!!!!!!!!!
# TODO
# - !! Hier 2 Ansätze zu coercion of Tracks-->MoveBurst --> auswählen oder weiter anpassen!?!?
# - provide Data in pkg dir /data
# - extend examplesTrack data, also for testing aggregation
# - ...

# Important
# - order of tests ok !!!
# - 
#!!!!!!!!!!!!!!!!!!!!

print(Sys.time())

#####
#delete
#save(
#     file="....RData")
#load("....RData")
#load(paste(getwd(), "/tests/testthat/", "...RData", sep=""))

load("trajectories_exampleTrack_Data_original.RData", verbose = TRUE)
#load("/home/harry/BSc_Thesis_Traj/R/R_wd/Exploring_PkgCreation/trajcoer_test/trajcoert01/tests/testthat/trajectories_exampleTrack_Data_original.RData", verbose = TRUE)
# A, A1, A2, B, B1, B2, Tr

#load("enviroCarTestData.RData", verbose = TRUE)
#load("/home/harry/BSc_Thesis_Traj/R/R_wd/Exploring_PkgCreation/trajcoer_test/trajcoert01/tests/testthat/enviroCarTestData.RData", verbose = TRUE)
# enviroCarTracksObj, enviroCarTracksCollObj

load("enviroCar_TestData.RData", verbose = TRUE)
#load("/home/harry/BSc_Thesis_Traj/R/R_wd/Exploring_PkgCreation/trajcoer_test/trajcoert01/tests/testthat/enviroCar_TestData.RData", verbose = TRUE)
# enviroCar_TracksObj1 enviroCar_TracksObj2 enviroCar_TracksColl
#####


#===============================================================================
#
# PREPARATION
#
#===============================================================================

# TODO /delete
# Creation of further "trajectories objects"

# Track object from enviroCar
trackList <- lapply(enviroCar_TracksObj1@tracks, function(x) x)
enviroCar_TrackObj1_1 <- trackList[[1]]
#class(enviroCar_TrackObj1_1)

# trajectories objects with different (number of) columns
#trackList <- lapply(enviroCar_TracksObj1@tracks, function(x) x)
trackList[[1]]@data$Speed <- NULL
#length(trackList[[1]]@data) # 26
#length(trackList[[2]]@data) # 27
# Tracks object
enviroCar_TracksObj1_AttrDiff <- trajectories::Tracks(trackList)

trackList[[1]]@data$GPS.Accuracy <- NULL
trackList[[2]]@data$Speed <- NULL
##trackList[[2]]@data$GPS.Accuracy <- NULL
#length(trackList[[2]]@data) # 26
trcs <- trajectories::Tracks(trackList)
#length(trcs@tracks[[1]]@data) # 25
#length(trcs@tracks[[2]]@data) # 26

#length(enviroCar_TracksObj2@tracks[[1]]@data) # 27
#length(enviroCar_TracksObj2@tracks[[2]]@data) # 27
# TracksCollection object
enviroCar_TracksColl_AttrDiff <- trajectories::TracksCollection(list(trcs, enviroCar_TracksObj2))


# Tracks and TracksCollection with just one Track resp. Tracks object
A1_as_Tracks <- trajectories::Tracks(list(A1))
A_as_TrColl <- trajectories::TracksCollection(list(A))


# ...

#-------------------------------------------------------------------------------


#===============================================================================
#
# CREATION OF A LIST OF "trajectories" OBJECTS TO BE TESTED
#
#===============================================================================

trajObjectsList <- list(A, 
                        A1, 
                        A2, 
                        B, 
                        B1, 
                        B2, 
                        Tr,
                        ## may be to be deleted
                        enviroCar_TracksObj1,
                        enviroCar_TracksObj2,
                        enviroCar_TracksColl,
                        ## additionally created
                        A1_as_Tracks,
                        A_as_TrColl,
                        enviroCar_TrackObj1_1,
                        enviroCar_TracksObj1_AttrDiff, # Differences in Attributes of Track objects
                        enviroCar_TracksColl_AttrDiff # Differences in Attributes of Tracks objects
                        ## may be to be deleted, bad data (zumindest TracksObj)
                        #enviroCarTracksObj, 
                        #enviroCarTracksCollObj
                        )

trajObjNamesList <- list("A", 
                         "A1", 
                         "A2", 
                         "B", 
                         "B1", 
                         "B2", 
                         "Tr",
                         ## may be to be deleted
                         "enviroCar_TracksObj1",
                         "enviroCar_TracksObj2",
                         "enviroCar_TracksColl",
                         ## additionally created
                         "A1_as_Tracks",
                         "A_as_TrColl",
                         "enviroCar_TrackObj1_1",
                         "enviroCar_TracksObj1_AttrDiff", # Differences in Attributes
                         "enviroCar_TracksColl_AttrDiff" # Differences in Attributes
                         ## may be to be deleted
                         #"enviroCarTracksObj", 
                         #"enviroCarTracksCollObj"
                         )


stopifnot(length(trajObjNamesList) == length(trajObjectsList))



#===============================================================================
#===============================================================================
#
#
# TESTING ...
#
#
#===============================================================================
#===============================================================================


for (i in 1:length(trajObjectsList)) {
  
  trajObj <- trajObjectsList[[i]]
  trajObjName <- trajObjNamesList[[i]]
  
  #delete
  # trajObj <- enviroCar_TracksObj1 # enviroCar_TracksObj1_AttrDiff
  # trajObjName <- "enviroCarTracksObj"

  #===============================================================================
  #-------------------------------------------------------------------------------
  #
  # TESTING COERCION FROM TRACK TO MOVE
  #
  #-------------------------------------------------------------------------------
  #===============================================================================
  
  
  if (is(trajObj, "Track")) {
    
    #===============================================================================
    #
    context(paste("toMoveCoercion.R: TEST COERCION FROM TRACK (", trajObjName,
                  ") TO MOVE.", sep = ""))
    #
    #===============================================================================
    
    
    test_that(paste("Test object ", trajObjName, ":", sep=""), {
      
      #trajObj <- enviroCar_TrackObj1_1
      #trajObj <- A
      
      myMove <- as(trajObj, "Move")
      
      # Test classes
      expect_that(trajObj, is_a("Track"))
      #expect_that(myMove, is_a("Move")) # already tested
      
      # Test length
      expect_that(length(myMove), is_identical_to(length(trajObj)))      
      
      # Test data
      expect_that(myMove@data[,1], is_identical_to(trajObj@data[,1]))
      #expect_that(myMove@data, is_identical_to(trajObj@data))
      #str(myMove@data) # --> additional attribute "sensor" due to move::move implementation
      trajDataColnames <- names(trajObj@data)
      myMoveDataColnames <- names(myMove@data)
      commonColnames <- intersect(trajDataColnames, myMoveDataColnames)
      expect_that(myMove@data[commonColnames],
                  is_identical_to(trajObj@data[commonColnames]))
      
      # Test time
      expect_that(myMove@timestamps, is_equivalent_to(zoo::index(trajObj@time))) # ignores attributes
      expect_that(myMove@timestamps, is_identical_to(zoo::index(trajObj@time))) 
            
      #delete:
      myTime <- zoo::index(trajObj@time)
      attr(myTime, "tclass") <- NULL
      expect_that(myMove@timestamps, is_equivalent_to(myTime)) # ignores attributes
      #expect_that(myMove@timestamps, is_identical_to(myTime)) # ignores attributes
      ##
      
      # Test geometry
      expect_that(sp::geometry(myMove), is_identical_to(sp::geometry(trajObj@sp)))
      
      # Test myMove@idData and completeness of myMove@data
      expect_that(!any(names(myMove@idData) %in% names(trajObj@data)), is_identical_to(TRUE))
      expect_that(all(names(trajObj@data) %in% names(myMove@data)), is_identical_to(TRUE))
      
      #delete
      #!any(names(m@idData) %in% names(data))
      #all(namesD %in% names(m@data))
      
    })
  } # Finish if is
  
  
  
  if (is(trajObj, "Tracks")) {
    
    
    #===============================================================================
    #-------------------------------------------------------------------------------
    #
    # TESTING COERCION FROM TRACKS TO MOVESTACK
    #
    #-------------------------------------------------------------------------------
    #===============================================================================
    
    
    #===============================================================================
    #
    context(paste("toMoveCoercion.R: TEST COERCION FROM TRACKS (", trajObjName,
                  ") TO MOVESTACK.", sep = ""))
    #
    #===============================================================================
    
    
    test_that(paste("Test object ", trajObjName, ":", sep=""), {
      
      #trajObj <- enviroCarTracksObj
      #trajObj <- enviroCar_TracksObj1
      #str(trajObj)
      
      myMoveSt <- as(trajObj, "MoveStack")
      
      #str(myMoveSt)
      
      # Test classes
      expect_that(trajObj, is_a("Tracks"))
      #expect_that(myMoveSt, is_a("MoveStack")) # already tested
      
      # Test length
      expect_that(length(myMoveSt),
                  is_identical_to(sum(trajObj@tracksData$n)))
      expect_that(length(unique(myMoveSt@trackId)),
                  is_identical_to(length(trajObj@tracks)))
      
      #########
      
      #####
      # Test data
      # TODO: aufräumen
      #
      # delete:
      #expect_that(trajObj@tracks[[1]]@data[1:2, ], is_equivalent_to(myMoveSt@data[1:2, ]))
      #mit hack: identical --> diff attr rownames
      #expect_that(trajObj@tracks[[1]]@data[1:3, ], is_identical_to(myMoveSt@data[1:3, ])) # Attributes rownames
      # Further data testing needs helping objects:
      # delete: warum, wo kommen diese attribute her??? --> Tracks()
      
      # !!!!!!!
      # !?!? ursprüglicher Ansatz (mit Hack) !!!
      # funzt nicht wenn unterschiedliche Attributes vorhanden sind..
      #dataList <- lapply(trajObj@tracks, function(x) x@data)
      #trcsData <- do.call(rbind, dataList)
      #trDataColnames <- names(trcsData)
      #?
      #mStDataColnames <- colnames(myMoveSt@data)
      # need of common colnames because in move::move coloumns were added 
      # and others might be dropped
      #?
      #commonColnames <- intersect(mStDataColnames, trDataColnames)
      # mit Hack:
      # !!!!
      #expect_that(myMoveSt@data[trDataColnames], is_equivalent_to(trcsData))
      ###expect_that(myMoveSt@data[commonColnammes], is_identical_to(trcsData)) # not because of row.names
      
      
      # ohne Hack:, aber unzureichend
      #expect_that(myMoveSt@data[commonColnames], is_equivalent_to(trcsData[commonColnames]))
      # not equivalent because of is.NA value mismatch, dieselben 4 Problemspalten wie im Folgenden!
      # --> 4 Fehler: GPS:Acc, ... Intake.Temp, ....
      # --> Problem: manche Vars (1 Ausprägung) werden in machen move obj rausgeschmissen: moveStack hat dann dort NA!!
      #lapply(trajObj@tracks, function(x) x@data[ ,"GPS.Accuracy"]) --> Track 7
      # --> in move::move wird attribut nach idData verschoben
      # --> in move::moveStack werden dafür dann NAs erzeugt !?!?
      
      
      # !!!
      #? in the case of different attributes in Track objects:
      #nrow(trajObj@tracks[[1]]@data) 
      #myMoveSt@data$Speed # in MoveStack mit NA aufgefüllt
      start_i <- 1
      end_i <- 0
      for (i in 1:length(trajObj@tracks)) {
        names_cur <- names(trajObj@tracks[[i]]@data)
        nrow_cur <- nrow(trajObj@tracks[[i]]@data)
        end_i <- end_i + nrow_cur
        subMoveStDF <- myMoveSt@data[start_i:end_i, names_cur, drop = FALSE]
        trajData <- trajObj@tracks[[i]]@data #[commonColnames], drop = FALSE)
        expect_that(subMoveStDF, is_equivalent_to(trajData))
        #expect_that(subMoveStDF, is_identical_to(trajData))
        # not identical due to row.names and factor levels obviously
        start_i <- start_i + nrow_cur
      }
      
      myMoveStSplitted <- move::split(myMoveSt)
      
      for (h in seq_along(myMoveStSplitted)) {
        names_traj <- names(trajObj@tracks[[h]]@data)
        #expect_that(head(myMoveStSplitted[[h]]@data[names_traj], -1L), 
        #            is_equivalent_to(head(trajObj@tracks[[h]]@data, -1L)))
        expect_that(myMoveStSplitted[[h]]@data[names_traj], 
                    is_equivalent_to(trajObj@tracks[[h]]@data))
        
        myMoveStSp_data <- myMoveStSplitted[[h]]@data[names_traj]
        myTracks_data <- trajObj@tracks[[h]]@data
        row.names(myMoveStSp_data) <- NULL
        row.names(myTracks_data) <- NULL
        
        for (j in seq_along(names_traj)) {
          if (is.factor(myMoveStSp_data[[names_traj[j]]])) {
            
            myMoveStSp_data[names_traj[j]] <- factor(myMoveStSp_data[[names_traj[j]]])
            myTracks_data[names_traj[j]] <- factor(myTracks_data[[names_traj[j]]])
            expect_that(myMoveStSp_data[names_traj[j]], is_equivalent_to(myTracks_data[names_traj[j]]))
            
            myMoveStSp_data[names_traj[j]] <- 
              factor(myMoveStSp_data[[names_traj[j]]], 
                     levels = sort(levels(myMoveStSp_data[[names_traj[j]]])))
            myTracks_data[names_traj[j]] <- 
              factor(myTracks_data[[names_traj[j]]], 
                     sort(levels(myTracks_data[[names_traj[j]]])))
            
            expect_that(levels(myMoveStSp_data[names_traj[j]]), is_identical_to(levels(myTracks_data[names_traj[j]])))
            
            expect_that(myMoveStSp_data[names_traj[j]], is_identical_to(myTracks_data[names_traj[j]]))
                        
          }
        }
        
        expect_that(myMoveStSp_data[names_traj], is_identical_to(myTracks_data))
      }
            
      
      #delete:
      # Try to get another solution without hack..
      #myMoveStMeans <- lapply(myMoveSt@data, function(x) mean(x, na.rm=T))
      #trcsDataMeans <- lapply(trcsData, function(x) mean(x, na.rm=T))
      #expect_that(myMoveStMeans[commonColnames], 
      #            is_equivalent_to(trcsDataMeans[commonColnames]))
      # --> 4 Fehler: GPS:Acc, ... Intake.Temp, ....
      
      # delete:
      # Try to get another solution without hack..
      #trcsColWithoutNA <- trcsData[sapply(trcsData, function(x) !any(is.na(x)))]
      #mStColWithoutNA <- myMoveSt@data[sapply(myMoveSt@data, function(x) !any(is.na(x)))]
      #colnames_trcsColWithoutNA <- names(trcsColWithoutNA)
      #colnames_mStColWithoutNA <- names(mStColWithoutNA)
      #commonColnamesNoNA <- intersect(colnames_trcsColWithoutNA, colnames_mStColWithoutNA)
      #expect_that(myMoveSt@data[commonColnamesNoNA], 
      #            is_equivalent_to(trcsData[commonColnamesNoNA]))
      #expect_that(myMoveSt@data[commonColnamesNoNA], is_identical_to(trcsData[commonColnamesNoNA]))
      # not identical because of row.names
      
      #expect_that(myMoveSt@data[trDataColnames], is_identical_to(trcsData))
      # --> row.names not identical because of rbind
      
      # delete ?? and with just one variable it might not be a data.frame
      #expect_that(as.data.frame(myMoveSt@data[trDataColnames])[1:nrow(trajObj@tracks[[1]]@data), ], 
      #            is_identical_to(trajObj@tracks[[1]]@data))
      
      # Try to get another solution without hack..
      #track1rows <- nrow(trajObj@tracks[[1]]@data)
      #mStDf <- myMoveSt@data[commonColnames]#[1:nrow(trajObj@tracks[[1]]@data), ]
      #mStDf_cut <- mStDf[1:track1rows, , drop = FALSE]
      #mStDF_cut <- myMoveSt@data[commonColnames][1:nrow(trajObj@tracks[[1]]@data), ]
      #identical(mStDf_cut, mStDF_cut)# T
      #trajData_cut <- trajObj@tracks[[1]]@data[commonColnames]
      #expect_that(mStDf_cut, is_equivalent_to(trajData_cut))
      #mStDF_cut2 <- myMoveSt@data[commonColnames][1:nrow(trajObj@tracks[[2]]@data), ]
      #trajData_cut2 <- trajObj@tracks[[2]]@data[commonColnames]
      
      ############
      
      ##
      # Test time
      expect_that(as.numeric(myMoveSt@timestamps), is_equivalent_to(
        unlist(lapply(trajObj@tracks, function(x) zoo::index(x@time))))) # ignores attributes
      
      ##
      # Test geometry
      # delete:
      #expect_that(sp::geometry(as(trajObj, "Spatial")),
      #            is_equivalent_to(sp::geometry(myMoveSt))) # ignores attributes
      # hack for the case of different attributes in the @data slots of the @tracks slot
      tmp <- trajObj
      for (i in 1:length(tmp@tracks)) {
        n <- names(tmp@tracks[[i]]@data)
        tmp@tracks[[i]]@data$zyxwvDummy <- 1:nrow(tmp@tracks[[i]]@data)
        tmp@tracks[[i]]@data <- 
          tmp@tracks[[i]]@data[!names(tmp@tracks[[i]]@data) %in% n]
        }
      expect_that(sp::geometry(myMoveSt),
                  is_identical_to(sp::geometry(as(tmp, "Spatial")))) # ignores attributes
      
      #Test tracksData 
      trcsDataColNames <- names(trajObj@tracksData)
      # delete:
      #expect_that(myMoveSt@idData[ , trcsDataColNames], is_equivalent_to(trajObj@tracksData))
      expect_that(myMoveSt@idData[trcsDataColNames], 
                  is_identical_to(trajObj@tracksData))
      
      # Test myMove@idData and completeness of myMove@data
      dataNamesList <- lapply(trajObj@tracks, function(x) names(x@data))
      commonNames <- Reduce(intersect, dataNamesList)
      expect_that(!any(names(myMoveSt@idData) %in% commonNames), is_identical_to(TRUE))
      expect_that(all(commonNames %in% names(myMoveSt@data)), is_identical_to(TRUE))
            
    })
    
  } # Finish if is(trajObj, "Tracks")
    
  
  
  
  # !ToDo clean / delete 2. part...
  if (is(trajObj, "Tracks")) {
        
  
    #===============================================================================
    #-------------------------------------------------------------------------------
    #
    # TESTING COERCION FROM TRACKS TO MOVEBURST
    #
    #-------------------------------------------------------------------------------
    #===============================================================================
    
    
#     # Test of old approach
#     #===============================================================================
#     #
#     context(paste("toMoveCoercion.R: TEST COERCION FROM TRACKS (", trajObjName,
#                   ") TO MOVEBURST (old approach!!!).", sep = ""))
#     #
#     #===============================================================================
#     
#     #trajObj <- enviroCar_TracksObj1_AttrDiff
#     
#     test_that(paste("Test object ", trajObjName, ":", sep=""), {
#       
#       #myMoveBurst <- as(trajObj, "MoveBurst")
#       myMoveBurst <- tracksToMoveBurst(trajObj)
#       
#       # !!!
#       #length(myMoveBurst) # 937
#       #nrow(myMoveBurst@data) # 937
#       #length(myMoveBurst@burstId) # 936
#       #uniqueBurstIds <- unique(myMoveBurst@burstId)      
#       
#       #sum(trajObj@tracksData$n) # 938
#       #trajObj@tracksData$n[1] # 451
#       #trajObj@tracksData$n[2] # 487
#       
#       #identical(myMoveBurst@coords[1:450,], trajObj@tracks[[1]]@sp@coords[1:450, ])#T
#       #identical(myMoveBurst@coords[451:550,], trajObj@tracks[[2]]@sp@coords[1:100, ])#T
#       # --> die letzte "Zeile" vom ersten Tack ist rausgeflogen !!!!
#       # --> kein guter Ansatz !?
#       
#       
#       # Test classes
#       expect_that(trajObj, is_a("Tracks"))
#       expect_that(myMoveBurst, is_a("MoveBurst"))
#       
#       # Test length
#       #length(myMoveBurst@burstId)# 937 vars / 936 burstIds
#       expect_that(length(myMoveBurst),
#                   is_equivalent_to(sum(trajObj@tracksData$n) - length(trajObj@tracks) + 1))
#       expect_that(length(unique(myMoveBurst@burstId)),
#                   is_identical_to(length(trajObj@tracks)))
#       
#       # Test data
#       #i <- 1
#       start_i <- 1
#       end_i <- 0
#       for (i in 1:length(trajObj@tracks)) {
#         names_cur <- names(trajObj@tracks[[i]]@data)
#         if (i < length(trajObj@tracks)) {
#           nrow_cur <- nrow(trajObj@tracks[[i]]@data) - 1
#           end_i <- end_i + nrow_cur
#           subMoveBurstDF <- myMoveBurst@data[start_i:end_i, names_cur, drop = FALSE]
#           trajData <- head(trajObj@tracks[[i]]@data, -1L) 
#           expect_that(subMoveBurstDF, is_equivalent_to(trajData))
#           #expect_that(subMoveBurstDF, is_identical_to(trajData))
#           # not identical due to row.names and factor levels obviously
#           start_i <- start_i + nrow_cur
#         } else {
#           nrow_cur <- nrow(trajObj@tracks[[i]]@data)
#           end_i <- end_i + nrow_cur
#           subMoveBurstDF <- myMoveBurst@data[start_i:end_i, names_cur, drop = FALSE]
#           trajData <- trajObj@tracks[[i]]@data
#           expect_that(subMoveBurstDF, is_equivalent_to(trajData))
#           #expect_that(subMoveBurstDF, is_identical_to(trajData))
#           # not identical due to row.names and factor levels obviously
#         }
#       }
#       
#       #myMoveBuSplitted <- move::split(myMoveBurst)
#       #str(myMoveBuSplitted) # List
#       #str(trajObj)
#       #nrow(myMoveBuSplitted[[1]]@data) # 451
#       #nrow(trajObj@tracks[[1]]@data) # 451
#       #names_traj <- names(trajObj@tracks[[1]]@data)
#       #names_burst <- names(myMoveBuSplitted[[1]]@data)
#       #expect_that(head(myMoveBuSplitted[[1]]@data[names_traj], -1L), 
#       #            is_equivalent_to(head(trajObj@tracks[[1]]@data, -1L)))
#       #tail(myMoveBuSplitted[[1]]@data[names_traj])
#       #tail(trajObj@tracks[[1]]@data)
#       #expect_that(myMoveBuSplitted[[1]]@data[names_traj], is_equivalent_to(trajObj@tracks[[1]]@data))
#       
#       ##
#       # Test time
#       #expect_that(as.numeric(myMoveBurst@timestamps), is_equivalent_to(
#       #  unlist(lapply(trajObj@tracks, function(x) zoo::index(x@time))))) # ignores attributes
#       
#       #??? hat fehler gegeben ??
#       #expect_that(mb_time[1:nrow(trajObj@tracks[[1]]@data) - 1], is_equivalent_to(
#       #  head(as.numeric(zoo::index(trajObj@tracks[[1]]@time)), -1))) # ignores attributes
#       
#       i <- 1
#       start_i <- 1
#       end_i <- 0
#       for (i in seq_along(trajObj@tracks)) {
#         if (i < length(trajObj@tracks)) {
#           nrow_cur <- nrow(trajObj@tracks[[i]]@data) - 1
#           end_i <- end_i + nrow_cur
#           expect_that(as.numeric(myMoveBurst@timestamps[start_i:end_i]), is_equivalent_to(
#             head(as.numeric(zoo::index(trajObj@tracks[[i]]@time)), -1))) # ignores attributes
#           start_i <- start_i + nrow_cur
#         } else {
#           nrow_cur <- nrow(trajObj@tracks[[i]]@data)
#           end_i <- end_i + nrow_cur
#           expect_that(as.numeric(myMoveBurst@timestamps[start_i:end_i]), is_equivalent_to(
#             as.numeric(zoo::index(trajObj@tracks[[i]]@time)))) # ignores attributes
#         }
#       }
#       
#       ##
#       # Test geometry
#       # delete:
#       #expect_that(sp::geometry(as(trajObj, "Spatial")),
#       #            is_equivalent_to(sp::geometry(myMoveSt))) # ignores attributes
#       # hack for the case of different attributes in the @data slots of the @tracks slot
#       tmp <- trajObj
#       for (i in 1:length(tmp@tracks)) {
#         n <- names(tmp@tracks[[i]]@data)
#         tmp@tracks[[i]]@data$zyxwvDummy <- 1:nrow(tmp@tracks[[i]]@data)
#         tmp@tracks[[i]]@data <- 
#           tmp@tracks[[i]]@data[!names(tmp@tracks[[i]]@data) %in% n]
#       }
#       # preparation
#       #i <- 2
#       start_i <- 1
#       end_i <- 0
#       for (i in seq_along(trajObj@tracks)) {
#         if (i < length(trajObj@tracks)) {
#           nrow_cur <- nrow(trajObj@tracks[[i]]@data) - 1
#           end_i <- end_i + nrow_cur
#           expect_that(sp::geometry(myMoveBurst)[start_i:end_i],
#                       is_identical_to(sp::geometry(as(tmp@tracks[[i]], "Spatial"))[1:nrow_cur])) # ignores attributes
#           start_i <- start_i + nrow_cur
#         } else {
#           nrow_cur <- nrow(trajObj@tracks[[i]]@data)
#           end_i <- end_i + nrow_cur
#           expect_that(sp::geometry(myMoveBurst)[start_i:end_i],
#                       is_identical_to(sp::geometry(as(tmp@tracks[[i]], "Spatial")))) 
#           #expect_that(sp::geometry(myMoveBurst)[start_i:end_i],
#           #            is_equivalent_to(sp::geometry(as(tmp@tracks[[i]], "Spatial")))) # ignores attributes
#         }
#       }
#       
#       
#       #Test tracksData 
#       #trcsDataColNames <- names(trajObj@tracksData)
#       # delete:
#       #expect_that(myMoveSt@idData[ , trcsDataColNames], is_equivalent_to(trajObj@tracksData))
#       #expect_that(myMoveBurst@idData[trcsDataColNames], 
#       #            is_identical_to(trajObj@tracksData))
#       #trcsDataColNames
#       #names(myMoveBurst@idData)
#       
#       # Test myMove@idData and completeness of myMove@data
#       dataNamesList <- lapply(trajObj@tracks, function(x) names(x@data))
#       commonNames <- Reduce(intersect, dataNamesList)
#       expect_that(!any(names(myMoveBurst@idData) %in% commonNames), is_identical_to(TRUE))
#       expect_that(all(commonNames %in% names(myMoveBurst@data)), is_identical_to(TRUE))
#       
#     })
    
    
    #===============================================================================
    #
    context(paste("toMoveCoercion.R: TEST COERCION FROM TRACKS (", trajObjName,
                  ") TO MOVEBURST (new approach).", sep = ""))
    #
    #===============================================================================
    
    #trajObj <- enviroCar_TracksObj1_AttrDiff
    #trajObj <- enviroCar_TracksObj1
    
    test_that(paste("Test object ", trajObjName, ":", sep=""), {
      
      #myMoveBurst <- tracksToMoveBurst(trajObj) 
      # new approach implemented as setAs!
      myMoveBurst <- as(trajObj, "MoveBurst")
      
      # Test classes
      expect_that(trajObj, is_a("Tracks"))
      expect_that(myMoveBurst, is_a("MoveBurst"))
      
      # Test length
      #length(myMoveBurst@burstId)# 937 locs / 936 burstIds
      expect_that(length(myMoveBurst),
                  is_equivalent_to(sum(trajObj@tracksData$n)))# - length(trajObj@tracks) + 1))
      if (length(trajObj@tracks) > 1) { # new approach with burstId 'undefined' for connection of Track obj
        # --> ' + 1 ' added
        expect_that(length(unique(myMoveBurst@burstId)),
                    equals(length(trajObj@tracks)+1))
      }
      
      
      # Test data
      #i <- 1
      start_i <- 1
      end_i <- 0
      #### so teste ich alle tracks ohne den letzten...!
      #for (i in 1:(length(trajObj@tracks) - 1)) {
      for (i in 1:length(trajObj@tracks)) {
        names_cur <- names(trajObj@tracks[[i]]@data)
        #if (i < length(trajObj@tracks)) {
          #nrow_cur <- nrow(trajObj@tracks[[i]]@data) - 1
          nrow_cur <- nrow(trajObj@tracks[[i]]@data)
          end_i <- end_i + nrow_cur
          subMoveBurstDF <- myMoveBurst@data[start_i:end_i, names_cur, drop = FALSE]
          #trajData <- head(trajObj@tracks[[i]]@data, -1L) 
          trajData <- trajObj@tracks[[i]]@data 
          expect_that(subMoveBurstDF, is_equivalent_to(trajData))
          #expect_that(subMoveBurstDF, is_identical_to(trajData))
          # not identical due to row.names and factor levels obviously
          start_i <- start_i + nrow_cur
      }
      
      
      ### Hier gibt noch Probleme mit Test of data und dem (alternativen Ansatz):
      ### U.a. wenn Tracks nur 1 Track object enthält....!?
      ### s.u: nicht ale tracks..
      
      myMoveBuSplitted <- move::split(myMoveBurst)
      
      # -- > so teste ich alle tracks außer den letzten..
      # !!!
      # commented out because of new approach with burstId = 'undefined'
      # !!!
#       for (h in 1:length(myMoveBuSplitted)) {
#         if (h < length(myMoveBuSplitted)) {
#           names_traj <- names(trajObj@tracks[[h]]@data)
#           #expect_that(head(myMoveBuSplitted[[h]]@data[names_traj], -1L), 
#           #            is_equivalent_to(head(trajObj@tracks[[h]]@data, -1L)))
#           expect_that(head(myMoveBuSplitted[[h]]@data[names_traj], -1), 
#                       is_equivalent_to(trajObj@tracks[[h]]@data))
#           
#           myMoveStSp_data <- head(myMoveBuSplitted[[h]]@data[names_traj], -1)
#           myTracks_data <- trajObj@tracks[[h]]@data
#           row.names(myMoveStSp_data) <- NULL
#           row.names(myTracks_data) <- NULL
#           
#           for (j in seq_along(names_traj)) {
#             if (is.factor(myMoveStSp_data[[names_traj[j]]])) {
#               
#               myMoveStSp_data[names_traj[j]] <- factor(myMoveStSp_data[[names_traj[j]]])
#               myTracks_data[names_traj[j]] <- factor(myTracks_data[[names_traj[j]]])
#               expect_that(myMoveStSp_data[names_traj[j]], is_equivalent_to(myTracks_data[names_traj[j]]))
#               
#               myMoveStSp_data[names_traj[j]] <- 
#                 factor(myMoveStSp_data[[names_traj[j]]], 
#                        levels = sort(levels(myMoveStSp_data[[names_traj[j]]])))
#               myTracks_data[names_traj[j]] <- 
#                 factor(myTracks_data[[names_traj[j]]], 
#                        sort(levels(myTracks_data[[names_traj[j]]])))
#               
#               expect_that(levels(myMoveStSp_data[names_traj[j]]), is_identical_to(levels(myTracks_data[names_traj[j]])))
#               
#               expect_that(myMoveStSp_data[names_traj[j]], is_identical_to(myTracks_data[names_traj[j]]))
#               
#             }
#           }
#           
#           expect_that(myMoveStSp_data[names_traj], is_identical_to(myTracks_data))
#         }
#         
#       }
      

      
      # Test time
      #i <- 1
      start_i <- 1
      end_i <- 0
      for (i in seq_along(trajObj@tracks)) {
        #if (i < length(trajObj@tracks)) {
          #nrow_cur <- nrow(trajObj@tracks[[i]]@data) - 1
          nrow_cur <- nrow(trajObj@tracks[[i]]@data)
          end_i <- end_i + nrow_cur
          expect_that(as.numeric(myMoveBurst@timestamps[start_i:end_i]), is_equivalent_to(
            as.numeric(zoo::index(trajObj@tracks[[i]]@time)))) # ignores attributes
          start_i <- start_i + nrow_cur
      }
      
      ##
      # Test geometry
      # delete:
      #expect_that(sp::geometry(as(trajObj, "Spatial")),
      #            is_equivalent_to(sp::geometry(myMoveSt))) # ignores attributes
      # hack for the case of different attributes in the @data slots of the @tracks slot
      tmp <- trajObj
      for (i in 1:length(tmp@tracks)) {
        n <- names(tmp@tracks[[i]]@data)
        tmp@tracks[[i]]@data$zyxwvDummy <- 1:nrow(tmp@tracks[[i]]@data)
        tmp@tracks[[i]]@data <- 
          tmp@tracks[[i]]@data[!names(tmp@tracks[[i]]@data) %in% n]
      }
      # preparation
      #print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      #print(sp::geometry(myMoveBurst)) # hier wird "+ellps=WGS84 +towgs84=0,0,0" hinzugefügt
      #print(sp::geometry(as(tmp@tracks[[1]], "Spatial")))
      #print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      #i <- 2
      start_i <- 1
      end_i <- 0
      for (i in seq_along(trajObj@tracks)) {
        #if (i < length(trajObj@tracks)) {
          #nrow_cur <- nrow(trajObj@tracks[[i]]@data) - 1
          nrow_cur <- nrow(trajObj@tracks[[i]]@data)
          end_i <- end_i + nrow_cur
          #expect_that(sp::geometry(myMoveBurst)[start_i:end_i],
          #            is_identical_to(sp::geometry(as(tmp@tracks[[i]], "Spatial"))[1:nrow_cur]))
          # --> for some suspicious reason "+ellps=WGS84 +towgs84=0,0,0" is added to sp::geometry(myMoveBurst))
          # just in the case of enviroCar trajectory data.
          # --> not identical, but...
          expect_that(sp::geometry(myMoveBurst)[start_i:end_i],
                      is_equivalent_to(sp::geometry(as(tmp@tracks[[i]], "Spatial"))[1:nrow_cur])) # ignores attributes
          start_i <- start_i + nrow_cur
      }

      # Test myMove@idData and completeness of myMove@data
      dataNamesList <- lapply(trajObj@tracks, function(x) names(x@data))
      commonNames <- Reduce(intersect, dataNamesList)
      expect_that(!any(names(myMoveBurst@idData) %in% commonNames), is_identical_to(TRUE))
      expect_that(all(commonNames %in% names(myMoveBurst@data)), is_identical_to(TRUE))

    })
  
  } # Finish if ...
  
  
  
  if (is(trajObj, "TracksCollection")) {
    
    
    #===============================================================================
    #-------------------------------------------------------------------------------
    #
    # TESTING COERCION FROM TRACKSCOLLECTION TO MOVESTACK
    #
    #-------------------------------------------------------------------------------
    #===============================================================================
    
    
    #===============================================================================
    #
    context(paste("toMoveCoercion.R: TEST COERCION FROM TRACKSCOLLECTION (", 
                  trajObjName, ") TO MOVESTACK.", sep = ""))
    #
    #===============================================================================
    
    #trajObj <- enviroCar_TracksColl
    
    test_that(paste("Test object ", trajObjName, ":", sep=""), {
      
      myMoveSt <- as(trajObj, "MoveStack")
      
      # Test classes
      expect_that(trajObj, is_a("TracksCollection"))
      #expect_that(myMoveSt, is_a("MoveStack"))
      
      # Test length
      expect_that(length(myMoveSt),
                  is_identical_to(
                    sum(unlist(lapply(trajObj@tracksCollection, function(x) x@tracksData$n)))))
      expect_that(length(unique(myMoveSt@trackId)),
                  is_identical_to(
                    sum(unlist(lapply(trajObj@tracksCollection, function(x) length(x@tracks))))))
                                                    
      
      # Test Data
      start_h <- 1
      end_h <- 0
      for (h in 1:length(trajObj@tracksCollection)){
       
        start_i <- start_h
        end_i <- end_h
        
        for (i in 1:length(trajObj@tracksCollection[[h]]@tracks)) {
          names_cur <- names(trajObj@tracksCollection[[h]]@tracks[[i]]@data)
          nrow_cur <- nrow(trajObj@tracksCollection[[h]]@tracks[[i]]@data)
          end_i <- end_i + nrow_cur
          subMoveStDF <- myMoveSt@data[start_i:end_i, names_cur, drop = FALSE]
          #... check for factor
          trajData <- trajObj@tracksCollection[[h]]@tracks[[i]]@data 
          expect_that(subMoveStDF, is_equivalent_to(trajData))
          #expect_that(subMoveStDF, is_identical_to(trajData))
          start_i <- start_i + nrow_cur
        }
        
        start_h <- start_i
        end_h <- end_i
        
      }
      
      
      myMoveStSplitted <- move::split(myMoveSt)
      
      counter <- 0
      for (h in seq_along(trajObj@tracksCollection)) {
        for (i in seq_along(trajObj@tracksCollection[[h]]@tracks)) {
          counter <- counter + 1
          names_traj <- names(trajObj@tracksCollection[[h]]@tracks[[i]]@data)
          expect_that(myMoveStSplitted[[counter]]@data[names_traj], 
                      is_equivalent_to(trajObj@tracksCollection[[h]]@tracks[[i]]@data))
          
          myMoveStSp_data <- myMoveStSplitted[[counter]]@data[names_traj]
          myTracks_data <- trajObj@tracksCollection[[h]]@tracks[[i]]@data
          
          row.names(myMoveStSp_data) <- NULL
          row.names(myTracks_data) <- NULL
          
          for (j in seq_along(names_traj)) {
            if (is.factor(myMoveStSp_data[[names_traj[j]]])) {
              
              myMoveStSp_data[names_traj[j]] <- factor(myMoveStSp_data[[names_traj[j]]])
              myTracks_data[names_traj[j]] <- factor(myTracks_data[[names_traj[j]]])
              expect_that(myMoveStSp_data[names_traj[j]], is_equivalent_to(myTracks_data[names_traj[j]]))
              
              myMoveStSp_data[names_traj[j]] <- 
                factor(myMoveStSp_data[[names_traj[j]]], 
                       levels = sort(levels(myMoveStSp_data[[names_traj[j]]])))
              myTracks_data[names_traj[j]] <- 
                factor(myTracks_data[[names_traj[j]]], 
                       sort(levels(myTracks_data[[names_traj[j]]])))
              
              expect_that(levels(myMoveStSp_data[names_traj[j]]), is_identical_to(levels(myTracks_data[names_traj[j]])))
              
              expect_that(myMoveStSp_data[names_traj[j]], is_identical_to(myTracks_data[names_traj[j]]))
              
            }
          }
          expect_that(myMoveStSp_data[names_traj], is_identical_to(myTracks_data))
        }
      }
        
       
      # Test time
      expect_that(as.numeric(myMoveSt@timestamps), is_equivalent_to(
        unlist(lapply(trajObj@tracksCollection, function(X) {
          lapply(X@tracks, function(x) zoo::index(x@time)) })))) 
                      
      
      ##
      # Test geometry
      # delete:
      #expect_that(sp::geometry(as(trajObj, "Spatial")),
      #            is_equivalent_to(sp::geometry(myMoveSt))) # ignores attributes
      # hack for the case of different attributes in the @data slots of the @tracks slot
      #h <- 1
      start_h <- 1
      end_h <- 0
      for (h in 1:length(trajObj@tracksCollection)) {
        tmp <- trajObj@tracksCollection[[h]]
        n_cur <- sum(trajObj@tracksCollection[[h]]@tracksData["n"])
        end_h <- end_h + n_cur
        # hack for the case of different attributes in the @data slots of the @tracks slot
        for (i in 1:length(tmp@tracks)) {
          n <- names(tmp@tracks[[i]]@data)
          tmp@tracks[[i]]@data$zyxwvDummy <- 1:nrow(tmp@tracks[[i]]@data)
          tmp@tracks[[i]]@data <- 
            tmp@tracks[[i]]@data[!names(tmp@tracks[[i]]@data) %in% n]
        }
        #expect_that(sp::geometry(myMoveSt)[start_h:end_h],
        #            is_identical_to(sp::geometry(as(tmp, "Spatial")))) # ignores attributes
        expect_that(sp::geometry(myMoveSt)[start_h:end_h],
                    is_equivalent_to(sp::geometry(as(tmp, "Spatial")))) # ignores attributes
        start_h <- start_h + n_cur
      }
      
      
      # Test MoveStack idData
      #trcsCollDataColNames <- names(trajObj@tracksCollectionData)
      
      ##tracksDataList <- lapply(trajObj@tracksCollection, function(x) x@tracksData)
      ##maxColCount <- max(sapply(dataNamesList, function(x) length(x)))
      ##commonNames <- Reduce(intersect, dataNamesList)
      
      trcsDataColNames <- lapply(trajObj@tracksCollection, function(x) names(x@tracksData))
      commonNames <- Reduce(intersect, trcsDataColNames)
      # "Just" equivalent, because row.names changed by rbind in the test.
      mSt <- myMoveSt@idData[commonNames]
      # !!! hier evtl mit plyr und rbind.fill !?! aber wie funzt das in testcase???
      trajO <- do.call(rbind, lapply(trajObj@tracksCollection, function(x) x@tracksData))
      
      mSt$tmin <- as.numeric(mSt$tmin)
      mSt$tmax <- as.numeric(mSt$tmax)
      trajO$tmin <- as.numeric(trajO$tmin)
      trajO$tmax <- as.numeric(trajO$tmax)
      expect_that(mSt, is_equivalent_to(trajO))

      
      # Test myMove@idData and completeness of myMove@data
      v <- vector("list", sum(trajObj@tracksCollectionData$n))
      counter <- 0
      for (i in 1:length(trajObj@tracksCollection)) {
        for (j in 1:length(trajObj@tracksCollection[[i]]@tracks)) {
          counter <- counter + 1
          v[[counter]] <- names(trajObj@tracksCollection[[i]]@tracks[[j]]@data)
        }
      }
      
      commonNames <- Reduce(intersect, v)
      expect_that(!any(names(myMoveSt@idData) %in% commonNames), is_identical_to(TRUE))
      expect_that(all(commonNames %in% names(myMoveSt@data)), is_identical_to(TRUE))
      
    })
    
  } # Finish if ...
  
} # Finish for-loop

print(Sys.time())