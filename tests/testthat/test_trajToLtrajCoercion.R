#test_file(paste(getwd(),"/tests/testthat/test_trajToLtrajCoercion.R", sep=""))
#===============================================================================
#
# TO RUN JUST THIS TEST FILE
#
#===============================================================================

#require(sp)
#require(testthat)
#test_file(paste(getwd(),"/tests/testthat/test_trajToLtrajCoercion.R", sep=""))

#-------------------------------------------------------------------------------



#!!!!!!!!!!!!!!!!!!!
# TODO
# - provide Data in pkg dir /data
# - extend examplesTrack data, also for testing aggregation
# - ...

# Important
# - order of tests ok !!!
# - no NAs in coords + time
# - always infolocs
# - ...
#!!!!!!!!!!!!!!!!!!!!

print(Sys.time())

#####
#delete
# ?!

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

# Track object from enviroCar (single Track)
trackList <- lapply(enviroCar_TracksObj1@tracks, function(x) x)
enviroCar_TrackObj1_1 <- trackList[[1]]

# trajectories objects with different (number of) columns

# Tracks object
trackList[[1]]@data$Speed <- NULL
enviroCar_TracksObj1_AttrDiff <- trajectories::Tracks(trackList)
# --> Tracks object with different (number of) columns (26 vs. 27) in the @data slots of
# the corresponding Track objects.

# TracksCollection object
trackList[[1]]@data$GPS.Accuracy <- NULL
trackList[[2]]@data$Speed <- NULL
trcs <- trajectories::Tracks(trackList)
enviroCar_TracksColl_AttrDiff <- trajectories::TracksCollection(list(trcs, enviroCar_TracksObj2))
# --> different number of attributes (25, 26, 27) in the appendent Track objects

# Tracks and TracksCollection with just one Track resp. Tracks object
A1_as_Tracks <- trajectories::Tracks(list(A1))
A_as_TrColl <- trajectories::TracksCollection(list(A))


# Identical Track object names in several Tracks objects
#enviroCar_TracksColl_identTrNames <- enviroCar_TracksColl
tracksList <- lapply(enviroCar_TracksColl@tracksCollection, function(x) x)
orig_names <- names(tracksList[[1]]@tracks)
names(tracksList[[1]]@tracks) <- paste("Track", 1:length(tracksList[[1]]@tracks), sep = "_")
names(tracksList[[2]]@tracks) <- paste("Track", 1:length(tracksList[[2]]@tracks), sep = "_")
enviroCar_TracksColl_identTrackNames <- trajectories::TracksCollection(tracksList)
# --> identical Track names in the different Tracks objects


#delete:
# Identical Tracks object names in TracksCollection objects
#tracksList <- lapply(enviroCar_TracksColl@tracksCollection, function(x) x)
#names(tracksList) <- c("enviroCarTracks", "enviroCarTracks")
#enviroCar_TracksColl_identTracksNames <- trajectories::TracksCollection(tracksList)
# --> not possible to create TrColl object with identical Tracks names!


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
                        enviroCar_TracksColl_AttrDiff, # Differences in Attributes of Tracks objects
                        
                        enviroCar_TracksColl_identTrackNames # Identical Track Names in Tracks objects
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
                         "enviroCar_TracksColl_AttrDiff", # Differences in Attributes
                         "enviroCar_TracksColl_identTrackNames" # Identical Track Names in Tracks objects
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
  
  
  #===============================================================================
  #-------------------------------------------------------------------------------
  #
  # TESTING COERCION FROM TRACK TO LTRAJ
  #
  #-------------------------------------------------------------------------------
  #===============================================================================
  
  
  if (is(trajObj, "Track")) {
    
    
    #===============================================================================
    #
    context(paste("toLtrajCoercion.R: TEST COERCION FROM TRACK (", trajObjName,
                  ") TO LTRAJ.", sep = ""))
    #
    #===============================================================================
    
    
    test_that(paste("Test object ", trajObjName, ":", sep=""), {
          
      myLtraj <- as(trajObj, "ltraj")
            
      # Test classes
      expect_that(trajObj, is_a("Track"))
      #expect_that(myLtraj, is_a("ltraj")) # already tested
      
      # Test length
      expect_that(nrow(myLtraj[[1]]), is_identical_to(length(trajObj)))      
      
      # Test data
      expect_that(attributes(myLtraj[[1]])$infolocs, is_equivalent_to(trajObj@data))
      #expect_that(attributes(myLtraj[[1]])$infolocs, is_identical_to(trajObj@data))
      # equivalent but not identical due to row.names (infolocs row.names always of type character)
      tmpData <- trajObj@data
      attr(tmpData, "row.names") <- row.names(trajObj@data)
      expect_that(attributes(myLtraj[[1]])$infolocs, is_identical_to(tmpData))
      
      # Test time
      expect_that(myLtraj[[1]]$date, is_equivalent_to(zoo::index(trajObj@time))) # ignores attributes
      #expect_that(myLtraj[[1]]$date, is_identical_to(zoo::index(trajObj@time))) # attributes length mismatch
      # --> attribute tclass, which comes from xts object representing time in Track
      myTime <- zoo::index(trajObj@time)
      attr(myTime, "tclass") <- NULL
      expect_that(myLtraj[[1]]$date, is_identical_to(myTime)) # 
      
      # Test geometry / coords
      expect_that(myLtraj[[1]]$x, is_identical_to(trajObj@sp@coords[,1]))
      expect_that(myLtraj[[1]]$y, is_identical_to(trajObj@sp@coords[,2]))
      
      # Test row.names
      expect_that(row.names(attributes(myLtraj[[1]])$infolocs), is_identical_to(
        row.names(trajObj@data)))
      expect_that(attr(myLtraj[[1]], "row.names"), is_identical_to(
        attr(trajObj@data, "row.names")))
      # row.names in infolocs and ltraj df need to be "identical"
      # (But rownames of infolocs are always of type character.):
      expect_that(row.names(myLtraj[[1]]), is_identical_to(
        attr(attributes(myLtraj[[1]])$infolocs, "row.names")))
      
      # Test names
      expect_that(names(attributes(myLtraj[[1]])$infolocs), is_identical_to(
        names(trajObj@data)))
      
      # Test connections slot data
      # connections data can not be respected in coercion to ltraj objects
      
    })
    
  } # Finish if is(trajObj, "Track")
  
  
  
  #===============================================================================
  #-------------------------------------------------------------------------------
  #
  # TESTING COERCION FROM TRACKS TO LTRAJ
  #
  #-------------------------------------------------------------------------------
  #===============================================================================
  
  
  if (is(trajObj, "Tracks")) {
    
    
    #===============================================================================
    #
    context(paste("toLtrajCoercion.R: TEST COERCION FROM TRACKS (", trajObjName,
                  ") TO LTRAJ.", sep = ""))
    #
    #===============================================================================
    
    test_that(paste("Test object ", trajObjName, ":", sep=""), {
      
      #trajObj <- enviroCar_TracksObj1_AttrDiff
      
      myLtraj <- as(trajObj, "ltraj")
      
      # Test classes
      expect_that(trajObj, is_a("Tracks"))
      #expect_that(myLtraj, is_a("ltraj")) # already tested
      
      # Test length
      expect_that(length(myLtraj), is_identical_to(length(trajObj@tracks)))
      for (i in seq_along(trajObj@tracks)) {
        expect_that(nrow(myLtraj[[i]]), is_identical_to(length(trajObj@tracks[[i]])))      
      }
      
      # Test data
      #i <- 2
      for (i in seq_along(trajObj@tracks)) {
        colNames <- names(trajObj@tracks[[i]]@data)
        expect_that(attributes(myLtraj[[i]])$infolocs[colNames], is_equivalent_to(
          trajObj@tracks[[i]]@data[colNames]))
        #expect_that(attributes(myLtraj[[i]])$infolocs[colNames], is_identical_to(
        #  trajObj@tracks[[i]]@data[colNames])) # not identical due to row.names ...
        tmpData <- trajObj@tracks[[i]]@data[colNames]
        attr(tmpData, "row.names") <- row.names(trajObj@tracks[[i]]@data[colNames])
        expect_that(attributes(myLtraj[[i]])$infolocs[colNames], is_identical_to(
          tmpData))
      }        
      
      # delete:
      ## Advanced approach - does not seem to be needed...
      #for (i in seq_along(trajObj@tracks)) {
      #  colNames <- names(trajObj@tracks[[i]]@data)
      #  #expect_that(attributes(myLtraj[[i]])$infolocs[colNames], is_equivalent_to(trajObj@tracks[[i]]@data[colNames]))
      #  for (j in seq_along(colNames)) {
      #    name_cur <- colNames[j]
      #    expect_that(attributes(myLtraj[[i]])$infolocs[name_cur], is_equivalent_to(trajObj@tracks[[i]]@data[name_cur]))
      #  }
      #}
      
      # Test time
      for (i in seq_along(trajObj@tracks)) {
        expect_that(myLtraj[[i]]$date, is_equivalent_to(zoo::index(trajObj@tracks[[i]]@time)))      
      }
      for (i in seq_along(trajObj@tracks)) {
        myTime <- zoo::index(trajObj@tracks[[i]]@time)
        attr(myTime, "tclass") <- NULL
        expect_that(myLtraj[[i]]$date, is_identical_to(myTime)) # 
      }
      
      # Test geometry / coordinates
      for (i in seq_along(trajObj@tracks)) {
        expect_that(myLtraj[[i]]$x, is_identical_to(trajObj@tracks[[i]]@sp@coords[,1]))
        expect_that(myLtraj[[i]]$y, is_identical_to(trajObj@tracks[[i]]@sp@coords[,2]))
      }
      
      # Test row.names
      for (i in seq_along(trajObj@tracks)) {
        expect_that(row.names(attributes(myLtraj[[i]])$infolocs), is_identical_to(
          row.names(trajObj@tracks[[i]]@data)))
        expect_that(attr(myLtraj[[i]], "row.names"), is_identical_to(
          attr(trajObj@tracks[[i]]@data, "row.names")))
        
        ### Problem with test object "enviroCarTracksObj" und "enviroCarTracksCollObj" !!
        ### TODO --> entscheiden ob code toLtrajCoercion angepasst werden muss...!
        ### --> eigentlich sind die row.names von connections auch egal, oder?
        ##if (!identical(trajObj, enviroCarTracksObj)) {
        ##  expect_that(attr(head(myLtraj[[i]], -1L), "row.names"), is_identical_to(
        ##    attr(trajObj@tracks[[i]]@connections, "row.names")))
        ##}
        
      }
      
      # row.names in infolocs and ltraj df need to be "identical"
      # (But rownames of infolocs are always of type character.)
      for (i in seq_along(myLtraj)) {
        expect_that(row.names(myLtraj[[i]]), is_identical_to(
          attr(attributes(myLtraj[[i]])$infolocs, "row.names")))
      }
      
      # Test names
      for (i in seq_along(trajObj@tracks)) {
        colNames <- names(trajObj@tracks[[i]]@data)
        expect_that(names(attributes(myLtraj[[i]])$infolocs[colNames]), is_identical_to(
          names(trajObj@tracks[[i]]@data[colNames])))
      }
      
      # Test connections slot data
      # connections data can not be respected in coercion to ltraj objects
      
    })
    
  } # Finish if (is(trajObj, "Tracks"))
  
  
  
  #===============================================================================
  #-------------------------------------------------------------------------------
  #
  # TESTING COERCION FROM TRACKSCOLLECTION TO LTRAJ
  #
  #-------------------------------------------------------------------------------
  #===============================================================================
  
  
  if (is(trajObj, "TracksCollection")) {
    
    
    #===============================================================================
    #
    context(paste("toLtrajCoercion.R: TEST COERCION FROM TRACKSCOLLECTION (", trajObjName,
                  ") TO LTRAJ.", sep = ""))
    #
    #===============================================================================
    
    
    test_that(paste("Test object ", trajObjName, ":", sep=""), {
      
      myLtraj <- as(trajObj, "ltraj")
      
      # Test classes
      expect_that(trajObj, is_a("TracksCollection"))
      #expect_that(myLtraj, is_a("ltraj")) # already tested
      
      # Test length
      expect_that(length(myLtraj), is_identical_to(sum(trajObj@tracksCollectionData$n)))
      expect_that(length(myLtraj), is_identical_to(
        sum(unlist(lapply(trajObj@tracksCollection, function(x) length(x@tracks))))))
      expect_that(sum(sapply(myLtraj, function(x) nrow(x))), is_identical_to(
        sum(unlist(lapply(trajObj@tracksCollection, function(x) x@tracksData$n)))))
      counter <- 0
      for (i in 1:length(trajObj@tracksCollection)) {
        for (j in 1:length(trajObj@tracksCollection[[i]]@tracks)) {
          counter <- counter + 1
          expect_that(nrow(myLtraj[[counter]]), is_identical_to(
            nrow(trajObj@tracksCollection[[i]]@tracks[[j]]@data)))
        }
      }
      
      # Test data
      counter <- 0
      for (h in seq_along(trajObj@tracksCollection)) {
        for (i in seq_along(trajObj@tracksCollection[[h]]@tracks)) {
          counter <- counter + 1
          colNames <- names(trajObj@tracksCollection[[h]]@tracks[[i]]@data)
          expect_that(attributes(myLtraj[[counter]])$infolocs[colNames], is_equivalent_to(
            trajObj@tracksCollection[[h]]@tracks[[i]]@data[colNames]))
          #expect_that(attributes(myLtraj[[counter]])$infolocs[colNames], is_identical_to(
          #  trajObj@tracksCollection[[h]]@tracks[[i]]@data[colNames])) 
          # --> not identical due to row.names
          tmpData <- trajObj@tracksCollection[[h]]@tracks[[i]]@data[colNames]
          attr(tmpData, "row.names") <- 
            row.names(trajObj@tracksCollection[[h]]@tracks[[i]]@data[colNames])
          expect_that(attributes(myLtraj[[counter]])$infolocs[colNames], is_identical_to(
            tmpData))
        }
      }
      
      # Test time
      counter <- 0
      for (h in seq_along(trajObj@tracksCollection)) {
        for (i in seq_along(trajObj@tracksCollection[[h]]@tracks)) {
          counter <- counter + 1
          expect_that(myLtraj[[counter]]$date, is_equivalent_to(
            zoo::index(trajObj@tracksCollection[[h]]@tracks[[i]]@time)))      
        } 
      }
      counter <- 0
      for (h in seq_along(trajObj@tracksCollection)) {
        for (i in seq_along(trajObj@tracksCollection[[h]]@tracks)) {
          counter <- counter + 1
          myTime <- zoo::index(trajObj@tracksCollection[[h]]@tracks[[i]]@time)
          attr(myTime, "tclass") <- NULL
          expect_that(myLtraj[[counter]]$date, is_identical_to(myTime)) # 
        }
      }
      
      # Test geometry / coordinates
      counter <- 0
      for (h in seq_along(trajObj@tracksCollection)) {
        for (i in seq_along(trajObj@tracksCollection[[h]]@tracks)) {
          counter <- counter + 1
          expect_that(myLtraj[[counter]]$x, is_identical_to(
            trajObj@tracksCollection[[h]]@tracks[[i]]@sp@coords[,1]))
          expect_that(myLtraj[[counter]]$y, is_identical_to(
            trajObj@tracksCollection[[h]]@tracks[[i]]@sp@coords[,2]))
        }
      }
      
      # Test row.names
      counter <- 0
      for (h in seq_along(trajObj@tracksCollection)) {
        for (i in seq_along(trajObj@tracksCollection[[h]]@tracks)) {
          counter <- counter + 1
          expect_that(row.names(attributes(myLtraj[[counter]])$infolocs), is_identical_to(
            row.names(trajObj@tracksCollection[[h]]@tracks[[i]]@data)))
          expect_that(attr(myLtraj[[counter]], "row.names"), is_identical_to(
            attr(trajObj@tracksCollection[[h]]@tracks[[i]]@data, "row.names")))
          
          ### delete ?!?!
          ### --> eigentlich sind die row.names von connections auch egal, oder?
          ##if (!identical(trajObj, enviroCarTracksCollObj)) {
          ##  expect_that(attr(head(myLtraj[[counter]], -1L), "row.names"), is_identical_to(
          ##    attr(trajObj@tracksCollection[[h]]@tracks[[i]]@connections, "row.names")))
          ##}
        }
      }

      # row.names in infolocs and ltraj df need to be "identical"
      # (But rownames of infolocs are always of type character.)
      for (i in seq_along(myLtraj)) {
        expect_that(row.names(myLtraj[[i]]), is_identical_to(
          attr(attributes(myLtraj[[i]])$infolocs, "row.names")))
      }
      
      # Test names
      counter <- 0
      for (h in seq_along(trajObj@tracksCollection)) {
        for (i in seq_along(trajObj@tracksCollection[[h]]@tracks)) {
          counter <- counter + 1
          colNames <- names(trajObj@tracksCollection[[h]]@tracks[[i]]@data)
          expect_that(names(attributes(myLtraj[[counter]])$infolocs[colNames]), 
                      is_identical_to(
                        names(trajObj@tracksCollection[[h]]@tracks[[i]]@data[colNames])))
        }
      }
      
      # Test connections slot data
      # connections data can not be respected in coercion to ltraj objects
      
    })
    
  } # Finish if...
  
} # Finish for-loop

print(Sys.time())