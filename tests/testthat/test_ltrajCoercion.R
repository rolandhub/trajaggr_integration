#test_file(paste(getwd(),"/tests/testthat/test_ltrajCoercion.R", sep=""))
#===============================================================================
#
# JUST RUN THIS TEST FILE
#
#===============================================================================

#require(testthat)
#test_file(paste(getwd(),"/tests/testthat/test_ltrajCoercion.R", sep=""))

#-------------------------------------------------------------------------------

# 20141117: Problem: Error: Wenn ich Skript diekt ausführen möchte...
# Fehler in as(x, "Track") : konnte Funktion "asMethod" nicht finden

print(Sys.time())

#####
#!!!
#require(adehabitatLT) # brauche ich (auch) wenn ich hier direkt wasmachen will ! ???
#!!! z.b. class(puechcirc[1]) ltraj, list
# aber ohne require(ade..) class(puechcirc[1]) list (und nicht ltraj!) !!!
# ?! evtl lässt sich das auch über "importFrom adehabitatLT as.ltraj" erledigen !?
# --> JA !!!
# oder über zuvor erstellten neue testdaten !! ERLEDIGT!!!
#####

require(sp)
#require(spacetime) # STIDF()
#require(trajectories) # Track()

# brauch ich  nit !?!
##require(methods)
##require(zoo)
##require(plyr)
##require(rgeos)

# brauche ich (auch) wenn ich hier direkt wasmachen will ! ???
#require(testthat)


# ####
# 
# #! nur wenn ich skript direkt ausführen möchte:
# #(sonst automatisch!)
# require(testthat)
# 
# #! nur wenn ich skript direkt ausführen möchte:
# #(sonst automatisch)
# source("/home/harry/BSc_Thesis_Traj/R/R_wd/myPkg/trajaggr/R/ltrajCoercion.R")
#
# 
# !!!!!
# # Load example data (for direct development)
# # !!! #
# load(paste(getwd(), "/tests/testthat/", "adehabitatLT_ExData.RData", sep=""))
# ####


#####

# "!!!!!"verlegt!!!
# Load test data --> testthat needs this data for tests!
#load("adehabitatLT_ExData.RData")
#load("adehabitatLT_extendedExData.RData")
#data(puechcirc)
#data(puechabonsp)
#class(puechabonsp_ltraj)
#as.character(puechabonsp_ltraj)
#getObjectName(puechabonsp_ltraj)
#####

#objectNameToCharacter <- function(x) deparse(substitute(x)) 

#####
#save(albatross, bear, capreotf, hseal, ibex, porpoise, 
#     file="adehabitatLT_AddTmpExData_ForTesting.RData")
#load("adehabitatLT_AddTmpExData_ForTesting.RData")
load("adehabitatLT_ExData.RData")
#load(paste(getwd(), "/tests/testthat/", "adehabitatLT_AddTmpExData_ForTesting.RData", sep=""), verbose = T)
# --> albatross, bear, ...
#load(paste(getwd(), "/tests/testthat/", "adehabitatLT_ExData.RData", sep=""), verbose = T)
# --> puechcirc, ..., puechabonsp_ltraj_cut (= wildboars_4Ind_ltraj), capreochiz_ltraj, ...
#identical(wildboars_4Ind_ltraj, puechabonsp_ltraj_cut) # TRUE !!!
##
# ??? puechcirc puechcirc1 puechcirc2 puechcirc3 puechabonsp
#load(paste(getwd(), "/tests/testthat/", "adehabitatLT_extendedExData.RData", sep=""), verbose = T)
#####

#===============================================================================
#
# PREPARATION
#
#===============================================================================

# Creation of further ltraj objects for testing relating to NA values and infolocs

# One trajectory

# capreochiz_ltraj without infolocs
capreochiz_ltraj_noInfoL <- capreochiz_ltraj
attributes(capreochiz_ltraj_noInfoL[[1]])$infolocs <- NULL

# capreochiz_ltraj with NA values in coords and time
capreochiz_ltraj_withNA <- capreochiz_ltraj
capreochiz_ltraj_withNA[[1]]$x[c(1,5,6,7,8,15)] <- NA
capreochiz_ltraj_withNA[[1]]$y[c(3,12,200)] <- NA
#capreochiz_ltraj_withNA[[1]]$date[c(99)] <- NA

# capreochiz_ltraj with NA values in coords and time and without infolocs
capreochiz_ltraj_withNA_noInfoL <- capreochiz_ltraj_withNA
capreochiz_ltraj_withNA_noInfoL[[1]]$date[c(100:115, nrow(capreochiz_ltraj[[1]]))] <- NA
attributes(capreochiz_ltraj_withNA_noInfoL[[1]])$infolocs <- NULL


# Serveral trajectories with same id

# capreochiz_ltraj_RoeDeer_bursts without infolocs
capreochiz_ltraj_RoeDeer_bursts_noInfoL <- capreochiz_ltraj_RoeDeer_bursts
capreochiz_ltraj_RoeDeer_bursts_noInfoL <- 
  adehabitatLT::removeinfo(capreochiz_ltraj_RoeDeer_bursts_noInfoL)

# capreochiz_ltraj_RoeDeer_bursts with NA values in coords and time
capreochiz_ltraj_RoeDeer_bursts_withNA <- capreochiz_ltraj_RoeDeer_bursts
capreochiz_ltraj_RoeDeer_bursts_withNA[[1]]$x[c(1,5,6,7,8,15)] <- NA
capreochiz_ltraj_RoeDeer_bursts_withNA[[1]]$y[c(3,12,20)] <- NA
capreochiz_ltraj_RoeDeer_bursts_withNA[[1]]$date[c(13)] <- NA


# Several trajectories with several ids 

# puechabonsp_ltraj with NA values
puechabonsp_ltraj_withNA <- puechabonsp_ltraj
puechabonsp_ltraj_withNA[[1]]$x[c(1,5,6)] <- NA
puechabonsp_ltraj_withNA[[3]]$y[c(4,8)] <- NA
puechabonsp_ltraj_withNA[[3]]$date[c(3,5)] <- NA

# puechabonsp_ltraj_cut with NA values
puechabonsp_ltraj_cut_withNA <- puechabonsp_ltraj_cut
puechabonsp_ltraj_cut_withNA[[1]]$x[c(2,3,4)] <- NA
puechabonsp_ltraj_cut_withNA[[3]]$y[c(4,8)] <- NA
puechabonsp_ltraj_cut_withNA[[5]]$date[c(1,5)] <- NA
#puechabonsp_ltraj_cut_withNA[[5]]$date[c(3,5)] <- NA


#-------------------------------------------------------------------------------



#===============================================================================
#
# CREATION OF A LIST OF LTRAJ OBJECTS TO BE TESTED
#
#===============================================================================

ltrajObjectsList <- list(puechcirc,
                         puechcirc_CH93,
                         puechcirc_JE93,
                         puechabonsp_ltraj,
                         puechabonsp_ltraj_cut,
                         puechabonsp_ltraj_Chou_bursts,
                         capreochiz_ltraj,
                         ##, ### to be deleted (because of file size)
                         #capreochiz_ltraj_RoeDeer_bursts, ###
                         ##, ### to be deleted, from load("...RData")
                         #albatross, ###
                         #bear, ###
                         #capreotf, ###
                         #hseal, ###
                         #ibex,
                         #porpoise, 
                         ##, ### additionally created
                         #capreochiz_ltraj_noInfoL,
                         #capreochiz_ltraj_withNA,
                         #capreochiz_ltraj_withNA_noInfoL,
                         #capreochiz_ltraj_RoeDeer_bursts_noInfoL,
                         #capreochiz_ltraj_RoeDeer_bursts_withNA,
                         puechabonsp_ltraj_withNA,
                         puechabonsp_ltraj_cut_withNA)

# geht das auch einfacher ???
ltrajObjNamesList <- list("puechcirc",
                          "puechcirc_CH93",
                          "puechcirc_JE93",
                          "puechabonsp_ltraj",
                          "puechabonsp_ltraj_cut",
                          "puechabonsp_ltraj_Chou_bursts",
                          "capreochiz_ltraj",
                          ###, ### to be deleted (because of file size)
                          #"capreochiz_ltraj_RoeDeer_bursts", ###
                          ###, ### to be deleted, from load("...RData")
                          #"albatross", ###
                          #"bear", ###
                          #"capreotf", ###
                          #"hseal", ###
                          #"ibex",
                          #"porpoise", 
                          #, # additionally created
                          #"capreochiz_ltraj_noInfoL",
                          #"capreochiz_ltraj_withNA",
                          #"capreochiz_ltraj_withNA_noInfoL",
                          #"capreochiz_ltraj_RoeDeer_bursts_noInfoL",
                          #"capreochiz_ltraj_RoeDeer_bursts_withNA",
                          "puechabonsp_ltraj_withNA",
                          "puechabonsp_ltraj_cut_withNA")

#-------------------------------------------------------------------------------



#===============================================================================
#===============================================================================
#
#
# TESTING ...
#
#
#===============================================================================
#===============================================================================



#===============================================================================
#-------------------------------------------------------------------------------
#
# TESTING COERCION FROM LTRAJ TO TRACK
#
#-------------------------------------------------------------------------------
#===============================================================================

legalLtrajObjectsL <- ltrajObjectsList
legalLtrajObjNamesL <- ltrajObjNamesList


#===============================================================================
#
context("ltrajCoercion.R: COERCION LTRAJ TO TRACK - BASIC TESTS\n")
#
#===============================================================================

message(paste("##### COERCION TO TRACK: START of basic tests ... #####\n", sep=""))

for (i in 1:length(ltrajObjectsList)) {
  
  test_that(paste("# Basic # Test if object ", ltrajObjNamesList[[i]],
                  " is of class ltraj", sep=""), {
    
    message(paste("Test if object ", ltrajObjNamesList[[i]], 
                  " is of class ltraj:", sep=""))
    
    expect_that(ltrajObjectsList[[i]], is_a("ltraj"))
    
  })
  
  if (!attributes(ltrajObjectsList[[i]])$typeII || length(ltrajObjectsList[[i]]) > 1) {
    
    test_that(paste("# Basic # Test if object ", ltrajObjNamesList[[i]], " throws an error because it contains
              more than one burst or no temporal information", sep=""), {
      
      message(paste("Test if object ", ltrajObjNamesList[[i]], 
                    " throws an error because it contains ", length(ltrajObjectsList[[i]]),
                    " bursts or no temporal information:", sep=""))
      
      expect_that(t <- as(ltrajObjectsList[[i]], "Track"), throws_error("[not,nicht] TRUE"))
      
    })
    
    # liegt hier das Problem??? list everändert sich im rahmen der schleife!!!
    #legalLtrajObjectsL[[i]] <- NULL
    #legalLtrajObjNamesL[[i]] <- NULL
    # evtl besser defaultwert zuweisen und dann von hinten her löschen
    # oder neue liste formen !?!
    legalLtrajObjectsL[[i]] <- "toBeDeleted"
    legalLtrajObjNamesL[[i]] <- "toBeDeleted"
  
  } else {
    
    test_that(paste("# Basic # Test coercion of ltraj object ", ltrajObjNamesList[[i]], 
              " to Track object", sep=""), {
      
      message(paste("Test of coercion of ", ltrajObjNamesList[[i]], " (containing ",
                    length(ltrajObjectsList[[i]]), " bursts) to Track object.", sep=""))
      
      expect_that(t <- as(ltrajObjectsList[[i]], "Track"), is_a("Track"))
      
      
      # !!! @na.omit
      # delete !!! ???
      # check na.omit: first simple approach
      #if (ltrajObjNamesList[[i]] == "puechcirc_JE93") {
      if (ltrajObjNamesList[[i]] == "capreochiz_ltraj_withNA") {
        print(ltrajObjNamesList[[i]])
        
        st1 <- system.time(t1 <- as(ltrajObjectsList[[i]], "Track"))
        print(st1)
        st2.1 <- system.time(ltrajObj_noNA <- adehabitatLT::na.omit.ltraj(ltrajObjectsList[[i]]))
        print(st2.1)
        st2.2 <- system.time(t2 <- as(ltrajObj_noNA, "Track"))
        print(st2.2)
        # --> puechcirc_JE93: geringfügig schneller
        # --> capreochiz_ltraj_withNA: langsamer...!!!
        
        print("expect_that(t2, is_equivalent_to(t1))")
        expect_that(t2, is_equivalent_to(t1))
        #print("expect_that(t2, is_identical_to(t1))")
        #expect_that(t2, is_identical_to(t1)) # not identical due to row.names
      }
    })
        
  }

} # Finish for-loop

message(paste("##### COERCION TO TRACK: FINISH of basic tests ... #####\n", sep=""))



#===============================================================================
#
# COERCION TO TRACK
# CREATION OF TRACK OBJECTS FOR FURTHER TESTING
#
#===============================================================================

if (length(legalLtrajObjectsL) != length(legalLtrajObjNamesL)) 
  stop("Error: Length of ltraj objects list is not equal to length of ltraj names list.")

for (i in length(legalLtrajObjectsL):1) {
  if (legalLtrajObjectsL[[i]] == "toBeDeleted") {
    legalLtrajObjectsL[[i]] <- NULL
    legalLtrajObjNamesL[[i]] <- NULL
  }
}


# Create Track objects for the upcoming tests
trackObjList <- lapply(legalLtrajObjectsL, function(x) as(x, "Track"))
trackObjNamesList <- lapply(legalLtrajObjNamesL, function(x) paste("t_", x, sep=""))
for (i in 1:length(trackObjList)){
  assign(trackObjNamesList[[i]], trackObjList[[i]])
}



#===============================================================================
#
context("ltrajCoercion.R: COERCION LTRAJ TO TRACK - FURTHER TESTS\n")
#
#=============================================================================== 

if (length(legalLtrajObjectsL) != length(trackObjList)) 
  stop("Error: Length of ltraj objects list is not equal to length of Track objects list.")

for (i in 1:length(trackObjList)) {
  
  message(paste("### Coercion of object ", legalLtrajObjNamesL[[i]], " to Track. Further tests:", sep=""))

  # Some data used in tests ...
  ltr <- legalLtrajObjectsL[[i]]
  #ltr <- puechcirc_JE93
  ltrdf <- ltr[[1]]
  ltrdf_nrow <- nrow(ltrdf)
  ltrdf_legalRows <- which(!is.na(ltrdf$x) & !is.na(ltrdf$y) & !is.na(ltrdf$date))
  legal_nrow <- length(ltrdf_legalRows)
  legal_rownames <- attr(ltrdf, "row.names")[ltrdf_legalRows]
  ltrdf_connData <- subset(ltrdf, select = -c(x, y, date, R2n))
  ltrdf_connData_colnames <- colnames(ltrdf_connData)
  
  t <- trackObjList[[i]]
  #t <- t_puechcirc_JE93
  #t <- as(ltr, "Track")
    
  # If NA values in ltraj ...
  if (ltrdf_nrow != legal_nrow) {
    
    # Step characteristics need to be recalculated because of deleted rows containing
    # with NA values in coords or time.
    ltr_new <- adehabitatLT::dl(ltrdf[ltrdf_legalRows, ])
    ltrdf_new <- ltr_new[[1]]
    attr(ltrdf_new, "row.names") <- legal_rownames
    ltrdf_new_connData <- subset(ltrdf_new, select = -c(x, y, date, R2n))
    ltrdf_new_connData_colnames <- colnames(ltrdf_new_connData)
    
    
    test_that(paste("ltraj object with NA values (", legalLtrajObjNamesL[[i]], 
                    ") to Track - further tests", sep = ""), {
      
      # length / nrow
      expect_that(legal_nrow, is_identical_to(nrow(t@data)))
      expect_that(legal_nrow - 1, equals(nrow(t@connections)))
      expect_that(nrow(ltrdf_new), is_identical_to(nrow(t@data)))
      
      # coords
      expect_that(ltrdf$x[ltrdf_legalRows], is_equivalent_to(t@sp@coords[,1]))
      expect_that(ltrdf$y[ltrdf_legalRows], is_equivalent_to(t@sp@coords[,2]))
      
      # time
      expect_that(ltrdf$date[ltrdf_legalRows], is_equivalent_to(zoo::index(t@time)))
      # --> not identical due to attributes
      myTime <- zoo::index(t@time)
      attr(myTime, "tclass") <- NULL
      attr(myTime, "tzone") <- NULL
      expect_that(myTime, equals(ltrdf$date[ltrdf_legalRows]))
      
      # column R2n (need to be in data slot of track object)
      expect_that(ltrdf_new$R2n, is_identical_to(t@data$R2n))
      
      # connections slot data
      expect_that(ltrdf_new_connData[-legal_nrow, ], 
                  is_identical_to(t@connections[ , ltrdf_new_connData_colnames]))
            
      # rownames
      expect_that(attributes(ltrdf)$row.names[ltrdf_legalRows], 
                  is_identical_to(attr(t@data, "row.names")))
      expect_that(attributes(ltrdf)$row.names[head(ltrdf_legalRows, -1L)], 
                  is_identical_to(attr(t@connections, "row.names")))
      
      # infolocs
      if (!is.null(attributes(ltrdf)$infolocs)) {
        
        # length /nrow and infolocs
        expect_that(nrow(attributes(ltrdf)$infolocs[ltrdf_legalRows, , drop = FALSE]), 
                    is_identical_to(nrow(t@data)))
        expect_that(nrow(attributes(ltrdf)$infolocs[ltrdf_legalRows, , drop = FALSE]), 
                    is_identical_to(nrow(t@data)))
        expect_that(length(attributes(ltrdf)$infolocs) + length(ltrdf), 
                    equals(length(t@data) + length(t@connections) - 1))
        
        # rownames and infolocs
        expect_that(row.names(attributes(ltrdf)$infolocs[ltrdf_legalRows,]), 
                    is_identical_to(row.names(t@data))) 
        
        infolocs_colnames <- colnames(attributes(ltrdf)$infolocs)
                
        # infolocs data 
        # Equivalent but not always identical, because the row.names of infolocs are  
        # always of type character but the coercion is done with respect to the ltraj
        # attribute row.names which also might be of type integer.
        expect_that(attributes(ltrdf)$infolocs[ltrdf_legalRows, , drop = FALSE], 
                    is_equivalent_to(t@data[infolocs_colnames]))
        tmpData <- t@data[infolocs_colnames]
        attr(tmpData, "row.names") <- row.names(t@data[infolocs_colnames])
        expect_that(tmpData, 
                    is_identical_to(
                      attributes(ltrdf)$infolocs[ltrdf_legalRows, , drop = FALSE]))
        
      } else {
        expect_that(length(ltrdf), 
                    equals(length(t@data) + length(t@connections) - 1))
      }
      
    })
    
  } else {
    
    test_that(paste("ltraj object without NA values (", legalLtrajObjNamesL[[i]],
              ") to Track - further tests", sep = ""), {
      
      # length
      expect_that(ltrdf_nrow, is_identical_to(nrow(t@data)))
      expect_that(ltrdf_nrow - 1, equals(nrow(t@connections)))
      
      # coords
      expect_that(ltrdf$x, is_equivalent_to(t@sp@coords[,1]))
      expect_that(ltrdf$y, is_equivalent_to(t@sp@coords[,2]))
      
      # time
      expect_that(ltrdf$date, is_equivalent_to(zoo::index(t@time)))
      # --> not identical due to attributes
      myTime <- zoo::index(t@time)
      attr(myTime, "tclass") <- NULL
      attr(myTime, "tzone") <- NULL
      expect_that(myTime, equals(ltrdf$date))
      
      # column R2n (in data)
      expect_that(ltrdf$R2n, is_identical_to(t@data$R2n))
      
      # connections slot data
      ## ltrdf_connData <- subset(ltrdf, select = -c(x, y, date, R2n))
      ## ltrdf_connData_colnames
      expect_that(ltrdf_connData[-ltrdf_nrow, , drop = FALSE], 
                  is_identical_to(t@connections[ltrdf_connData_colnames]))
      
      # rownames
      expect_that(attributes(ltrdf)$row.names,
                  is_identical_to(attr(t@data, "row.names"))) 
      expect_that(head(attributes(ltrdf)$row.names, -1L), 
                  is_identical_to(attr(t@connections, "row.names")))      
      
      # infolocs
      if (!is.null(attributes(ltrdf)$infolocs)) {
        
        # length /nrow and infolocs
        #expect_that(nrow(data.frame(attributes(ltrdf)$infolocs)), 
        #            is_identical_to(nrow(t@data)))
        expect_that(nrow(attributes(ltrdf)$infolocs), is_identical_to(nrow(t@data)))
        expect_that(length(attributes(ltrdf)$infolocs) + length(ltrdf), 
                    equals(length(t@data) + length(t@connections) - 4 + 3))
        
        # rownames and infolocs
        expect_that(row.names(attributes(ltrdf)$infolocs), 
                    is_identical_to(row.names(t@data))) 
        
        infolocs_colnames <- colnames(attributes(ltrdf)$infolocs)
        
        # infolocs data 
        # Equivalent but not always identical, because the row.names of infolocs are  
        # always of type character but the coercion is done with respect to the ltraj
        # attribute row.names which also might be of type integer.
        expect_that(attributes(ltrdf)$infolocs, 
                    is_equivalent_to(t@data[infolocs_colnames]))
        tmpData <- t@data[infolocs_colnames]
        attr(tmpData, "row.names") <- row.names(t@data[infolocs_colnames])
        expect_that(tmpData, 
                    is_identical_to(attributes(ltrdf)$infolocs))
        
      } else {
        expect_that(length(ltrdf), 
                    equals(length(t@data) + length(t@connections) - 4 + 3))
      }
      
    })
  }
}



#===============================================================================
#-------------------------------------------------------------------------------
#
# TESTING COERCION FROM LTRAJ TO TRACKS
#
#-------------------------------------------------------------------------------
#===============================================================================

legalLtrajObjectsL <- ltrajObjectsList
legalLtrajObjNamesL <- ltrajObjNamesList

    
#===============================================================================
#
context("ltrajCoercion.R: COERCION LTRAJ TO TRACKS - BASIC TESTS\n")
#
#===============================================================================

message(paste("##### COERCION TO TRACKS: START of basic tests ... #####\n", sep=""))

for (i in 1:length(ltrajObjectsList)) {
  
  test_that(paste("# Basic # Test if object ", ltrajObjNamesList[[i]],
                  " is of class ltraj", sep=""), {
                    
                    message(paste("Test if object ", ltrajObjNamesList[[i]], 
                                  " is of class ltraj:", sep=""))
                    
                    expect_that(ltrajObjectsList[[i]], is_a("ltraj"))
                    
                  })
  

  idVec <- sapply(ltrajObjectsList[[i]], function(x) attributes(x)$id)

  if (!attributes(ltrajObjectsList[[i]])$typeII || length(ltrajObjectsList[[i]]) < 2
      || length(unique(idVec)) > 1) {
    
    
    test_that(paste("# Basic # Test if object ", ltrajObjNamesList[[i]], " throws an error because it contains
                    less than to trajectories, more than one id or no temporal information", sep=""), 
              {
                      message(paste("Test if object ", ltrajObjNamesList[[i]], 
                                    " throws an error because it contains less than to ",
                                    "trajectories, more than one id or no temporal information:",
                                    sep=""))
                      
                      expect_that(t <- as(ltrajObjectsList[[i]], "Tracks"), throws_error("[not,nicht] TRUE"))
              })
    
    legalLtrajObjectsL[[i]] <- "toBeDeleted"
    legalLtrajObjNamesL[[i]] <- "toBeDeleted"

  } else {
    
    test_that(paste("# Basic # Test coercion of ltraj object ", ltrajObjNamesList[[i]], 
                    " to Tracks object", sep=""), {
                      
                      message(paste("Test of coercion of ", ltrajObjNamesList[[i]], " (containing ",
                                    length(ltrajObjectsList[[i]]), " bursts) to Tracks object.", sep=""))
                      
                      expect_that(t <- as(ltrajObjectsList[[i]], "Tracks"), is_a("Tracks"))
                      
                    })  
  }
} # Finish for-loop

message(paste("##### COERCION TO TRACKS: FINISH of basic tests ... #####\n", sep=""))



#===============================================================================
#
# COERCION TO TRACKS
# CREATION OF TRACKS OBJECTS FOR FURTHER TESTING
#
#===============================================================================

if (length(legalLtrajObjectsL) != length(legalLtrajObjNamesL)) 
  stop("Error: Length of ltraj objects list is not equal to length of ltraj names list.")


#Achtung: verändert
for (i in length(legalLtrajObjNamesL):1) {
  if (legalLtrajObjNamesL[[i]] == "toBeDeleted") {
    legalLtrajObjectsL[[i]] <- NULL
    legalLtrajObjNamesL[[i]] <- NULL
  }
}



# Create Tracks objects for the upcoming tests
tracksObjList <- lapply(legalLtrajObjectsL, function(x) as(x, "Tracks"))
tracksObjNamesList <- lapply(legalLtrajObjNamesL, function(x) paste("t_", x, sep=""))
for (i in 1:length(tracksObjList)){
  assign(tracksObjNamesList[[i]], tracksObjList[[i]])
}



#===============================================================================
#
context("ltrajCoercion.R: COERCION LTRAJ TO TRACKS - FURTHER TESTS\n")
#
#=============================================================================== 

if (length(legalLtrajObjectsL) != length(tracksObjList)) 
  stop("Error: Length of ltraj objects list is not equal to length of Tracks objects list.")

for (i in 1:length(tracksObjList)) {
  
  message(paste("### Coercion of object ", legalLtrajObjNamesL[[i]], 
                " to Tracks. Further tests:", sep=""))
  
  ltr <- legalLtrajObjectsL[[i]]
  #ltr <- puechcirc_CH93; length(ltr); class(ltr); str(ltr) 
  #ltr <-  capreochiz_ltraj_RoeDeer_bursts_withNA
  ltrdf_list <- lapply(ltr, function(x) x)
  #identical(ltr, ltrdf_list); str(ltrdf_list)
  
  idVec <- sapply(ltr, function(x) attributes(x)$id)
  burstVec <- sapply(ltr, function(x) attributes(x)$burst)
  
  ltrdf_nrow_all <- sum(sapply(ltrdf_list, function(x) nrow(x)))
  ltrdf_list_nrow <- lapply(ltrdf_list, function(x) nrow(x))
  
  ltrdf_list_legalRows <- lapply(ltrdf_list, function(z) { 
    which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)) })
  legal_nrow_list <- lapply(ltrdf_list_legalRows, function(x) length(x))
  legal_nrow_all <- sum(sapply(ltrdf_list_legalRows, function(x) length(x)))
  legal_rownames_list <- lapply(ltrdf_list, function(z) {
    attr(z, "row.names")[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date))] })
    
  
  #legal_rn <- attributes(ltrdf)$row.names[ltrdf_legalRows] # bei cap numeric
  ltrdf_list_connData <- lapply(ltrdf_list, function(z) { 
    subset(z, select = -c(x, y, date, R2n)) })
  ltrdf_list_connData_colnames <- lapply(ltrdf_list_connData, function(x) colnames(x))
  
  #?
  ltrdf_connData_colnames <- colnames(ltrdf_list_connData[[1]])
  
  trcs <- tracksObjList[[i]]
  # trcs <- mytrcs <- as(ltr, "Tracks")
  
  # If NA values in ltraj ...
  if (ltrdf_nrow_all != legal_nrow_all) {
    
    # Step characteristics need to be recalculated because of deleted rows containing
    # with NA values in coords or time.
    
    recalc_ltrdf <- function(ltrdf) {
      legalRI <- !is.na(ltrdf$x) & !is.na(ltrdf$y) & !is.na(ltrdf$date)
      id <- attr(ltrdf, "id")
      burst <- attr(ltrdf, "burst")
      if (FALSE %in% legalRI) {
        legalRws <- which(!is.na(ltrdf$x) & !is.na(ltrdf$y) & !is.na(ltrdf$date))
        legalRwsNames <- attr(ltrdf, "row.names")[legalRws]
        ltr_new <- adehabitatLT::dl(ltrdf[legalRws, ])
        ltrdf_new <- ltr_new[[1]]
        attr(ltrdf_new, "row.names") <- legalRwsNames
        attr(ltrdf_new, "id") <- id
        attr(ltrdf_new, "burst") <- burst
        return(ltrdf_new)
      } else {
        return(ltrdf)
      }
    }
    
    ltrdf_new_list <- lapply(ltrdf_list,  function(x) recalc_ltrdf(x))
    
    ltrdf_new_list_legalRows <- lapply(ltrdf_new_list, function(z) { 
      which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)) })
    legal_new_nrow_list <- lapply(ltrdf_new_list_legalRows, function(x) length(x))
    
    ltrdf_new_connData_list <- lapply(ltrdf_new_list, function(z) { 
      subset(z, select = -c(x, y, date, R2n)) })
     
    ltrdf_new_connData_colnames <- colnames(ltrdf_new_connData_list[[1]])
    
    
    test_that("ltraj object with NA values to Tracks - further tests", {
      
      # length / nrow
      #trcs <- mytrcs
      expect_that(legal_nrow_all, is_identical_to(
        sum(sapply(trcs@tracks, function(x) nrow(x@data)))))
      expect_that(legal_nrow_list, is_equivalent_to(
        lapply(trcs@tracks, function(x) nrow(x@data))))
      expect_that(lapply(legal_nrow_list, function(x) x - 1), is_equivalent_to(
        lapply(trcs@tracks, function(x) nrow(x@connections))))
      # --> just equivalent because of names
      
      # names
      expect_that(burstVec, is_identical_to(names(trcs@tracks)))
      
      # coords
      expect_that(lapply(ltrdf_list, function(z) {
        z$x[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date))] }), is_equivalent_to(
          lapply(trcs@tracks, function(x) x@sp@coords[,1])))
      expect_that(lapply(ltrdf_list, function(z) {
        z$y[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date))] }), is_equivalent_to(
          lapply(trcs@tracks, function(x) x@sp@coords[,2])))
      # --> just equivalent because of names
      
      # time
      expect_that(lapply(ltrdf_list, function(z) { 
        z$date[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date))] }), is_equivalent_to(
          lapply(trcs@tracks, function(x) zoo::index(x@time))))

      # rownames
      expect_that(lapply(ltrdf_list, function(z) {
        attributes(z)$row.names[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date))] }),
        is_equivalent_to(lapply(trcs@tracks, function(x)  attr(x@data, "row.names"))))
      expect_that(lapply(ltrdf_list, function(z) {
        attributes(z)$row.names[head(which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), -1L)] }),
        is_equivalent_to(lapply(trcs@tracks, function(x)  attr(x@connections, "row.names"))))
      # --> just equivalent because of names
      # e.g. Roe.Deer.1 in $ Roe.Deer.1: int [1:36] 2 4 9 10 11 14 16 17 18 19 ...
      
      #expect_that(lapply(ltrdf_list, function(z) {
      #  attributes(z)$row.names[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date))] }),
      #  is_identical_to(lapply(trcs@tracks, function(x)  attr(x@data, "row.names"))))
      #str((lapply(ltrdf_list, function(z) {
      #  attributes(z)$row.names[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date))] })))
      #str(lapply(trcs@tracks, function(x)  attr(x@data, "row.names")))
      
      
      
      # infolocs
      # ?reicht folgendes ???
      # adehabitatLT manual: "... all the variables are measured for all bursts and ids"
      if (!is.null(attributes(ltrdf_list[[1]])$infolocs)) {   
        
        # length (number of attributes) / nrow and infolocs
        expect_that(lapply(ltrdf_list, function(z) { 
          nrow(attributes(z)$infolocs[
            which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), , drop = FALSE]) }),
          is_equivalent_to(lapply(trcs@tracks, function(x) nrow(x@data))))
        expect_that(lapply(ltrdf_list, function(z) { 
          nrow(attributes(z)$infolocs[
            which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), , drop = FALSE]) - 1 }),
          is_equivalent_to(lapply(trcs@tracks, function(x) nrow(x@connections))))
        expect_that(lapply(ltrdf_list, function(z) { 
          length(attributes(z)$infolocs) + length(z) }),
          is_equivalent_to(lapply(trcs@tracks, function(x) {
            length(x@data) + length(x@connections) - 1 } )))
  
        # rownames and infolocs
        expect_that(lapply(ltrdf_list, function(z) {
          row.names(attributes(z)$infolocs[
            which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), , drop = FALSE]) 
          }), is_equivalent_to(
            lapply(trcs@tracks, function(x)  as.character(attr(x@data, "row.names")))))
        # Differences: names for target but not for current
        
        #expect_that(lapply(ltrdf_list, function(z) {
        #  row.names(attributes(z)$infolocs[
        #    which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), , drop = FALSE]) 
        #}), is_identical_to(
        #  lapply(trcs@tracks, function(x)  as.character(attr(x@data, "row.names")))))
        
        
        # infolocs data 
        # Equivalent but not always identical, because the row.names of infolocs are  
        # always of type character but the coercion is done with respect to the ltraj
        # attribute row.names which also might be of type integer.
        infolocs_colnames <- colnames(attributes(ltrdf_list[[1]])$infolocs)
        expect_that(lapply(ltrdf_list, function(z) {
          attributes(z)$infolocs[
            which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), , drop = FALSE] }),
          is_equivalent_to(lapply(trcs@tracks, function(x) x@data[infolocs_colnames])))
        trcsDataList <- lapply(trcs@tracks, function(x) x@data[infolocs_colnames])
        names(trcsDataList) <- NULL
        for (i in seq_along(trcsDataList)) { 
          attr(trcsDataList[[i]], "row.names") <- row.names(trcsDataList[[i]]) 
        }
        expect_that(trcsDataList, is_identical_to(lapply(ltrdf_list, function(z) {
            attributes(z)$infolocs[
              which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), , drop = FALSE] })))
        
      } else { # no infolocs
        
        # length (number of attributes)
        expect_that(lapply(ltrdf_list, function(x) length(x)), is_equivalent_to(
          lapply(trcs@tracks, function(x) { length(x@data) + length(x@connections) - 1 })))
                    
      }

      # column R2n (need to be in data slot of track object)
      expect_that(lapply(ltrdf_new_list, function(x) x$R2n), 
                  is_equivalent_to(lapply(trcs@tracks, function(x) x@data$R2n)))
      
      # connections slot data
      #z <- ltrdf_new_connData_list[[1]]
      expect_that(lapply(ltrdf_new_connData_list, function(z) { 
        z[-nrow(z), ] }), is_equivalent_to(
          lapply(trcs@tracks, function(x) x@connections[ltrdf_new_connData_colnames])))
      
            
    })

  } else { # no NA values in ltraj coords and time
    
    test_that("ltraj object without NA values to Tracks - further tests", {
      
      # length / nrow
      expect_that(legal_nrow_all, is_identical_to(
        sum(sapply(trcs@tracks, function(x) nrow(x@data)))))
      expect_that(legal_nrow_list, is_equivalent_to(
        lapply(trcs@tracks, function(x) nrow(x@data))))
      expect_that(lapply(legal_nrow_list, function(x) x - 1), is_equivalent_to(
        lapply(trcs@tracks, function(x) nrow(x@connections))))
      
      # names
      expect_that(burstVec, is_identical_to(names(trcs@tracks)))
      
      # coords
      expect_that(lapply(ltrdf_list, function(z) z$x), is_equivalent_to(
          lapply(trcs@tracks, function(x) x@sp@coords[,1])))  
      expect_that(lapply(ltrdf_list, function(z) z$y), is_equivalent_to(
          lapply(trcs@tracks, function(x) x@sp@coords[,2])))      
      
      # time
      expect_that(lapply(ltrdf_list, function(z) z$date), is_equivalent_to(
          lapply(trcs@tracks, function(x) zoo::index(x@time))))
      
      # rownames
      expect_that(lapply(ltrdf_list, function(z) attributes(z)$row.names),
        is_equivalent_to(lapply(trcs@tracks, function(x)  attr(x@data, "row.names"))))
      expect_that(lapply(ltrdf_list, function(z) { head(attributes(z)$row.names, -1L) }),
        is_equivalent_to(lapply(trcs@tracks, function(x)  attr(x@connections, "row.names"))))
      
      # infolocs
      # ?reicht folgendes ???
      # adehabitatLT manual: "... all the variables are measured for all bursts and ids"
      if (!is.null(attributes(ltrdf_list[[1]])$infolocs)) {   
        
        # length (number of attributes) / nrow and infolocs
        expect_that(lapply(ltrdf_list, function(z) {
          nrow(attributes(z)$infolocs) }),
          is_equivalent_to(lapply(trcs@tracks, function(x) nrow(x@data))))
        expect_that(lapply(ltrdf_list, function(z) { 
          nrow(attributes(z)$infolocs) - 1 }),
          is_equivalent_to(lapply(trcs@tracks, function(x) nrow(x@connections))))
        expect_that(lapply(ltrdf_list, function(z) { 
          length(attributes(z)$infolocs) + length(z) }),
          is_equivalent_to(lapply(trcs@tracks, function(x) {
            length(x@data) + length(x@connections) - 1 } )))
        
        # rownames and infolocs
        expect_that(lapply(ltrdf_list, function(z) {
          row.names(attributes(z)$infolocs) }), is_equivalent_to(
            lapply(trcs@tracks, function(x)  as.character(attr(x@data, "row.names")))))
        
        # infolocs data 
        # Equivalent but not always identical, because the row.names of infolocs are  
        # always of type character but the coercion is done with respect to the ltraj
        # attribute row.names which also might be of type integer.
        infolocs_colnames <- colnames(attributes(ltrdf_list[[1]])$infolocs)
        # !!! error!!! für puechabonsp_ltraj_Chou_bursts
        #str(lapply(ltrdf_list, function(z) { attributes(z)$infolocs })) # 119 levels !?!?
        #str(lapply(trcs@tracks, function(x) x@data[ , infolocs_colnames])) 
        # --> Achtung, kein data.frame (mehr)!!! 119 levels !?!?
        # data.frame(...) added !!! ??? !!!
        expect_that(lapply(ltrdf_list, function(z) { attributes(z)$infolocs }),
          is_equivalent_to(
            lapply(trcs@tracks, function(x) data.frame(x@data[infolocs_colnames]))))
        trcsDataList <- lapply(trcs@tracks, function(x) data.frame(x@data[infolocs_colnames]))
        names(trcsDataList) <- NULL
        for (i in seq_along(trcsDataList)) { 
          attr(trcsDataList[[i]], "row.names") <- row.names(trcsDataList[[i]]) 
        }
        expect_that(trcsDataList, is_identical_to(
                      lapply(ltrdf_list, function(z) { attributes(z)$infolocs })))
        
      } else { # no infolocs
        
        # length (number of attributes)
        expect_that(lapply(ltrdf_list, function(x) length(x)), is_equivalent_to(
          lapply(trcs@tracks, function(x) { length(x@data) + length(x@connections) - 1 })))
        
      }

      # column R2n (need to be in data slot of track object)
      expect_that(lapply(ltrdf_list, function(x) x$R2n), 
                  is_equivalent_to(lapply(trcs@tracks, function(x) x@data$R2n)))
      
      # connections slot data
      expect_that(lapply(ltrdf_list_connData, function(x) x[-nrow(x), , drop = FALSE]), 
                  is_equivalent_to(
                    lapply(trcs@tracks, function(x) x@connections[ltrdf_connData_colnames])))
            
    })
  }
}
  


#===============================================================================
#-------------------------------------------------------------------------------
#
# TESTING COERCION FROM LTRAJ TO TRACKSCOLLECTION
#
#-------------------------------------------------------------------------------
#===============================================================================

legalLtrajObjectsL <- ltrajObjectsList
legalLtrajObjNamesL <- ltrajObjNamesList



#===============================================================================
#
context("ltrajCoercion.R: 3. COERCION LTRAJ TO TRACKSCOLLECTION - BASIC TESTS\n")
#
#===============================================================================

message(paste("##### COERCION TO TRACKSCOLLECTION: START of basic tests ... #####\n",
              sep=""))

for (i in 1:length(ltrajObjectsList)) {
  
  test_that(paste("# Basic # Test if object ", ltrajObjNamesList[[i]],
                  " is of class ltraj", sep=""), {
                    
                    message(paste("Test if object ", ltrajObjNamesList[[i]], 
                                  " is of class ltraj:", sep=""))
                    
                    expect_that(ltrajObjectsList[[i]], is_a("ltraj"))
                    
                  })
  
  
  idVec <- sapply(ltrajObjectsList[[i]], function(x) attributes(x)$id)
  
  if (!attributes(ltrajObjectsList[[i]])$typeII || length(ltrajObjectsList[[i]]) < 2
      || length(unique(idVec)) < 2) {
    
    
    test_that(paste("# Basic # Test if object ", ltrajObjNamesList[[i]], " throws an error because it contains
                    less than to trajectories, less than two different ids or no temporal information", 
                    sep=""),
              {
                
                message(paste("Test if object ", ltrajObjNamesList[[i]], 
                              " throws an error because it contains less than two ",
                              "trajectories, less than two different ids or no temporal information:",
                              sep=""))
                
                expect_that(t <- as(ltrajObjectsList[[i]], "TracksCollection"), throws_error("[not,nicht] TRUE"))
              })
    
    legalLtrajObjectsL[[i]] <- "toBeDeleted"
    legalLtrajObjNamesL[[i]] <- "toBeDeleted"

  } else {
    
    test_that(paste("# Basic # Test coercion of ltraj object ", ltrajObjNamesList[[i]], 
                    " to TracksCollection object", sep=""), 
              {
                      
                message(paste("Test of coercion from ", ltrajObjNamesList[[i]], " (containing ",
                              length(ltrajObjectsList[[i]]), " bursts) to TracksCollection.", sep=""))
                
                expect_that(t <- as(ltrajObjectsList[[i]], "TracksCollection"), 
                            is_a("TracksCollection"))
           
              })  
  }
} # Finish for-loop

message(paste("##### COERCION TO TRACKSCOLLECTION: FINISH of basic tests ... #####\n", sep=""))



#===============================================================================
#
# COERCION TO TRACKSCOLLECTION
# CREATION OF TRACKSCOLLECTION OBJECTS FOR FURTHER TESTING
#
#===============================================================================

if (length(legalLtrajObjectsL) != length(legalLtrajObjNamesL)) 
  stop("Error: Length of ltraj objects list is not equal to length of ltraj names list.")


#Achtung: verändert
for (i in length(legalLtrajObjNamesL):1) {
  if (legalLtrajObjNamesL[[i]] == "toBeDeleted") {
    legalLtrajObjectsL[[i]] <- NULL
    legalLtrajObjNamesL[[i]] <- NULL
  }
}



# Create TracksCollection objects for the upcoming tests
tracksCollObjList <- lapply(legalLtrajObjectsL, function(x) as(x, "TracksCollection"))
tracksCollObjNamesList <- lapply(legalLtrajObjNamesL, function(x) paste("t_", x, sep=""))
for (i in 1:length(tracksCollObjList)){
  assign(tracksCollObjNamesList[[i]], tracksCollObjList[[i]])
}



#===============================================================================
#
context("ltrajCoercion.R: COERCION LTRAJ TO TRACKSCOLLECTION - FURTHER TESTS\n")
#
#=============================================================================== 

if (length(legalLtrajObjectsL) != length(tracksCollObjList)) 
  stop("Error: Length of ltraj objects list is not equal to length of TracksCollection objects list.")

for (i in 1:length(tracksCollObjList)) {
  
  message(paste("### Coercion of object ", legalLtrajObjNamesL[[i]], 
                " to TracksCollection. Further tests:", sep=""))
  
  ltr <- legalLtrajObjectsL[[i]]
  #ltr <- puechcirc # with NA, no infolocs
  
  ltrdf_list <- lapply(ltr, function(x) x)
  #identical(ltr, ltrdf_list); str(ltrdf_list)
  
  idVec <- sapply(ltr, function(x) attributes(x)$id)
  burstVec <- sapply(ltr, function(x) attributes(x)$burst)
  
  ltrdf_nrow_all <- sum(sapply(ltrdf_list, function(x) nrow(x)))
  ltrdf_list_nrow <- lapply(ltrdf_list, function(x) nrow(x))
  
  ltrdf_list_legalRows <- lapply(ltrdf_list, function(z) { 
    which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)) })
  legal_nrow_list <- lapply(ltrdf_list_legalRows, function(x) length(x))
  legal_nrow_all <- sum(sapply(ltrdf_list_legalRows, function(x) length(x)))
  legal_rownames_list <- lapply(ltrdf_list, function(z) {
    attr(z, "row.names")[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date))] })
  
  ltrdf_list_connData <- lapply(ltrdf_list, function(z) { 
    subset(z, select = -c(x, y, date, R2n)) })
  ltrdf_list_connData_colnames <- lapply(ltrdf_list_connData, function(x) colnames(x))
  
  #?
  ltrdf_connData_colnames <- colnames(ltrdf_list_connData[[1]])
  
  # !!!
  trcsColl <- tracksCollObjList[[i]]
  # trcsColl <- mytrcs <- as(ltr, "TracksCollection")
  #str(trcsColl)
  
  # If NA values in ltraj ...
  if (ltrdf_nrow_all != legal_nrow_all) {
    
    # Step characteristics need to be recalculated because of deleted rows containing
    # with NA values in coords or time.
    
    # ltrdf <- ltr[[1]]
    recalc_ltrdf <- function(ltrdf) {
      legalRI <- !is.na(ltrdf$x) & !is.na(ltrdf$y) & !is.na(ltrdf$date)
      id <- attr(ltrdf, "id")
      burst <- attr(ltrdf, "burst")
      if (FALSE %in% legalRI) {
        legalRws <- which(!is.na(ltrdf$x) & !is.na(ltrdf$y) & !is.na(ltrdf$date))
        legalRwsNames <- attr(ltrdf, "row.names")[legalRws]
        ltr_new <- adehabitatLT::dl(ltrdf[legalRws, ])
        ltrdf_new <- ltr_new[[1]]
        if (!is.null(attributes(ltrdf)$infolocs)) {
          attributes(ltrdf_new)$infolocs <- attributes(ltrdf)$infolocs[legalRws,]
          attr(attributes(ltrdf_new)$infolocs, "row.names") <- as.character(legalRwsNames)
        }
        attr(ltrdf_new, "row.names") <- legalRwsNames
        attr(ltrdf_new, "id") <- id
        attr(ltrdf_new, "burst") <- burst
        return(ltrdf_new)
      } else {
        return(ltrdf)
      }
    }
    #str(ltrdf_new)
    ltrdf_new_list <- lapply(ltrdf_list,  function(x) recalc_ltrdf(x))
    #str(ltrdf_new_list)
    
    ltrdf_new_list_legalRows <- lapply(ltrdf_new_list, function(z) { 
      which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)) })
    legal_new_nrow_list <- lapply(ltrdf_new_list_legalRows, function(x) length(x))
    
    ltrdf_new_connData_list <- lapply(ltrdf_new_list, function(z) { 
      subset(z, select = -c(x, y, date, R2n)) })
    
    ltrdf_new_connData_colnames <- colnames(ltrdf_new_connData_list[[1]])
    
    
    test_that("ltraj object with NA values to TracksCollection - further tests", {
      
      # length / nrow
      expect_that(legal_nrow_all, is_identical_to(
        sum(unlist(lapply(trcsColl@tracksCollection, function(X) {
          lapply(X@tracks, function(x) nrow(x@data)) })))))
      expect_that(unlist(lapply(legal_nrow_list, function(x) x - 1)), is_equivalent_to(
        unlist(lapply(trcsColl@tracksCollection, function(X) {
          lapply(X@tracks, function(x) nrow(x@connections)) }))))
      
      # names
      expect_that(unique(idVec), is_identical_to(names(trcsColl@tracksCollection)))
      # Just equivalent due to names in TracksCollection
      expect_that(burstVec, is_equivalent_to(unlist(lapply(trcsColl@tracksCollection, function(X) 
        { names(X@tracks) }))))

      # coords
      expect_that(unlist(lapply(ltrdf_list, function(z) {
        z$x[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date))] })), is_equivalent_to(
          unlist(lapply(trcsColl@tracksCollection, function(X) {
            lapply(X@tracks, function(x) x@sp@coords[,1]) }))))
      expect_that(unlist(lapply(ltrdf_list, function(z) {
        z$y[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date))] })), is_equivalent_to(
          unlist(lapply(trcsColl@tracksCollection, function(X) {
            lapply(X@tracks, function(x) x@sp@coords[,2]) }))))
      
      # time
      expect_that(unlist(lapply(ltrdf_list, function(z) { 
        z$date[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date))] })), is_equivalent_to(
          unlist(lapply(trcsColl@tracksCollection, function(X) {
            lapply(X@tracks, function(x) zoo::index(x@time)) }))))

      # rownames
      expect_that(unlist(lapply(ltrdf_list, function(z) {
        attributes(z)$row.names[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date))] })),
        is_equivalent_to(unlist(lapply(trcsColl@tracksCollection, function(X) {
          lapply(X@tracks, function(x) attr(x@data, "row.names")) }))))
      expect_that(unlist(lapply(ltrdf_list, function(z) {
        attributes(z)$row.names[head(which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), -1L)] })),
        is_equivalent_to(unlist(lapply(trcsColl@tracksCollection, function(X) {
          lapply(X@tracks, function(x) attr(x@connections, "row.names")) }))))
      
      # infolocs
      # ?reicht folgendes ???
      # adehabitatLT manual: "... all the variables are measured for all bursts and ids"
      if (!is.null(attributes(ltrdf_list[[1]])$infolocs)) {   
        
        # length (number of attributes) / nrow and infolocs
        expect_that(unlist(lapply(ltrdf_list, function(z) { 
            nrow(attributes(z)$infolocs[
              which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), , drop = FALSE]) })),
          is_equivalent_to(unlist(lapply(trcsColl@tracksCollection, function(X) {
            lapply(X@tracks, function(x) nrow(x@data)) }))))
        expect_that(unlist(lapply(ltrdf_list, function(z) { 
          nrow(attributes(z)$infolocs[
            which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), , drop = FALSE]) - 1 })),
          is_equivalent_to(unlist(lapply(trcsColl@tracksCollection, function(X) {
            lapply(X@tracks, function(x) nrow(x@connections)) }))))
        expect_that(unlist(lapply(ltrdf_list, function(z) { 
          length(attributes(z)$infolocs) + length(z) })), is_equivalent_to(
            unlist(lapply(trcsColl@tracksCollection, function(X) {
              lapply(X@tracks, function(x) { length(x@data) + length(x@connections) - 1 }) }))))
        
        
        # rownames and infolocs
        #
        ## hier gibt probleme: row.names infolocs je Track durchlaufend von 1 an...
        #expect_that(unlist(lapply(ltrdf_list, function(z) {
        #  row.names(data.frame(attributes(z)$infolocs[
        #    which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), ])) })),
        # is_equivalent_to(
        ## row.names von trColl ok
        #   unlist(lapply(trcsColl@tracksCollection, function(X) {
        #     lapply(X@tracks, function(x) as.character(attr(x@data, "row.names"))) }))
        #   ))
        #
        # vllt gehts mit ltrdf_new_list:
        #attr(attributes(ltrdf_new_list[[1]])$infolocs, "row.names")
        expect_that(unlist(lapply(ltrdf_new_list, function(z) {
          attr(attributes(z)$infolocs, "row.names") })), is_equivalent_to(
           unlist(lapply(trcsColl@tracksCollection, function(X) {
             lapply(X@tracks, function(x) as.character(attr(x@data, "row.names"))) }))))
        
        # infolocs data 
        # Equivalent but not always identical, because the row.names of infolocs are  
        # always of type character but the coercion is done with respect to the ltraj
        # attribute row.names which also might be of type integer.
        infolocs_colnames <- colnames(attributes(ltrdf_list[[1]])$infolocs)
        #expect_that(sapply(ltrdf_list, function(z) {
        #  attributes(z)$infolocs[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), ] }),
        #  is_equivalent_to(
        #    sapply(trcsColl@tracksCollection, function(X) {
        #      lapply(X@tracks, function(x) x@data[ , infolocs_colnames]) })) )
        expect_that(unlist(lapply(ltrdf_list, function(z) {
          attributes(z)$infolocs[
            which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), ] })),
          is_equivalent_to(
            unlist(lapply(trcsColl@tracksCollection, function(X) {
              unlist(lapply(X@tracks, function(x) x@data[ , infolocs_colnames])) })) ))
        trC <- unlist(lapply(trcsColl@tracksCollection, function(X) {
          unlist(lapply(X@tracks, function(x) x@data[ , infolocs_colnames])) }))
        names(trC) <- NULL
        expect_that(trC, equals(
            unlist(lapply(ltrdf_list, function(z) { attributes(z)$infolocs[
                which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), ] })) ))
        expect_that(trC, is_identical_to(
          unlist(lapply(ltrdf_list, function(z) { attributes(z)$infolocs[
            which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), ] })) ))
      
      } else { # no infolocs
        
        # length (number of attributes)
        expect_that(unlist(lapply(ltrdf_list, function(x) length(x))), is_equivalent_to(
          unlist(lapply(trcsColl@tracksCollection, function(X) {
            lapply(X@tracks, function(x) { length(x@data) + length(x@connections) - 1 }) }))))
  
      }

      # column R2n (need to be in data slot of track object)
      expect_that(unlist(lapply(ltrdf_new_list, function(x) x$R2n)), is_equivalent_to(
        unlist(lapply(trcsColl@tracksCollection, function(X) { 
          lapply(X@tracks, function(x) x@data$R2n) })) ))

      # connections slot data
      #z <- ltrdf_new_connData_list[[1]]
      expect_that(unlist(lapply(ltrdf_new_connData_list, function(z) { 
        z[-nrow(z), ] })), is_equivalent_to(
          unlist(lapply(trcsColl@tracksCollection, function(X) { 
            lapply(X@tracks, function(x) x@connections[ , ltrdf_new_connData_colnames]) }))))

    })
    
  } else { # no NA values in ltraj coords and time
    
    test_that("ltraj object without NA values to TracksCollection - further tests", {
      
      # length / nrow 
      expect_that(ltrdf_nrow_all, is_identical_to(
        sum(unlist(lapply(trcsColl@tracksCollection, function(X) {
          lapply(X@tracks, function(x) nrow(x@data)) })))))
      expect_that(unlist(lapply(ltrdf_list_nrow, function(x) x - 1)), is_equivalent_to(
        unlist(lapply(trcsColl@tracksCollection, function(X) {
          lapply(X@tracks, function(x) nrow(x@connections)) }))))
      
      # names
      expect_that(unique(idVec), is_identical_to(names(trcsColl@tracksCollection)))
      # Just equivalent due to names in TracksCollection
      expect_that(burstVec, is_equivalent_to(unlist(lapply(trcsColl@tracksCollection, function(X) 
      { names(X@tracks) }))))
      
      # coords
      expect_that(unlist(lapply(ltrdf_list, function(z) { z$x })), is_equivalent_to(
          unlist(lapply(trcsColl@tracksCollection, function(X) {
            lapply(X@tracks, function(x) x@sp@coords[,1]) }))))
      expect_that(unlist(lapply(ltrdf_list, function(z) { z$y })), is_equivalent_to(
          unlist(lapply(trcsColl@tracksCollection, function(X) {
            lapply(X@tracks, function(x) x@sp@coords[,2]) }))))
      
      # time
      expect_that(unlist(lapply(ltrdf_list, function(z) { z$date })), is_equivalent_to(
          unlist(lapply(trcsColl@tracksCollection, function(X) {
            lapply(X@tracks, function(x) zoo::index(x@time)) }))))
      
      # rownames
      expect_that(unlist(lapply(ltrdf_list, function(z) { attributes(z)$row.names })),
        is_equivalent_to(unlist(lapply(trcsColl@tracksCollection, function(X) {
          lapply(X@tracks, function(x) attr(x@data, "row.names")) }))))
      expect_that(unlist(lapply(ltrdf_list, function(z) {
        head(attributes(z)$row.names, -1L) })),
        is_equivalent_to(unlist(lapply(trcsColl@tracksCollection, function(X) {
          lapply(X@tracks, function(x) attr(x@connections, "row.names")) }))))
      
      # infolocs
      # ?reicht folgendes ???
      # adehabitatLT manual: "... all the variables are measured for all bursts and ids"
      if (!is.null(attributes(ltrdf_list[[1]])$infolocs)) {   
        
        # length (number of attributes) / nrow and infolocs
        expect_that(unlist(lapply(ltrdf_list, function(z) { 
          nrow(data.frame(attributes(z)$infolocs)) })),
          is_equivalent_to(unlist(lapply(trcsColl@tracksCollection, function(X) {
            lapply(X@tracks, function(x) nrow(x@data)) }))))
        expect_that(unlist(lapply(ltrdf_list, function(z) { 
          nrow(data.frame(attributes(z)$infolocs)) - 1 })),
          is_equivalent_to(unlist(lapply(trcsColl@tracksCollection, function(X) {
            lapply(X@tracks, function(x) nrow(x@connections)) }))))
        expect_that(unlist(lapply(ltrdf_list, function(z) { 
          length(attributes(z)$infolocs) + length(z) })), is_equivalent_to(
            unlist(lapply(trcsColl@tracksCollection, function(X) {
              lapply(X@tracks, function(x) { length(x@data) + length(x@connections) - 1 }) }))))
        
        # rownames and infolocs
        #
        ## hier gibt probleme: row.names infolocs je Track durchlaufend von 1 an...
        #expect_that(unlist(lapply(ltrdf_list, function(z) {
        #  row.names(data.frame(attributes(z)$infolocs[
        #    which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), ])) })),
        # is_equivalent_to(
        ## row.names von trColl ok
        #   unlist(lapply(trcsColl@tracksCollection, function(X) {
        #     lapply(X@tracks, function(x) as.character(attr(x@data, "row.names"))) }))
        #   ))
        #
        # vllt gehts mit ltrdf_new_list:
        #attr(attributes(ltrdf_new_list[[1]])$infolocs, "row.names")
        expect_that(unlist(lapply(ltrdf_list, function(z) {
          attr(attributes(z)$infolocs, "row.names") })), is_equivalent_to(
            unlist(lapply(trcsColl@tracksCollection, function(X) {
              lapply(X@tracks, function(x) as.character(attr(x@data, "row.names"))) }))))
        
        # infolocs data 
        # Equivalent but not always identical, because the row.names of infolocs are  
        # always of type character but the coercion is done with respect to the ltraj
        # attribute row.names which also might be of type integer.
        infolocs_colnames <- colnames(attributes(ltrdf_list[[1]])$infolocs)
        #expect_that(sapply(ltrdf_list, function(z) {
        #  attributes(z)$infolocs[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), ] }),
        #  is_equivalent_to(
        #    sapply(trcsColl@tracksCollection, function(X) {
        #      lapply(X@tracks, function(x) x@data[ , infolocs_colnames]) })) )
        #expect_that(unlist(lapply(ltrdf_list, function(z) {
        #  attributes(z)$infolocs[which(!is.na(z$x) & !is.na(z$y) & !is.na(z$date)), ] })),
        #  is_equivalent_to(
        #    unlist(lapply(trcsColl@tracksCollection, function(X) {
        #      unlist(lapply(X@tracks, function(x) x@data[ , infolocs_colnames])) })) ))
        expect_that(do.call(rbind, (lapply(ltrdf_list, function(z) {
          attributes(z)$infolocs }))),
          is_equivalent_to(
            data.frame(unlist(lapply(trcsColl@tracksCollection, function(X) {
              unlist(lapply(X@tracks, function(x) x@data[ , infolocs_colnames])) })) )))
        #ltrData <- do.call(rbind, (lapply(ltrdf_list, function(z) {
        #  attributes(z)$infolocs })))
        #trajData <- data.frame(unlist(lapply(trcsColl@tracksCollection, function(X) {
        #  unlist(lapply(X@tracks, function(x) x@data[ , infolocs_colnames])) })) )
        #names(ltrData) <- paste("a", seq_along(ltrData), sep = "")
        #names(trajData) <- paste("a", seq_along(trajData), sep = "")
        #expect_that(trajData, equals(ltrData))
        #expect_that(trajData, is_identical_to(ltrData))
        # --> not due to row.names
        
      } else { # no infolocs
        
        # length (number of attributes)
        expect_that(unlist(lapply(ltrdf_list, function(x) length(x))), is_equivalent_to(
          unlist(lapply(trcsColl@tracksCollection, function(X) {
            lapply(X@tracks, function(x) { length(x@data) + length(x@connections) - 1 }) }))))
        
      }
      
      # column R2n (need to be in data slot of track object)
      expect_that(unlist(lapply(ltrdf_list, function(x) x$R2n)), is_equivalent_to(
        unlist(lapply(trcsColl@tracksCollection, function(X) { 
          lapply(X@tracks, function(x) x@data$R2n) })) ))
      
      # connections slot data
      #z <- ltrdf_new_connData_list[[1]] #ltrdf_new_connData_list
      expect_that(unlist(lapply(ltrdf_list_connData, function(z) { 
        z[-nrow(z), ] })), is_equivalent_to(
          unlist(lapply(trcsColl@tracksCollection, function(X) { 
            lapply(X@tracks, function(x) x@connections[ , ltrdf_connData_colnames]) }))))

    })
  }
}

#print(Sys.time())