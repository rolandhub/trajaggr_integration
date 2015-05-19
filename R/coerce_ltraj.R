################################################################################
# Coercion of ltraj objects
#-------------------------------------------------------------------------------

#' @import methods sp spacetime trajectories
NULL


#-------------------------------------------------------------------------------

# Function to restructure ltraj object if NA entires are dropped.
# Recalculates the data. (Internal function)
# ltrdf = data.frame from ltraj object
# legalRowsInd = indices of the rows with legal data (no NA)

restructureLtrajDf <- function(ltrdf, legalRowsInd) {
  
  curDf <- ltrdf[legalRowsInd, , drop = FALSE]
  
  # adehabitatLT imported, thus the following lines are not needed any more.
  # Originally it was not planed to import the package,
  # but it is needed for illustartion in the vignette...
  
  ## The suggested package adehabitatLT is needed for the following function.
  #if (!requireNamespace("adehabitatLT", quietly = TRUE))
  #  stop("Package adehabitatLT (function adehabitatLT::dl) is required 
  #       to coerce from ltraj objects having NA values in 
  #       coordinates or temporal information.")
  
  # Includes a recaluculaton of step characteristics!
  ltr_new_fromCurDf <- adehabitatLT::dl(curDf)
  
  curDf <- ltr_new_fromCurDf[[1]]
  
  message(paste("Information\nBurst ", attributes(ltrdf)$burst, 
                ": Relocations with missing coordinates have been excluded.",
                " Burst is definitely not regular (anymore).",
                " The step characteristics have been recalculated.\n", sep=""))
  
  if (!is.null(attributes(ltrdf)$infolocs)) {
  
    cur_info_locs <- attributes(ltrdf)$infolocs[legalRowsInd, , drop = FALSE]
    names(cur_info_locs) <- names(attributes(ltrdf)$infolocs)
    
    stopifnot(nrow(curDf) == nrow(cur_info_locs))
    
    curDf <- cbind(curDf, cur_info_locs) # 
    
  }
  
  curDf
  
}


#-------------------------------------------------------------------------------


# Function that creates a Track object from a 'ltraj-data.frame'
# (Internal function)

ltrajDfToTrack <- function(ltrdf) {
  
  legalRowsInd <- !is.na(ltrdf$x) & !is.na(ltrdf$y) & !is.na(ltrdf$date)
  
  rn <- attributes(ltrdf)$row.names[legalRowsInd]
  
  if (FALSE %in% legalRowsInd){
    
    curDf <- restructureLtrajDf(ltrdf, legalRowsInd)
    
  } else {
    
    curDf <- ltrdf
  
    if (!is.null(attributes(curDf)$infolocs)) {
      info_locs <- attributes(curDf)$infolocs
  
      stopifnot(nrow(curDf) == nrow(info_locs))
      
      stopifnot(row.names(curDf) == row.names(info_locs))
      
      curDf <- cbind(curDf, info_locs) # 
    }
    
  }
  
  # df to Track....
  
  attr(curDf, "row.names") <- rn
  
  crds <- curDf[c("x","y")]
  
  time <- curDf[["date"]]
  
  sp <- SpatialPoints(crds)
  
  connData <- curDf[c("dx", "dy", "dist", "dt", "abs.angle", "rel.angle")]
  
  # Need to delte the last row
  connData <- connData[-(nrow(connData)), , drop = FALSE]
  
  namesCurDf <- names(curDf)
  varToDrop <- namesCurDf %in% c("x", "y", "date", "dx", "dy", "dist", "dt", "abs.angle", "rel.angle")
  
  curDf <- curDf[!varToDrop]
  
  #require(spacetime)
  stidf <- STIDF(sp, time, data = curDf)
  
  #require(trajectories)
  Track(stidf, df = connData)
  
}


#-------------------------------------------------------------------------------

setAs("ltraj", "Track",
      function(from) {
        
        stopifnot(attributes(from)$typeII && length(from) == 1)
        
        ltrajDfToTrack(from[[1]])
      }
)


#-------------------------------------------------------------------------------


setAs("ltraj", "Tracks",
      function(from) {
        
        idVec <- sapply(from, function(x) attributes(x)$id)
        
        if (!attributes(from)$typeII) {        
          stop("attributes(from)$typeII is not TRUE") 
        }
        
        # ltraj object must be of length > 1
        if (length(from) < 2) {
          stop("length(from) > 1 is not TRUE!")
        }
        
        # Expecting just one id; several trajectories from one individual
        if (length(unique(idVec)) > 1) {
          stop("length(unique(idVec)) < 2 is not TRUE!")
        }
        
        trackList <- lapply(from, function(x) ltrajDfToTrack(x))
        
        names(trackList) <- sapply(from, function(x) attributes(x)$burst)
        
        Tracks(trackList)
        
      }
)


#-------------------------------------------------------------------------------


setAs("ltraj", "TracksCollection",
      function(from) {

        idVec <- sapply(from, function(x) attributes(x)$id)
        uniqueIdVec <- unique(idVec)
        names(uniqueIdVec) <- uniqueIdVec # !!!
        
        idVecWithBurstNames <- idVec 

        names(idVecWithBurstNames) <- sapply(from, function(x) attributes(x)$burst)
        
        if (!attributes(from)$typeII) {           
          stop("attributes(from)$typeII is not TRUE!") 
        }
        
        # Expecting more than 1 trajectory
        if (length(from) < 2) {
          stop("length(from) > 1 is not TRUE!")
        }
        
        # Expecting more than one individual
        if (length(unique(idVec)) < 2) {
          stop("length(unique(idVec)) > 1 is not TRUE!")
        }
                
        namedWhichList <- lapply(uniqueIdVec, function(x)
          as.list(which(x == idVecWithBurstNames)))
        
        tracksList <- lapply(namedWhichList, function(x)
          Tracks(lapply(x, function(X) ltrajDfToTrack(from[[X]]))))
        
        TracksCollection(tracksList)
      }
)
