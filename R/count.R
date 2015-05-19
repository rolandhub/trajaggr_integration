
#' @import methods sp spacetime trajectories
NULL

#' @importFrom rgeos gIntersects
NULL

#' @include over_STF.R agg_allGenerics.R
NULL



### Count of Track ###

count_Track <- function(x, by) { 
  
  if (is(by, "STF")) {
    if (!(is(by@sp, "SpatialPolygons") || is(by@sp, "SpatialPixels"))) {
      stop("If method 'count' is called with an STF object for argument by, the sp slot of by needs to be of class SpatialPolygons or SpatialPixels")
    }
    pi <- over(by, x, returnList = FALSE, use.data = FALSE)
  } else if (is(by, "Spatial")) {
    pi <- overIgnDom(by, x, returnList = FALSE, use.data = FALSE)
  } else {
    stop("No adequate object passed to argument by in method count.")
  }
  
  countVec <- as.numeric(!is.na(pi))# !!!
  
  # Replace 0 with NA
  countVec[countVec == 0] <- NA
  
  if ("data" %in% slotNames(by)) {
    by@data$ntraj <- countVec
    by
  } else{
    addAttrToGeom(by, data.frame(ntraj = countVec), match.ID = FALSE)
  }
}

#' @rdname count
#' @aliases count,Track,STF
setMethod("count", signature(x = "Track", by = "STF"), count_Track)

#' @rdname count
#' @aliases count,Track,SpatialPolygons
setMethod("count", signature(x = "Track", by = "SpatialPolygons"), count_Track)

#' @rdname count
#' @aliases count,Track,SpatialPixels
setMethod("count", signature(x = "Track", by = "SpatialPixels"), count_Track)

#' @rdname count
#' @aliases count,Track,SpatialGrid
setMethod("count", signature(x = "Track", by = "SpatialGrid"), count_Track)



#########################################################


### Count of Tracks ###

count_Tracks <- function(x, by) {
  
  if (is(by, "STF")) {
    if (!(is(by@sp, "SpatialPolygons") || is(by@sp, "SpatialPixels"))) {
      stop("If method 'count' is called with an STF object for argument by, the sp slot of by needs to be of class SpatialPolygons or SpatialPixels")
    }    
    FUN <- over
  } else if (is(by, "Spatial")) {
    FUN <- overIgnDom
  } else {
    stop("No adequate object passed to argument by in method count.")
  }
  
  lst <- lapply(x@tracks, function (z) {
    pi <- FUN(by, z, returnList = FALSE, use.data = FALSE)
    countVec <- as.numeric(!is.na(pi))
  })
  
  rSum <- rowSums(do.call(cbind, lst))#, na.rm = TRUE)
  
  # Replace 0 with NA
  rSum[rSum == 0] <- NA
  
  if ("data" %in% slotNames(by)) {
    by@data$ntraj <- rSum
    by
  } else{
    addAttrToGeom(by, data.frame(ntraj = rSum), match.ID = FALSE)
  }
}

#' @rdname count
#' @aliases count,Tracks,STF
setMethod("count", signature(x = "Tracks", by = "STF"), count_Tracks)

#' @rdname count
#' @aliases count,Tracks,SpatialPolygons
setMethod("count", signature(x = "Tracks", by = "SpatialPolygons"), count_Tracks)

#' @rdname count
#' @aliases count,Tracks,SpatialPixels
setMethod("count", signature(x = "Tracks", by = "SpatialPixels"), count_Tracks)

#' @rdname count
#' @aliases count,Tracks,SpatialGrid
setMethod("count", signature(x = "Tracks", by = "SpatialGrid"), count_Tracks)


########################################################


### Count of TracksCollection ###

count_TracksCollection <- function(x, by, byID = FALSE) {
  
  if (is(by, "STF")) {
    if (!(is(by@sp, "SpatialPolygons") || is(by@sp, "SpatialPixels"))) {
      stop("If method 'count' is called with an STF object for argument by, the sp slot of by needs to be of class SpatialPolygons or SpatialPixels")
    }
    FUN <- over
  } else if (is(by, "Spatial")) {
    FUN <- overIgnDom
  } else {
    stop("No adequate object passed to argument by in method count.")
  }
  
  listsByID <- lapply(x@tracksCollection, function (z) {
    lapply(z@tracks, function(zz) {
      pi <- FUN(by, zz, returnList = FALSE, use.data = FALSE)
      countVec <- as.numeric(!is.na(pi))# !!!
    })
  }) 
  
  if (byID == FALSE) {
    
    lst <- unlist(listsByID, recursive = FALSE, use.names = FALSE)
    rSum <- rowSums(do.call(cbind, lst))
  
    # Replace 0 with NA
    rSum[rSum == 0] <- NA
    
    if ("data" %in% slotNames(by)) {
      by@data$ntraj <- rSum
      by
    } else {
      addAttrToGeom(by, data.frame(ntraj = rSum), match.ID = FALSE)
    }
  } else { # byID == TRUE
    
    rSumByID <- lapply(listsByID, function(z) {
      rSum <- rowSums(do.call(cbind, z))
      # Replace 0 with NA
      rSum[rSum == 0] <- NA
      rSum
    })
    
    newNames <- lapply(seq_along(rSumByID), function(z) {
      newName <- paste("ntraj_", names(x@tracksCollection[z]), sep = "") 
    })
    
    if ("data" %in% slotNames(by)) {
      
      for (i in seq_along(newNames)) {
        by@data[newNames[[i]]] <- rSumByID[[i]]
      }
      by
      
    } else {
      
      df <- data.frame(rSumByID)
      names(df) <- newNames
      addAttrToGeom(by, df, match.ID = FALSE)
      
    }
  }
}

#' @rdname count
#' @aliases count,TracksCollection,STF
setMethod("count", signature(x = "TracksCollection", by = "STF"), 
          count_TracksCollection)

#' @rdname count
#' @aliases count,TracksCollection,SpatialPolygons
setMethod("count", signature(x = "TracksCollection", by = "SpatialPolygons"), count_TracksCollection)

#' @rdname count
#' @aliases count,TracksCollection,SpatialPixels
setMethod("count", signature(x = "TracksCollection", by = "SpatialPixels"), count_TracksCollection)

#' @rdname count
#' @aliases count,TracksCollection,SpatialGrid
setMethod("count", signature(x = "TracksCollection", by = "SpatialGrid"), count_TracksCollection)
