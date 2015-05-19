
#' @import methods sp spacetime trajectories
NULL

## @importFrom rgeos gIntersects
## @ importFrom xts merge.xts
NULL

#' @include agg_allGenerics.R over_STF.R aggregate_by_STF.R over_sp.R
NULL


###
# Aggregation of Track: aggregate_Track_sp
###

aggregate_Track_sp <- function(x, by, FUN = mean, ..., simplify = TRUE, use.data = TRUE,
                               weight.points = NULL) {
  
  FUN = match.fun(FUN)
  
  # warning in case of FUN == sum
  if (paste(deparse(FUN), collapse="") == paste(deparse(sum), collapse="")) {
    warning("Aggregation of trajectory data using 'sum' as aggregation function is not meaningful.")
  }
  
  #identicalCRS tested in over!
  #identicalTZ as well!
  
  # Check for legal use.data argument
  stopifnot(isTRUE(use.data) || is.character(use.data) || is.numeric(use.data))
  
  # Get the indices of Track points intersection with the geometry passed to the by argument
  over_res_ind <- over(by, x, returnList = FALSE, fn = NULL, use.data = FALSE)
  w <- which(!is.na(over_res_ind))
  relevantInd <- unique(over_res_ind[w])
  
  # Create spatio-temporal geometry (STF object)
  stf <- STF(by, time = x@time[relevantInd], endTime = index(x@time)[relevantInd])
  
  # Get the resulting data
  # fn is not realy applied, fn = identity just to clarify that!
  # And fn needs to be passed due to guarantee the calculation of metadata
  over_res_data <- over(by, x, returnList = FALSE, fn = identity, use.data = use.data, 
                        weight.points = weight.points)
  
  stopifnot(length(stf) == nrow(over_res_data))
  
  # Add data to resulting object with respect to argument simplify
  if (simplify && length(by) == 1) { 
    
    xts(cbind(over_res_data, as.matrix(stf@time)), index(stf@time), 
        tzone = attr(x@time, "tzone"))
    
  } else if (simplify && nrow(stf@time) == 1) { 
    
    if ("data" %in% slotNames(by)) {
      over_res_data = data.frame(over_res_data, by@data, row.names = row.names(by@data))
    }
    addAttrToGeom(geometry(by), over_res_data, match.ID = FALSE)
    
  } else { # Return ST object
    
    addAttrToGeom(stf, over_res_data, match.ID = FALSE)
    
  }
}


# S4 method for S4 classes
setMethod("aggregateBy", signature(x = "Track", by = "SpatialPolygons"), 
          aggregate_Track_sp)

setMethod("aggregateBy", signature(x = "Track", by = "SpatialGrid"), 
          aggregate_Track_sp)

setMethod("aggregateBy", signature(x = "Track", by = "SpatialPixels"), 
          aggregate_Track_sp)



#########################################################


###
# Aggregation of Tracks: aggregate_Tracks_sp
###

aggregate_Tracks_sp <- function(x, by, FUN = mean, ..., simplify = TRUE, use.data = TRUE,
                                weight.points = NULL, weight.tracks = NULL) {
  
  # First identify the relevant / intersecting Track points
  indexList <- lapply(x@tracks, function(z) {
    over_res_ind <- over(by, z, returnList = FALSE, fn = NULL, use.data = FALSE)
    unique(over_res_ind[which(!is.na(over_res_ind))])
  })
  
  # Get the time data for each Track as a list
  timeList <- lapply(seq_along(x@tracks), function(z) {
    x@tracks[[z]]@time[indexList[[z]]]
  })
  
  # Merge times
  mxts <- do.call(merge.xts, timeList)
  
  # Clean merged xts object etc
  mxts <- xts(seq(1:nrow(mxts)), order.by = index(mxts), 
              tzone = attr(x@tracks[[1]]@time, "tzone"))
  names(mxts) <- "timeIndex"
  
  # Create STF object
  stf <- STF(by, time = mxts, endTime = index(mxts))
  
  # Aggregate Tracks by STF
  aggregate(x, stf, FUN = FUN, ..., simplify = simplify, use.data = use.data,
            weight.points = weight.points, weight.tracks = weight.tracks)
  
}


# S4 method for S4 classes
setMethod("aggregateBy", signature(x = "Tracks", by = "SpatialPolygons"), 
          aggregate_Tracks_sp)

setMethod("aggregateBy", signature(x = "Tracks", by = "SpatialGrid"), 
          aggregate_Tracks_sp)

setMethod("aggregateBy", signature(x = "Tracks", by = "SpatialPixels"), 
          aggregate_Tracks_sp)



##########################################################


###
# Aggregation of TracksCollection: aggregate_TracksColl_sp
###

aggregate_TracksColl_sp <- function(x, by, FUN = mean, ..., simplify = TRUE, 
                                    use.data = TRUE, weight.points = NULL, 
                                    weight.tracks = NULL, byID = FALSE) {
  
  if (byID == FALSE) {
    
    # 'Quick-and-Dirty' approach which causes a little computation overhead
    # by creating an overall Tracks object, but on the other hand 
    # this approach ensures code consistency and avoids a lot of identical code lines.
    
    # Create a list of Track objects from the TracksCollection object
    trackList <- unlist(lapply(x@tracksCollection, function (z) { 
      lapply(z@tracks, function(zz) { zz } ) }), recursive = FALSE, use.names = FALSE)
    
    # Create an overall Tracks object
    tracksObj <- Tracks(trackList)
    
    # Aggregate Tracks object by Spatial object
    aggregate(tracksObj, by, FUN, ..., simplify = simplify, use.data = use.data, 
              weight.points = weight.points, weight.tracks = weight.tracks)
    
  } else if (byID == TRUE) {

    # First identify the relevant / intersecting Track points
    indexList <- unlist(lapply(x@tracksCollection, function (z) { 
      lapply(z@tracks, function(zz) {
        over_res_ind <- over(by, zz, returnList = FALSE, fn = NULL, use.data = FALSE)
        unique(over_res_ind[which(!is.na(over_res_ind))])
      }) 
    }), recursive = FALSE, use.names = FALSE)
    
    # Get the time data for each Track combined in a list
    completeTimeList <- unlist(lapply(x@tracksCollection, function (z) { 
      lapply(z@tracks, function(zz) { zz@time }) 
    }), recursive = FALSE, use.names = FALSE)
    
    # Subset the time data due to the Track points intersecting the spatial geometry
    timeList <- lapply(seq_along(indexList), function(z) {
      completeTimeList[[z]][indexList[[z]]]
    })
    
    # Merge times
    mxts <- do.call(merge.xts, timeList)
    
    # Clean merged xts object etc.
    mxts <- xts(seq(1:nrow(mxts)), order.by = index(mxts), 
                tzone = attr(x@tracksCollection[[1]]@tracks[[1]]@time, "tzone"))
    names(mxts) <- "timeIndex"
    
    # Create STF object ... sp = by,  time = endTime = mxts
    stf <- STF(by, time = mxts, endTime = index(mxts))
    
    # Aggregate TracksCollection by STF
    aggregate(x, stf, FUN, ..., simplify = simplify, use.data = use.data,
              weight.points = weight.points, weight.tracks = weight.tracks, byID = byID)
    
  } else {
    
    stop("Method aggregate with an object of class TracksCollection passed to the x argument: argument byID needs to be logical.")
    
  }
}


# S4 method for S4 classes
setMethod("aggregateBy", signature(x = "TracksCollection", by = "SpatialPolygons"), 
          aggregate_TracksColl_sp)

setMethod("aggregateBy", signature(x = "TracksCollection", by = "SpatialGrid"), 
          aggregate_TracksColl_sp)

setMethod("aggregateBy", signature(x = "TracksCollection", by = "SpatialPixels"), 
          aggregate_TracksColl_sp)
