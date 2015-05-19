#' @import methods sp xts spacetime trajectories
NULL

# @ importFrom xts xts
#' @importFrom rgeos gIntersects
NULL

#' @include agg_allGenerics.R
NULL

###################################################################
#
# over:
# 

# ####
# # Usage of rgeos::gIntersects instead of sp::over because of some strange 
# # results given by over with signature(x, SpatialPointsDataFrame)
# # with x being SpatialPolygons coerced from SpatialGrid (or SpatialPixels):
# require(trajectories); example("Track")
# require(raster)
# rasterLayerA1 <- raster(as(A1, "SpatialPointsDataFrame"),ncols=3,nrows=3)
# spGridA1 <- as(rasterLayerA1, "SpatialGrid")
# spPolygA1 <- as(rasterLayerA1, "SpatialPolygons")
# spPolygA1_fromGrid <- as(spGridA1, "SpatialPolygons") # !!!
# # Determine which points are intersecting which polygons by rgeos::gIntersects
# gI_Polyg <- rgeos::gIntersects(spPolygA1, A1@sp, byid = TRUE)
# gI_Polyg_fromGrid <- rgeos::gIntersects(spPolygA1_fromGrid, A1@sp, byid = TRUE)
# # Testing for equivalence of the gIntersects results
# all.equal(gI_Polyg, gI_Polyg_fromGrid)
# # --> [1] "Attributes: < Component “dimnames”: Component 2: 9 string mismatches >"
# # --> ok!
# # Determine which points are intersecting which polygons by sp::over
# over_Polyg <- over(spPolygA1, A1@sp, returnList = TRUE)
# over_Polyg_fromGrid <- over(spPolygA1_fromGrid, A1@sp, returnList = TRUE)
# # Testing for equivalence of the over results
# all.equal(over_Polyg, over_Polyg_fromGrid)
# # [1] "Component 1: Numeric: lengths (1, 0) differ" "Component 3: Numeric: lengths (2, 0) differ"
# # --> ?
# # A similar problem occurs with SpatialPolygons coerced from SpatialSpixels.
# ####



###################################################################


#
# over.STF.Track
#

over_STF_Track <- function(x, y, returnList = FALSE, fn = NULL, ...,
                          use.data = FALSE, weight.points = NULL) {
  
  stopifnot(identicalCRS(x,y))
  
  stopifnot(identical(attr(x@time, "tzone"), attr(y@time, "tzone")))
  
  # warning in case of fn == sum
  if (paste(deparse(fn), collapse="") == paste(deparse(sum),collapse="")) {
    warning("Aggregation of trajectory data using 'sum' as aggregation function is not meaningful.")
  }
  
  geom <- geometry(x@sp)
  
  ## !!! 
  #if (!(is(geom, "SpatialPolygons") || is(geom, "SpatialPixels"))) {
  #  stop("If method over is called with an STF object for argument x, the sp slot of x needs to be of class SpatialPolygons or SpatialPixels")
  #}
  
  if (gridded(geom)) {
    geom <- as(geom, "SpatialPolygons")
  }
  
  # Get the time matches
  if (any(x@endTime > as.POSIXct(index(x@time)))) {
    #end.x <- x@endTime
    tm <- timeMatch(index(x@time), index(y@time), returnList = TRUE, x@endTime)
  } else {
    tm <- timeMatch(index(x@time), index(y@time), returnList = TRUE)
  }
  
  indexLists <- lapply(seq_along(tm), function(z) {
    ti = tm[[z]] # y entry indices for x@time[i] intervals
    if (length(ti) > 0) {
      
      #ti # point indices belonging to this time slice
      gI_matrix <- rgeos::gIntersects(y@sp[ti], geom, byid = TRUE) 
      
      if (all(gI_matrix == FALSE)) {
        
        indexL <- lapply(seq_along(geom), function(z) { integer(0) })
        
      } else if (length(x@sp) == 1) {
        
        indexL <- list(ti[gI_matrix])
        
      } else {
        
        indexL <- apply(gI_matrix, 1, function(zz) ti[which(zz == TRUE)])
        
        # Clean
        names(indexL) <- NULL
        
      }      
          
      indexL
      
    } else { # length(ti) == 0 --> no Track-Points do match this time-slice
      
      lapply(seq_along(geom), function(z) { integer(0) })
      
    }
  })
  
  cleanIndexList <- do.call(c, indexLists)
  
  if (use.data[1] == FALSE) { # indices should be returned
    
    if (returnList) {
      
      cleanIndexList
      
    } else { # returnList == FALSE
      
      unlist(lapply(cleanIndexList, function(z) z[1]), use.names = FALSE)
      
    }
    
  } else { # use.data == TRUE or selection, data to be returned
    
    # Auskommentieren da ich evtl leere Trackobjecte in agg erlauben will ?!?!
    # --> (TODO)
    #stopifnot(nrow(y@data) > 0 )
    # ??? TODO !!!
    #stopifnot(nrow(y@data) > 0 && length(y@data) > 0)
    #if (length(y@data) == 0) {
    #  dfList
    #}
    
    if (returnList) { # return a list of data.frames
            
      dfList <- lapply(cleanIndexList, function(z) {
        # It's (theoretically) possible to have a legal Track object without columns in the data slot
        if (length(y@data) > 0) {
          if (length(z) > 0) {
            
            df <- y@data[z, use.data, drop = FALSE]
          
          } else { 
                      
            df <- y@data[FALSE, use.data, drop = FALSE]
                      
          }
        } else {
          
          df <- data.frame()
          
        }
      })
      
      dfList
      
    } else { # returnList = FALSE and use.data = TRUE or selection
      
      if ("data" %in% slotNames(x)) {
        rNames <- row.names(x@data) } else { rNames <- NULL }
      
      if (is.null(fn)) {
        
        # return df with 1. value of each STF-unit
        
        # vector of first index or NA in case of no intersection
        firstIndexVec <- sapply(cleanIndexList, function(z) z[1])
        if (length(y@data) > 0) {
          df <- data.frame(y@data[firstIndexVec, use.data, drop=FALSE], row.names = rNames)  
        } else {
          df <- data.frame(y@data[firstIndexVec, FALSE, drop=FALSE], row.names = rNames)
        }
        
        # !!! time added additionally !!!
        df$time <- index(y@time)[firstIndexVec]
        df$timeIndex <- as.matrix(y@time)[firstIndexVec]
        df
        
      } else { # fn != NULL
        
        # number of relocations
        nlocsList <- lapply(cleanIndexList, function(z) length(z))
        nlocs <- do.call(rbind, nlocsList)
        # If nlocs == 0, set to NA
        nlocs[nlocs == 0] <- NA
        
        # approximate duration
        halfConnDuration <- lapply(1:(length(y@time) - 1), function(z) {
          y@connections[z, "duration"] / 2 })
        durMatrix <- matrix(data = as.numeric(c(c(0, halfConnDuration), c(halfConnDuration, 0))),
                            nrow = length(y), ncol = 2)
        approxPointDuration <- .rowSums(durMatrix, length(y), 2, na.rm = TRUE)
        
        # !!! Duration of each Point separated by cells / polygons
        approxPointsInCellsDurList <- lapply(cleanIndexList, function(z) {
          if (length(z) == 0) { NA } else { approxPointDuration[z] } })
        
        approxDurationPerCell <- do.call(rbind, lapply(approxPointsInCellsDurList, sum))
        
        # approximate length
        halfConnDist <- lapply(1:(length(y@time) - 1), function(z) {
          round(y@connections[z, "distance"] / 2, 2) })
        distMatrix <- matrix(data = as.numeric(c(c(0, halfConnDist), c(halfConnDist, 0))),
                            nrow = length(y), ncol = 2)
        approxPointDist <- .rowSums(distMatrix, length(y), 2, na.rm = TRUE)
        
        # !!! Duration of each Point separated by cells / polygons
        approxPointsInCellsDistList <- lapply(cleanIndexList, function(z) {
          if (length(z) == 0) { NA } else { approxPointDist[z] } })
        
        approxDistPerCell <- do.call(rbind, lapply(approxPointsInCellsDistList, sum))
        
        metadata <- data.frame(nlocs = nlocs, approx_duration = approxDurationPerCell,
                               approx_distance = approxDistPerCell)
        
        if (length(y@data) > 0) {
        
          if (is.null(weight.points)) { # apply fn to data without weighting
            
            dfList <- lapply(cleanIndexList, function(z) {
              dat <- y@data[z, use.data, drop=FALSE]
              if (nrow(dat) == 0) {
                data.frame(lapply(dat, function(zz) { c(zz, NA) } ))
              } else {
                data.frame(lapply(dat, function(zz) { 
                  # avoid NaN
                  if (any(!is.na(zz))) {
                    
                    # !!! Achtung : Auskommentierung ändern!
                    #fn(zz, na.rm=T)
                    fn(zz, ...)

                  } else {
                    NA
                  }
                }))
              }})
                        
            data.frame(do.call(rbind, dfList), metadata, row.names = rNames)
            
          } else { # !is.null(weight.points)
            
            # weighting just for numeric attributes !?!?!
            
            weightsList <- switch(weight.points,
                                  byTime  = approxPointsInCellsDurList,
                                  byDist  = approxPointsInCellsDistList,
                                  # Possibility to use equal weights to enable 'real'
                                  # weighting JUST for tracks (weight.tracks).
                                  equal   = lapply(approxPointsInCellsDurList, 
                                                   function(z) {
                                                     if (!is.na(z[1])) z[] <- 1; z }),
                                  stop("No adequate string is passed to the weight.points argument!"))
            
            
            dfList <- lapply(seq_along(cleanIndexList), function(z) {
              dat <- y@data[cleanIndexList[[z]], use.data, drop=FALSE]
              if (nrow(dat) == 0) {
                data.frame(lapply(dat, function(zz) { c(zz, NA) } ))
              } else {
                
                data.frame(lapply(dat, function(zz) {
                                    
                  if (is.numeric(zz)) {
                    
                    #fn(zz, weightsList[[z]], na.rm=TRUE)  
                    fn(zz, weightsList[[z]], ...) 
                    
                  } else {
                    
                    warning("Weighting functions may just be applied to numeric attributes: A non numeric attribute was encountered. NA is returned.")
                    NA
                    
                  }
                }))
              }})
            
            data.frame(do.call(rbind, dfList), metadata, row.names = rNames)
          
          }
        } else { # length(y@data) == 0
          
          warning(paste("Method over was called with use.data = ", use.data, " and fn != NULL, but the data slot of the Track object does not contain any attributes. A data.frame just containing metadata is returned.", sep = ""))
          
          data.frame(y@data[seq_along(cleanIndexList), FALSE, drop = FALSE],
                     metadata, row.names = rNames)
          
        }
      } # Finish: returnList = FALSE and use.data = TRUE and fn != NULL
    } # Finish: returnList = FALSE and use.data = TRUE
  } # Finish: use.data == TRUE, data to be returned
}

#' @rdname over
#' @aliases over,STF,Track
setMethod("over", signature(x = "STF", y = "Track"), over_STF_Track)