
#' @import methods sp spacetime trajectories
#' @importFrom rgeos gIntersects
NULL

#' @include agg_allGenerics.R
NULL

#
# overIgnDom_sp_Track
#

# This method perform a overlay of a Track object and a Spatial object
# without respecting the temporal domain in the returned object. 
# The main differences to an overlay
# implemented in package sp (of points and spatial objects) is the 
# support of weighting and the calculation of metadata.
# The method is internally used in the implementation of the methods
# over (for Track and Spatial objects) and count.

overIgnDom_sp_Track <- function(x, y, returnList = FALSE, fn = NULL, ...,
                          use.data = FALSE, weight.points = NULL) {
  
  stopifnot(identicalCRS(x,y))
  
  # Need of coercion to SpatialPolygons if x is gridded,
  # because gIntersects just works with SpatialPolygons.
  if (gridded(x)) {
    x <- as(x, "SpatialPolygons")
  }
  
  # Create a matrix indicating intersections of x and y
  gI_matrix <- rgeos::gIntersects(y@sp, x, byid = TRUE)
  
  # Create a list with length == length(x), 
  # in which the i-th element contains a vector of indices of that points from y,
  # which are intersecting the i-th element of x.
  
  if (all(gI_matrix == FALSE)) {
    indexList <- lapply(seq_along(x), function(z) { integer(0) })
  } else if (length(x) == 1) {
    # Special case: sp object with just one spatial instance
    indexList <- list(as.matrix(apply(gI_matrix, 1, function(z) which(z == TRUE)))[ , 1])
  } else {
    indexList <- apply(gI_matrix, 1, function(z) which(z == TRUE))
    names(indexList) <- NULL
  }
  
  cleanIndexList <- lapply(indexList, function(z) { attr(z, "names") <- NULL; z })
  
  if (use.data[1] == FALSE) { # indices should be returned
    
    if (returnList) {
  
      cleanIndexList
      
    } else { # returnList == FALSE
  
      unlist(lapply(cleanIndexList, function(z) z[1]), use.names = FALSE)
      
    }
    
  } else { # use.data == TRUE, data to be returned
    
    if (returnList) { # return a list of xts time series
    
      xtsList <- lapply(cleanIndexList, function(z) {
        if (length(y@data) > 0) {
          if (length(z) > 0) {
    
            df <- cbind(y@data[z, use.data, drop = FALSE], as.matrix(y@time[z]))
            xts::xts(df, index(y@time[z]), tzone = attr(y@time, "tzone"))
            
          } else {
            # Impossible to ceate an 'empty' xts object
            NA
          } 
        } else {
          #xts object without data
          if (length(z) > 0) {
            
            df <- y@data[z, FALSE, drop = FALSE]
            xts::xts(df, order.by = index(y@time[z]), tzone = attr(y@time, "tzone"))
            
          } else {
            # Impossible to ceate an 'empty' xts object
            NA
          }
        }})
      
      xtsList
      
    } else { # returnList = FALSE and use.data = TRUE
      
      rNames <- row.names(x)
      
      if (is.null(fn)) {
        
        # return df with 1. value of each sp-unit ...
        # vector of first index or NA in case of no intersection
        firstIndexVec <- sapply(cleanIndexList, function(z) z[1])
        if (length(y@data) > 0) {
          df <- data.frame(y@data[firstIndexVec, use.data, drop=FALSE], row.names = rNames)
        } else {
          df <- data.frame(y@data[firstIndexVec, FALSE, drop=FALSE], row.names = rNames)
        }
        df$time <- index(y@time)[firstIndexVec]
        df$timeIndex <- as.matrix(y@time)[firstIndexVec]
        df
        
      } else { # fn != NULL
        
        fn = match.fun(fn)
        
        # warning in case of fn == sum
        if (paste(deparse(fn), collapse="") == paste(deparse(sum),collapse="")) {
          warning("Aggregation of trajectory data using 'sum' as aggregation function is not meaningful.")
        }
        
        # Calculate some metadata for each cell / polygon
       
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
        
        # metadata # duration per spatial unit
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
        
        # metadata # duration per spatial unit
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
            
                    fn(zz, ...) 
            
                  } else {
                    NA
                  }
                }))
              }})
            
            data.frame(do.call(rbind, dfList), metadata, row.names = rNames)
            
          } else { # !is.null(weight.points)
            
            # weighting just for numeric attributes ?!?!?
            
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

      } # Finish: use.data = TRUE and returnList = FALSE and fn != NULL
    } # Finish: use.data = TRUE and returnList = FALSE
  }  # Finish: use.data == TRUE, data to be returned
}


# ! overIgnDom is not expoted and thus not documented officially

# # @rdname over
# # @aliases over,SpatialPolyons,Track
# @param use.data
# @param weights
setMethod("overIgnDom", signature(x = "SpatialPolygons", y = "Track"), overIgnDom_sp_Track)

# # @rdname over
# # @aliases over,SpatialGrid,Track
setMethod("overIgnDom", signature(x = "SpatialGrid", y = "Track"), overIgnDom_sp_Track)

# # @describeIn over
# # \cr This may be a sub-description...!
# # @aliases over,SpatialPixels,Track
setMethod("overIgnDom", signature(x = "SpatialPixels", y = "Track"), overIgnDom_sp_Track)
