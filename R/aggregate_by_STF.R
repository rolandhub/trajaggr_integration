#' @import methods sp spacetime trajectories
NULL

#' @importFrom rgeos gIntersects
NULL

#' @include agg_allGenerics.R over_STF.R 
NULL


###
# Aggregation of Track: aggregate_Track_STF
###

aggregate_Track_STF <- function(x, by, FUN = mean, ..., simplify = TRUE, use.data = TRUE,
                                weight.points = NULL) {
  
  
  
  # by = stf / stfdf mit Polygons oder Pixels
  if (!(is(by@sp, "SpatialPolygons") || is(by@sp, "SpatialPixels"))) {
    stop("If method aggregate is called with an STF object for argument by, the sp slot of by needs to be of class SpatialPolygons or SpatialPixels")
  }
  
  # Check for legal use.data argument
  stopifnot(isTRUE(use.data) || is.character(use.data) || is.numeric(use.data))
  
  FUN = match.fun(FUN)
    
  over_res <- over(by, x, returnList = FALSE, fn = FUN, ..., use.data = use.data,
                   weight.points = weight.points)
    
  if (simplify && length(by@sp) == 1) { 
    
    # Return time series of length == nrow(by@time) 
    # with aggregated values and by@time data (if existing).
    # The by@data (if existing) is ignored.
    xts(cbind(over_res, as.matrix(by@time)), index(by@time), 
        tzone = attr(by@time, "tzone"))
    
  } else if (simplify && nrow(by@time) == 1) { 
    
    # Return spatial object of length == length(by@sp)
    # with aggregated values and by@sp data (if existing).
    # The by@data (if existing) is ignored.
    if ("data" %in% slotNames(by@sp)) {
      over_res = data.frame(over_res, by@sp@data, row.names = row.names(by@sp@data))
    }
    
    addAttrToGeom(geometry(by@sp), over_res, match.ID = FALSE)
    
  } else { # Return ST object: by-argument object with added aggregated values
    
    if ("data" %in% slotNames(by)) {
      over_res = data.frame(over_res, by@data, row.names = row.names(by@data))
    }
    addAttrToGeom(by, over_res, match.ID = FALSE)
    
  }  
}


# S4 method for S4 class
#' @rdname aggregate
#' @aliases aggregate,Track
setMethod("aggregate", signature(x = "Track"), 
          function(x, by, FUN = mean, ..., simplify = TRUE, use.data = TRUE, 
                   weight.points = NULL) {
            aggregateBy(x, by, FUN = FUN, simplify = simplify, use.data = use.data,
                        weight.points = weight.points, ...)
          })


# S3 method for S4 class
# In "R: General Information on Methods" it is recommended to define both S3 and S4
# methods if a S4 generic is desired for an existing S3 generic function:
aggregate.Track = function(x, by, FUN = mean, ..., simplify = TRUE, use.data = TRUE,
                           weight.points = NULL) {
  aggregateBy(x, by, FUN = FUN, simplify = simplify, use.data = use.data, 
              weight.points = weight.points, ...)}


# S4 method for S4 classes - with dispatch on a second argument...
setMethod("aggregateBy", signature(x = "Track", by = "STF"), 
          aggregate_Track_STF)


#############################################################


###
# Aggregation of Tracks: aggregate_Tracks_STF
###

aggregate_Tracks_STF <- function(x, by, FUN = mean, ..., simplify = TRUE, use.data = TRUE,
                                weight.points = NULL, weight.tracks = NULL) {
  
  # identicalCRS tested in over!
  # "identicalTZ" as well!
  
  # by = stf / stfdf with Polygons or Pixels
  if (!(is(by@sp, "SpatialPolygons") || is(by@sp, "SpatialPixels"))) {
    stop("If method aggregate is called with an STF object for argument by, the sp slot of by needs to be of class SpatialPolygons or SpatialPixels")
  }
    
  # Check for legal use.data argument
  stopifnot(isTRUE(use.data) || is.character(use.data) || is.numeric(use.data))
  
  FUN = match.fun(FUN)
  
  # Needed to perform occasional pre-selection of attributes before applying 'over'.
  trackList <- lapply(x@tracks, function(z) { z } )

  
  ###
  # This section is about data preselection and creation of missing attributes
  #
  # Enable attribute selection even accepting attributes which 
  # are not present in the data slots of all Track objects.
  # (It is no requirement of trajectories objects that all data slots 'are equal'.)
  
  # Preparation
  dataNamesList <- lapply(trackList, function(z) names(z@data))
  
  allNames <- Reduce(union, dataNamesList)
  
  commonNames <- Reduce(intersect, dataNamesList)
  identicalCols <- identical(commonNames, allNames)
  
  # Occasional pre-selection (--> performance) and occasional addition of 
  # partly missing attributes both related to the use.data argument.
  if (is.character(use.data)) {
    #useDataNamesStatus <- use.data %in% allNames
    if (!all(use.data %in% allNames)) {
      stop("Undefined columns requested: 
           One or more column names specified in use.data are not existent in any Track object.")
    } else if (!all(use.data %in% commonNames)) {
      # Adding attributes if missing and selection ...
      # Columns not identical but all requested column names are existent in at least one Track
      trackList <- lapply(trackList, function(z) { 
        for (i in seq_along(use.data)) {
          if (!use.data[i] %in% names(z@data)) {
            
              z@data[use.data[i]] <- NA
            
          }
        }
        
        # Pre-selection, even due to better performance
        z@data <- z@data[ , use.data, drop = FALSE]
        z
        
      })
      
    } else { #if (all(use.data %in% commonNames)) {
      
      # Just selection...
      trackList <- lapply(trackList, function(z) { 
      
        z@data <- z@data[ , use.data, drop = FALSE]
        z
        
      })
    }
    
    requestedNames <- use.data
    
  } else if (is.numeric(use.data)) {
    
    identicalColsAndOrder <- Reduce(identical, dataNamesList)
    if (identicalColsAndOrder) {
      trackList <- lapply(trackList, function(z) { 
    
        z@data <- z@data[ , use.data, drop = FALSE]
        z
        
      })
      
    } else {
      
      if (identicalCols) {
        warning("The data slots of the involved Track objects do not have an identical column order. In this case attribute selection by indices is not recommanded and needs to be applied with caution.")
        trackList <- lapply(trackList, function(z) { 
          
          z@data <- z@data[ , use.data, drop = FALSE]
          z
          
        })
        
      } else {
        
        warning("The data slots of the involved Track objects do not have identical columns. In this case attribute selection by indices is not recommanded and needs to be applied with caution, because after adding partly missing attributes an identical attribute order can not be ensured in any case which may lead to an error.")
        trackList <- lapply(trackList, function(z) { 
          if (length(allNames) > length(z@data)) {
            w <- which(!allNames %in% names(z@data))
            for (i in seq_along(w)) {
              
              z@data[allNames[w[i]]] <- NA
              
            }
          }
          z@data <- z@data[ , use.data, drop = FALSE]
          z
        })
      }
    }
    requestedNames <- names(trackList[[1]]@data[use.data])
    
  } else {
    if (!identicalCols) {
      
      trackList <- lapply(trackList, function(z) { 
      
        if (length(allNames) > length(z@data)) {
          w <- which(!allNames %in% names(z@data))
          for (i in seq_along(w)) {
      
            z@data[allNames[w[i]]] <- NA
      
          }
        }
        z
      })
    }
    requestedNames <- allNames
  }
  
  #####
  
  
  over_res_list <- lapply(trackList, function(z) {
    
    over(by, z, returnList = FALSE, fn = FUN, ..., use.data = use.data,
         weight.points = weight.points)
      
  })
  
  
  # !!! Calculate new metadata and then delete old from all over_res_list elements
  # New data: sum of length , duration and track objects per cell

  mdNames <- c("nlocs", "approx_duration", "approx_distance")
  trackmetadata_list <- lapply(over_res_list, function(z) {
    z[ , mdNames]
  })
  
  metadata_Lists <- lapply(seq_along(by), function(z) { 
    lapply(trackmetadata_list, function(zz) zz[z,])
  })
  
  metadata_list <- lapply(metadata_Lists, function(z) do.call(rbind, z))
  
  metadata_list <- lapply(metadata_list, function(z) {
    z$ntraj <- 0
    z$ntraj[z$nlocs > 0] <- 1
    z
  })
  
  sumList <- lapply(metadata_list, function(z) colSums(z, na.rm = T))
  
  
  # New metadata
  
  if ("data" %in% slotNames(by)) {
    rNames <- row.names(by@data) } else { rNames <- NULL }
  
  metadata <- data.frame(do.call(rbind, sumList), row.names = rNames) #row.names(by@data))
  
  # If values == 0, set to NA
  metadata[metadata$nlocs == 0, ] <- NA
  
  
  # Delete 'old' Track metadata even due to better performance
  over_res_list <-lapply(over_res_list, function(x) { x[ , !names(x) %in% mdNames, drop = FALSE] })
  
  
  
  # Check if over_res_list[[1]] contains all requested attributes.
  # If TRUE, over_res_list[[1]] will be used as a basis for the resulting df.
  if (identical(requestedNames, names(over_res_list[[1]]))) {
  
    res_df <- over_res_list[[1]]
  
  } else {
  
    res_df <- data.frame(matrix(vector(), nrow = length(by), ncol= length(requestedNames), 
                                      dimnames=list(c(), requestedNames)))
  }
  
  if (is.null(weight.tracks)) {
        
    for (i in seq_along(res_df)) {
      
      # Works if data columns in different order
      curName <- names(res_df[i])
      
      attrDataList <- lapply(over_res_list, function(z) {
        z[[curName]] 
        })
      
      # Create matrix out of list
      attrMatrix <- do.call(cbind, attrDataList)
      
      attrAgg <- apply(attrMatrix, 1, function(z) {
        if (any(!is.na(z))) { FUN(z, ...) } else { NA } }) # contains NaN !?!?!
      
      # Assign values to df
      res_df[ , curName] <- attrAgg
      
    }
    
    res_df <- data.frame(res_df, metadata, row.names = rNames)
    
  } else { # !is.null(weight.tracks)
    
    weightsList <- switch(weight.tracks,
                          byTime  = lapply(seq_along(metadata_list), function(z) { 
                            metadata_list[[z]][["approx_duration"]] }),
                          byDist  = lapply(seq_along(metadata_list), function(z) { 
                            metadata_list[[z]][["approx_distance"]] }),
                          # Possibility to use equal weights to enable 'real'
                          # weighting JUST for points (weight.points).
                          equal   = lapply(metadata_list, function(z) { 
                            z[ , "ntraj"] }),
                          stop("Method aggregate: No adequate string is passed to the weight.tracks argument!"))
    
    
    for (i in seq_along(res_df)) {
      curName <- names(res_df[i])
      attrDataList <- lapply(over_res_list, function(z) {
        z[[curName]] })
      
      # Create matrix out of list
      attrMatrix <- do.call(cbind, attrDataList)
      
      attrAgg <- unlist(lapply(1:nrow(attrMatrix), function(z) {
    
        # avoid NaN
        if (any(!is.na(attrMatrix[z, ]))) {
    
          # Achtung Auskommentierung
          FUN(attrMatrix[z, ], weightsList[[z]], ...)
          #FUN(attrMatrix[z, ], weightsList[[z]], na.rm = T)
          
        } else {
          NA
        }
      }), use.names = FALSE)
        
      # Assign values to df
      res_df[ , curName] <- attrAgg
        
    }
    
    res_df <- data.frame(res_df, metadata, row.names = rNames)
    
  }
  
  if (simplify && length(by@sp) == 1) { 
    # Return time series of length == nrow(by@time) 
    # with aggregated values and by@time data (if existing).
    # The by@data (if existing) is ignored.
    xts(cbind(res_df, as.matrix(by@time)), index(by@time), 
        tzone = attr(by@time, "tzone"))
    
  } else if (simplify && nrow(by@time) == 1) { 
    # Return spatial object of length == length(by@sp)
    # with aggregated values and by@sp data (if existing).
    # The by@data (if existing) is ignored.
    if ("data" %in% slotNames(by@sp)) {
      res_df <- data.frame(res_df, by@sp@data, row.names = row.names(by@sp@data))
    }
    addAttrToGeom(geometry(by@sp), res_df, match.ID = FALSE)
    
  } else { # Return ST object: object of class of the by-argument with added aggregated values
    if ("data" %in% slotNames(by)) {
      res_df <- data.frame(res_df, by@data, row.names = row.names(by@data))
    }
    addAttrToGeom(by, res_df, match.ID = FALSE)
  }
  
} # End


# S4 method for S4 class
#' @rdname aggregate
#' @aliases aggregate,Tracks
setMethod("aggregate", signature(x = "Tracks"), 
          function(x, by, FUN = mean, ..., simplify = TRUE, use.data = TRUE, 
                   weight.points = NULL, weight.tracks = NULL) {
            aggregateBy(x, by, FUN = FUN, simplify = simplify, use.data = use.data,
                        weight.points = weight.points, weight.tracks = weight.tracks, ...)
          })

# S3 method for S4 class - recommended in "R: General Information on methods".
aggregate.Tracks = function(x, by, FUN = mean, ..., simplify = TRUE, use.data = TRUE,
                           weight.points = NULL, weight.tracks = NULL) {
  aggregateBy(x, by, FUN = FUN, simplify = simplify, use.data = use.data, 
              weight.points = weight.points, weight.tracks = weight.tracks, ...)
}

# S4 method for S4 classes
setMethod("aggregateBy", signature(x = "Tracks", by = "STF"), 
          aggregate_Tracks_STF)



#################################################################



aggregate_TracksColl_STF <- function(x, by, FUN = mean, ..., simplify = TRUE, use.data = TRUE,
                                 weight.points = NULL, weight.tracks = NULL, byID = FALSE) {
  
  #identicalCRS testet in over!
  #"identicalTZ" as well!
  
  if (byID == FALSE) {
    
    # 'Quick-and-Dirty' approach which causes a little computation overhead
    # by creating an overall Tracks object, but on the other hand 
    # this approach ensures code consistency and avoids a lot of identical code lines.
    
    trackList <- unlist(lapply(x@tracksCollection, function (z) { 
      lapply(z@tracks, function(zz) { zz } ) }), recursive = FALSE, use.names = FALSE)
    
    tracksObj <- Tracks(trackList)
    
    aggregate(tracksObj, by, FUN, ..., simplify = simplify, use.data = use.data, 
              weight.points = weight.points, weight.tracks = weight.tracks)
    
  } else if (byID == TRUE) {
    
    if (!(is(by@sp, "SpatialPolygons") || is(by@sp, "SpatialPixels"))) {
      stop("If method aggregate is called with an STF object for argument by, the sp slot of by needs to be of class SpatialPolygons or SpatialPixels")
    }
    
    # Check for legal use.data argument
    stopifnot(isTRUE(use.data) || is.character(use.data) || is.numeric(use.data))
    
    FUN = match.fun(FUN)
    
    resDfList <- lapply(x@tracksCollection, function (y) { 
      
      trackList <- lapply(y@tracks, function(z) { z } )
      
      # Enable attribute selection even accepting attributes which 
      # are not present in the data slots of all Track objects.
      # (It is no requirement of trajectories objects that all data slots 'are equal'.)
      
      # Preparation
      dataNamesList <- lapply(trackList, function(z) names(z@data))
      
      allNames <- Reduce(union, dataNamesList)
      
      commonNames <- Reduce(intersect, dataNamesList)
      identicalCols <- identical(commonNames, allNames)
      
      # Occasional pre-selection (--> performance) and occasional addition of 
      # partly missing attributes both related to the use.data argument.
      if (is.character(use.data)) {
        if (!all(use.data %in% allNames)) {
          stop("Undefined columns requested: 
               One or more column names specified in use.data are not existent in any Track object.")
        } else if (!all(use.data %in% commonNames)) {
          # Adding attributes if missing and selection ...
          # Columns not identical but all requested column names are existent in at least one Track
          trackList <- lapply(trackList, function(z) { 
            for (i in seq_along(use.data)) {
              if (!use.data[i] %in% names(z@data)) {
      
                z@data[use.data[i]] <- NA
      
              }
            }
            # Pre-selection, even due to better performance
            z@data <- z@data[ , use.data, drop = FALSE]
            z
          })
        } else { # if (all(use.data %in% commonNames)) {
          # Just selection...
          trackList <- lapply(trackList, function(z) { 
            z@data <- z@data[ , use.data, drop = FALSE]
            z
          })
        }
        requestedNames <- use.data
        
      } else if (is.numeric(use.data)) {
        identicalColsAndOrder <- Reduce(identical, dataNamesList)
        if (identicalColsAndOrder) {
          trackList <- lapply(trackList, function(z) { 
            z@data <- z@data[ , use.data, drop = FALSE]
            z
          })
        } else {
          if (identicalCols) {
            warning("The data slots of the involved Track objects do not have an identical column order. In this case attribute selection by indices is not recommanded and needs to be applied with caution.")
            trackList <- lapply(trackList, function(z) { 
              z@data <- z@data[ , use.data, drop = FALSE]
              z
            })
          } else {
            warning("The data slots of the involved Track objects do not have identical columns. In this case attribute selection by indices is not recommanded and needs to be applied with caution, because after adding partly missing attributes an identical attribute order can not be ensured in any case which may lead to an error.")
            trackList <- lapply(trackList, function(z) { 
              if (length(allNames) > length(z@data)) {
                w <- which(!allNames %in% names(z@data))
                for (i in seq_along(w)) {
                  
                  z@data[allNames[w[i]]] <- NA
                  
                }
              }
              z@data <- z@data[ , use.data, drop = FALSE]
              z
            })
          }
        }
        requestedNames <- names(trackList[[1]]@data[use.data])
        
      } else {
        if (!identicalCols) {
          trackList <- lapply(trackList, function(z) { 
            if (length(allNames) > length(z@data)) {
              w <- which(!allNames %in% names(z@data))
              for (i in seq_along(w)) {
                
                z@data[allNames[w[i]]] <- NA
                
              }
            }
            z
          })
        }
        requestedNames <- allNames
      }
      
      over_res_list <- lapply(trackList, function(z) {
        
        over(by, z, returnList = FALSE, fn = FUN, ..., use.data = use.data,
             weight.points = weight.points)
        
      })
      
      # Calculate new metadata and then delete old from all over_res_list elements
      # New data: sum of length , duration relocations and track objects for each geometry
      mdNames <- c("nlocs", "approx_duration", "approx_distance")
      trackmetadata_list <- lapply(over_res_list, function(z) {
        z[ , mdNames]
      })
      
      metadata_Lists <- lapply(seq_along(by), function(z) { 
        lapply(trackmetadata_list, function(zz) zz[z,])
      })
      
      metadata_list <- lapply(metadata_Lists, function(z) do.call(rbind, z))
      metadata_list <- lapply(metadata_list, function(z) {
        z$ntraj <- 0
        z$ntraj[z$nlocs > 0] <- 1
        z
      })
      
      sumList <- lapply(metadata_list, function(z) colSums(z, na.rm = T))
      
      # New metadata
      if ("data" %in% slotNames(by)) {
        rNames <- row.names(by@data) } else { rNames <- NULL }
      
      metadata <- data.frame(do.call(rbind, sumList), row.names = rNames) 
            
      # # If values == 0, set to NA
      metadata[metadata$nlocs == 0, ] <- NA
      
      over_res_list <-lapply(over_res_list, function(x) { x[ , !names(x) %in% mdNames, drop = FALSE] })
      
      # Check if over_res_list[[1]] contains all requested attributes.
      # If TRUE, over_res_list[[1]] will be used as a basis for the result-df.
      if (identical(requestedNames, names(over_res_list[[1]]))) {
      
        # Supports providing the original data types
        res_df <- over_res_list[[1]]
        
      } else {
        
        res_df <- data.frame(matrix(vector(), nrow = length(by), ncol= length(requestedNames), 
                                    dimnames=list(c(), requestedNames)))
      }
      
      if (is.null(weight.tracks)) {
      
        for (i in seq_along(res_df)) {
          curName <- names(res_df[i])
          
          attrDataList <- lapply(over_res_list, function(z) {
          
            z[[curName]] })
          
          attrMatrix <- do.call(cbind, attrDataList)
                    
          #Achtung Auskommentierung !!!
          #attrAgg <- apply(attrMatrix, 1, function(z) {
          #  if (any(!is.na(z))) { FUN(z, na.rm = TRUE) } else { NA } }) 
          attrAgg <- apply(attrMatrix, 1, function(z) {
            if (any(!is.na(z))) { FUN(z, ...) } else { NA } }) 
                    
          res_df[ , curName] <- attrAgg
                    
        }
        
        res_df <- data.frame(res_df, metadata, row.names = rNames)
        
      } else { # !is.null(weight.tracks)
        
        weightsList <- switch(weight.tracks,
                              byTime  = lapply(seq_along(metadata_list), function(z) { 
                                metadata_list[[z]][["approx_duration"]] }),
                              byDist  = lapply(seq_along(metadata_list), function(z) { 
                                metadata_list[[z]][["approx_distance"]] }),
                              # Possibility to use equal weights to enable 'real'
                              # weighting JUST for points (weight.points).
                              equal   = lapply(metadata_list, function(z) { 
                                z[ , "ntraj"] }),
                              stop("Method aggregate: No adequate string is passed to the weight.tracks argument!"))
        
        
        for (i in seq_along(res_df)) {
          curName <- names(res_df[i])
        
          attrDataList <- lapply(over_res_list, function(z) {
            z[[curName]] })
          
          # Create matrix out of list
          attrMatrix <- do.call(cbind, attrDataList)
          
          attrAgg <- unlist(lapply(1:nrow(attrMatrix), function(z) {
            # avoid NaN
            if (any(!is.na(attrMatrix[z, ]))) {
              
              FUN(attrMatrix[z, ], weightsList[[z]], ...)
          
            } else {
              NA
            }
          }), use.names = FALSE)
          
          # Assign values to df
          res_df[ , curName] <- attrAgg
        }
      
      res_df <- data.frame(res_df, metadata, row.names = rNames)
      
      } # Finish weight.tracks != NULL
            
    }) # Finish creation resDfList
    
    # bind resDfList 
    res_df <- do.call(cbind,resDfList)
        
    if (simplify && length(by@sp) == 1) { 
      # Return time series of length == nrow(by@time) 
      # with aggregated values and by@time data (if existing).
      # The by@data (if existing) is ignored.
      xts(cbind(res_df, as.matrix(by@time)), index(by@time), 
          tzone = attr(by@time, "tzone"))
    } else if (simplify && nrow(by@time) == 1) { 
      # Return spatial object of length == length(by@sp)
      # with aggregated values and by@sp data (if existing).
      # The by@data (if existing) is ignored.
      if ("data" %in% slotNames(by@sp)) {
        res_df <- data.frame(res_df, by@sp@data, row.names = row.names(by@sp@data))
      }
      addAttrToGeom(geometry(by@sp), res_df, match.ID = FALSE)
    } else { # Return ST object: object of same class as the by-argument with added aggregated values
      if ("data" %in% slotNames(by)) {
        res_df <- data.frame(res_df, by@data, row.names = row.names(by@data))
      }
      addAttrToGeom(by, res_df, match.ID = FALSE)
    }
    
  } else {
    
    stop("Method aggregate with an object of class TracksCollection passed to the x argument: argument byID needs to be logical.")
    
  }
  
} # End of function
  

# S4 method for S4 class
#' @rdname aggregate
#' @aliases aggregate,TracksCollection
setMethod("aggregate", signature(x = "TracksCollection"), 
          function(x, by, FUN = mean, ..., simplify = TRUE, use.data = TRUE, 
                   weight.points = NULL, weight.tracks = NULL, byID = FALSE) {
            aggregateBy(x, by, FUN = FUN, simplify = simplify, use.data = use.data,
                        weight.points = weight.points, weight.tracks = weight.tracks, 
                        byID = byID, ...)
          })

# S3 method for S4 class - ...
aggregate.TracksCollection = function(x, by, FUN = mean, ..., simplify = TRUE, 
                                      use.data = TRUE, weight.points = NULL, 
                                      weight.tracks = NULL, byID = FALSE) {
  aggregateBy(x, by, FUN = FUN, simplify = simplify, use.data = use.data, 
              weight.points = weight.points, weight.tracks = weight.tracks, 
              byID = byID,...)
}

# S4 method for S4 classes
setMethod("aggregateBy", signature(x = "TracksCollection", by = "STF"), 
          aggregate_TracksColl_STF)
