#' @import methods sp spacetime trajectories
NULL

#' @importFrom rgeos gIntersects
NULL

#' @include agg_allGenerics.R over_sp_IgnDom.R
NULL

###############################################################

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


##############################################

#
# over.sp.Track
#

#x <- spPoly; y <- B1
over_sp_Track <- function(x, y, returnList = FALSE, fn = NULL, ...,
                           use.data = FALSE, weight.points = NULL) {
  
  # Get point indices matching spatial geometries of x (no respect of time)
  cleanIndexList_sp <- overIgnDom(x, y, returnList = TRUE, fn = NULL, use.data = FALSE)
  
  
  ######
  
  # determine which points intersecting spatial geoemtries
  
  # Get unique point indices of matching points
  uniqueSortedIndVec <- unique(sort(unlist(cleanIndexList_sp, use.names=F)))
  len <- length(uniqueSortedIndVec)
    
  # Create a list of complete vectors of indices for each spatial geometry
  # over all time instances (NA if not matching)
  completeIndexList_sp <- if (len > 0) {
    lapply(cleanIndexList_sp, function(z) {
      helpVec <- rep(NA, len)
      for (i in seq_along(z)) {
        w <- which(uniqueSortedIndVec == z[i])
        helpVec[w] <- z[i]
      }
      helpVec
    })
  } else { lapply(cleanIndexList_sp, function(z) { z <- NA; z }) }
    
  # Matrix with indices, one row for one spatial geometry,
  # columns representing machting points / time instances
  indByGeomMatrix <- do.call(rbind,completeIndexList_sp)
  
  cleanIndexVec <- as.vector(indByGeomMatrix)
  
  indexList <- as.list(cleanIndexVec)
  
  # Clean the indexList by setting NA to integer(0)
  cleanIndexList <- lapply(indexList, function(z) {
    if (is.na(z)) {
      z <- integer(0)
    } else {
      z
    }
  })  
  
  if (len == 0) {
    stopifnot(identical(cleanIndexList_sp, cleanIndexList))
  }
    
  ######
  
  if (use.data[1] == FALSE) {
    
    if (returnList) {
      cleanIndexList
    } else {
      cleanIndexVec
    }
    
  } else { # use.data[1] != FALSE
    
    if (returnList) {
      
      dfList <- lapply(cleanIndexList, function(z) {
        # It's (theoretically) possible to have a legal Track object without columns in the data slot
        if (length(y@data) > 0) {
          if (length(z) > 0) {
            
            df <- y@data[z, use.data, drop = FALSE]
            
          } else {
            
            df <- y@data[FALSE, use.data, drop = FALSE]
            
          }
        } else { # length(y@data) == 0
          
          df <- data.frame()
          df
        }
      })
      
      dfList
      
    } else { # retuenList == FALSE, use.data = T | selection
            
      if (is.null(fn)) {
        
        if (length(y@data) > 0) {
      
          df <- data.frame(y@data[cleanIndexVec, use.data, drop=FALSE], row.names = NULL)  
          
        } else {
          
          df <- data.frame(y@data[cleanIndexVec, FALSE, drop=FALSE], row.names = NULL)
          
        }
        
        # time added additionally 
        df$time <- index(y@time)[cleanIndexVec]
        df$timeIndex <- as.matrix(y@time)[cleanIndexVec]
        df
                
      } else { # !is.null(fn)
        
        fn = match.fun(fn)
        
        # warning in case of fn == sum
        if (paste(deparse(fn), collapse="") == paste(deparse(sum),collapse="")) {
          warning("Aggregation of trajectory data using 'sum' as aggregation function is not meaningful.")
        }
        
        ###
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
        
        # metadata # duration per spatio-temporal geometry
        approxDistPerCell <- do.call(rbind, lapply(approxPointsInCellsDistList, sum))
        
        metadata <- data.frame(nlocs = nlocs, approx_duration = approxDurationPerCell,
                               approx_distance = approxDistPerCell)
      
        if (length(y@data) > 0) {
          
          if (!is.null(weight.points)) { # apply fn to data without weighting

            warning(paste("weight.points = ", weight.points, ", but no weighting is performed, because each spatio-temporal geometry just contains at most one Track point.", sep = ""))
            
          } 
          #} else { # !is.null(weight.points)
          #  # ...
          #}
          
          df <- data.frame(y@data[cleanIndexVec, use.data, drop=FALSE], row.names = NULL)
          
          data.frame(df, metadata)
          
        } else { # length(y@data) == 0
          
          warning(paste("Method over was called with use.data = ", use.data, " and fn != NULL, but the data slot of the Track object does not contain any attributes. A data.frame just containing metadata is returned.", sep = ""))
          
          data.frame(y@data[seq_along(cleanIndexList), FALSE, drop = FALSE], metadata)
          
        }
      }
    }
  }  
}

#' @rdname over
#' @aliases over,SpatialPolygons,Track
setMethod("over", signature(x = "SpatialPolygons", y = "Track"), over_sp_Track)

#' @rdname over
#' @aliases over,SpatialPixels,Track
setMethod("over", signature(x = "SpatialPixels", y = "Track"), over_sp_Track)

#' @rdname over
#' @aliases over,SpatialGrid,Track
setMethod("over", signature(x = "SpatialGrid", y = "Track"), over_sp_Track)


#######################################################################


# # AN old / alternative approach!!!
# over_sp_Track_alternativ <- function(x, y, returnList = FALSE, fn = NULL, ...,
#                                      use.data = FALSE, weight.points = NULL) {
#   
#   #stopifnot(identicalCRS(x,y))
#   #if (gridded(x)) {
#   #  x <- as(x, "SpatialPolygons")
#   #}
#   #gI_matrix <- rgeos::gIntersects(y@sp, x, byid = TRUE)  
#   ## determine which point sare matching...
#   #if (length(x) == 1) {
#   #  # Special case: sp object with just one spatial instance
#   #  #help <- apply(gI_matrix, 1, function(z) which(z == TRUE))
#   #  indexList <- list(apply(gI_matrix, 1, function(z) which(z == TRUE))[ , 1])
#   #} else {
#   #  indexList <- apply(gI_matrix, 1, function(z) which(z == TRUE))
#   #  names(indexList) <- NULL
#   #}
#   ##indexL <- apply(gI_matrix, 1, function(zz) which(zz == TRUE))
#   #cleanIndexList_sp2 <- lapply(indexList, function(z) { attr(z, "names") <- NULL; z })
#   
#   # Get point indices matching spatial geometries of x (no respect of time)
#   cleanIndexList_sp <- overIgnDom(x, y, returnList=T, fn = NULL, use.data = FALSE)
#   
#   # delete
#   #identical(cleanIndexList_sp, cleanIndexList_sp2)
#   
#   ######
#   
#   # ... (determine which points intersecting spatial geoemtries)
#   #z <- cleanIndexList_sp[[1]]
#   #zz <- z[[1]]
#   # i <- 1
#   
#   # Get unique point indices of matching points
#   uniqueSortedIndVec <- unique(sort(unlist(cleanIndexList_sp, use.names=F)))
#   len <- length(uniqueSortedIndVec)
#   
#   # Create a list of complete vectors of indices for each spatial geometry
#   # over all time instances (NA if not matching)
#   completeIndexList_sp <- lapply(cleanIndexList_sp, function(z) {
#     helpVec <- rep(NA, len)
#     for (i in seq_along(z)) {
#       w <- which(uniqueSortedIndVec == z[i])
#       helpVec[w] <- z[i]
#     }
#     helpVec
#   })
#   
#   # Matrix with indices, one row for one spatial geometry,
#   # columns representing machting points / time instances
#   indByGeomMatrix <- do.call(rbind,completeIndexList_sp)
#   
#   # TODO !!! ??? das evtlnoch oben einbinden...
#   cleanIndexVec <- as.vector(indByGeomMatrix)
#     
#   ###cleanIndexVec <- unlist(completeIndexList_sp, use.names = FALSE)
#   
#   # !!!
#   indexList <- as.list(cleanIndexVec)
#   
#   #z <- indexList[[4]]
#   #z <- indexList[[84]]
#   
#   # Clean the indexList by setting NA to integer(0)
#   cleanIndexList <- lapply(indexList, function(z) {
#     #z <- ifelse(is.na(z), z <- integer(0), z)
#     if (is.na(z)) {
#       z <- integer(0)
#     } else {
#       z
#     }
#   })
#   
#   
#   if (use.data[1] == FALSE) {
#     if (returnList) {
#       cleanIndexList
#     } else {
#       cleanIndexVec
#     }
#   } else { # use.data[1] != FALSE
#     if (returnList) {
#       
#       dfList <- lapply(cleanIndexList, function(z) {
#         # It's (theoretically) possible to have a legal Track object without columns in the data slot
#         if (length(y@data) > 0) {
#           if (length(z) > 0) {
#             
#             df <- y@data[z, use.data, drop = FALSE]
#             
#           } else {
#             
#             df <- y@data[FALSE, use.data, drop = FALSE]
#             
#           }
#         } else { # length(y@data) == 0
#           
#           df <- data.frame()
#           
#           # delete todo --> scheint ok zu sein !!! delete message !!! ??? !!!
#           message("check if that is meaningful / useful..!?!")
#           #stop("Not completely implemeted yet")
#           # ... check Todo if sinnvoll!
#           
#           df
#         }
#       })
#       
#       dfList
#       
#     } else { # retuenList == FALSE, use.data = T | selection
#       
#       # FAST kein Unterschied zw. fn = NULL und fn != Null,
#       # da jede spatio-temporal geometry max nur 1 point enthält!?!
#       # --> fn wird nicht angewendet!
#       
#       # --> wie implementieren / Berücksichtigen ???
#       
#       # --> einzige Unterschiede:
#       # - is NULL == fn: evtl time dazu und keine metadaten
#       
#       if ("data" %in% slotNames(x)) {
#         rNames <- row.names(x@data) } else { rNames <- NULL }
#       
#       if (is.null(fn)) {
# 
#         if (length(y@data) > 0) {
#           df <- data.frame(y@data[cleanIndexVec, use.data, drop=FALSE], row.names = rNames)  
#         } else {
#           df <- data.frame(y@data[cleanIndexVec, FALSE, drop=FALSE], row.names = rNames)
#         }
#         # !!! time added additionally !!!
#         df$time <- index(y@time)[cleanIndexVec]
#         df$timeIndex <- as.matrix(y@time)[cleanIndexVec]
#         df
#         
#         #delete
#         #stop("Not implemeted yet")
#         
#       } else { # !is.null(fn)
#         
#         ###
#         # number of relocations
#         nlocsList <- lapply(cleanIndexList, function(z) length(z))
#         nlocs <- do.call(rbind, nlocsList)
#         
#         ###
#         # approximate duration
#         halfConnDuration <- lapply(1:(length(y@time) - 1), function(z) {
#           y@connections[z, "duration"] / 2 })
#         durMatrix <- matrix(data = as.numeric(c(c(0, halfConnDuration), c(halfConnDuration, 0))),
#                             nrow = length(y), ncol = 2)
#         approxPointDuration <- .rowSums(durMatrix, length(y), 2, na.rm = TRUE)
#         # leere Zellen: besser NA oder 0 ???
#         # !!! Duration of each Point separated by cells / polygons
#         approxPointsInCellsDurList <- lapply(cleanIndexList, function(z) {
#           if (length(z) == 0) { NA } else { approxPointDuration[z] } })
#         #approxPointsInCellsDurSumList <- lapply(approxPointsInCellsDurList, sum)
#         # !!! -> metadata # duration per spatial unit
#         approxDurationPerCell <- do.call(rbind, lapply(approxPointsInCellsDurList, sum))
#         
#         
#         ###
#         # approximate length
#         halfConnDist <- lapply(1:(length(y@time) - 1), function(z) {
#           round(y@connections[z, "distance"] / 2, 2) })
#         distMatrix <- matrix(data = as.numeric(c(c(0, halfConnDist), c(halfConnDist, 0))),
#                              nrow = length(y), ncol = 2)
#         approxPointDist <- .rowSums(distMatrix, length(y), 2, na.rm = TRUE)
#         # leere Zellen: besser NA oder 0 ???
#         # !!! Duration of each Point separated by cells / polygons
#         approxPointsInCellsDistList <- lapply(cleanIndexList, function(z) {
#           if (length(z) == 0) { NA } else { approxPointDist[z] } })
#         #approxPointsInCellsDistSumList <- lapply(approxPointsInCellsDistList, sum)
#         # !!! -> metadata # duration per spatio-temporal geometry
#         approxDistPerCell <- do.call(rbind, lapply(approxPointsInCellsDistList, sum))
#         
#         
#         # metadata data.frame
#         #metadata <- data.frame(nlocs = nlocs, approx_duration = approxDurationPerCell,
#         #                       approx_distance = approxDistPerCell, row.names = rNames)
#         #row.names?? : x --> NULL, x@sp --> nur spatial, ...?
#         #approx_distance = approxDistPerCell)#, row.names = row.names(x))
#         metadata <- data.frame(nlocs = nlocs, approx_duration = approxDurationPerCell,
#                                approx_distance = approxDistPerCell)
#         
#         
#         if (length(y@data) > 0) {
#           
#           # kein Unterschied zw. is.null(weight.points) und !is.null(weight.points),
#           # da jede spatio-temporal geometry max nur 1 point enthält!?!
#           # --> fn wird nicht angewendet! --> auch kein weighting !?!
#           
#           if (!is.null(weight.points)) { # apply fn to data without weighting
#             # ...
#             # ???
#             warning("Method over with an object inherting from class Spatial passed to the x argument and point.weights != NULL: No weighting will be performed, because each spatio-temporal geometry just contains at most one track point.")
#           } 
#           #} else { # !is.null(weight.points)
#           #  # ...
#           #}
#           
#           df <- data.frame(y@data[cleanIndexVec, use.data, drop=FALSE], row.names = rNames)
#           
#           data.frame(df, metadata, row.names = rNames)
#           
#           # (...) fertig !?
#         
#         } else { # length(y@data) == 0
# 
#           # Waas tun wenn keine Daten vorhanden???
#           
#           warning(paste("Method over was called with use.data = ", use.data, " and fn != NULL, but the data slot of the Track object does not contain any attributes. A data.frame just containing metadata is returned.", sep = ""))
#           
#           data.frame(y@data[seq_along(cleanIndexList), FALSE, drop = FALSE],
#                      metadata, row.names = rNames)
#           
#         }
#       }
#     }
#   }
# 
#   # MUELL
#   
#   ## hier evtl nochmal / schon unterscheiden rl= T vs rl = F
#   #sortedIndVec <- sort(unlist(indexL, use.names=F))
#   #gI_sub <- gI_matrix[ , sortedIndVec]
#   #vec_sub <- as.vector(gI_sub)
#   ## immer ???
#   #stopifnot(length(vec_sub) == (length(x) * length(sortedIndVec)))
#   #w_sub <- which(vec_sub)
#   #vec_sub_num <- as.numeric(vec_sub)
#   #vec_sub_num[w_sub] <- sortedIndVec
#   #vec_sub_num[vec_sub_num == 0] <- NA
#   #vec_sub_num
#   ### 
#   
#   ## Passt nicht , da alle Points berücksichtigt werden, und nichtnur die matchen..!
#   #vec <- as.vector(gI_matrix)
#   #w <- which(vec)
#   #trackIndex <- seq_along(y)
#   #stopifnot(length(vec_sub) == (length(x) * length(sortedIndVec)))
#   #numVec <- as.numeric(vec)
#   ## assign Track indices
#   #numVec[w] <- trackIndex
#   ##numVec[w] <- trackIndex[w]
#   #numVec[numVec == 0] <- NA
#   #numVec
#   
#   #!!!
#   #correct_res_for_matching_all_points <- numVec
#   
#   #indexList <- apply(gI_matrix, 1, function(z) which(z == TRUE))
#   #names(indexList) <- NULL
#   
# }