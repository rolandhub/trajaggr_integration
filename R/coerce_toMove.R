
#' @import methods sp spacetime xts trajectories move
NULL


# Internal function to create Move objects out of 
# Track objects with possibility for passing a name argument
.createNamedMoveObject <- function(from, name = "unnamed") {
  
  x <- from@sp@coords[ , 1]
  
  y <- from@sp@coords[ , 2]

  time <- as.POSIXct(zoo::index(from@time))
  
  data <- slot(from, "data")
  
  proj <- from@sp@proj4string
  
  m <- move::move(x, y, time, data, proj, animal = name)
  
  dimnames(m@coords) <- dimnames(from@sp@coords)
  dimnames(m@bbox) <- dimnames(from@sp@bbox)
    
  # 'Hack' to avoid loosing data from the data slot,
  # because move::move may shift variables to idData, if they are unique.
  # Track data slot is intended to hold data related to relocations.
  # Even if this might have the same value for the whole track,
  # it is not intended to be (just) idData.
  namesD <- names(data)
  namesMD <- names(m@data)
  if (any(!(namesD %in% namesMD))) {
    
    # add dropped columns to m@data again with keeping the column order
    whichMoveSpecCols <- which(!(namesMD %in% namesD))
    
    moveSpecCols_Colnames <- namesMD[whichMoveSpecCols]
    m@data <- cbind(data, m@data[moveSpecCols_Colnames])
    
    # delete the added columns from m@idData
    drop <- names(m@idData[which(names(m@idData) %in% namesD)])
    m@idData <- m@idData[!names(m@idData) %in% drop]
    
  }
  m
}


##############################################################

setAs("Track", "Move", function(from) {
  
  # Create move object using internal function
  m <- .createNamedMoveObject(from)
  
})


#############################################################

setAs("Tracks", "MoveStack", function(from) {

  trackNames <- names(slot(from, "tracks"))
  l <- as.list(trackNames)

  for (i in seq_along(l)) {
    l[[i]] <- .createNamedMoveObject(slot(from, "tracks")[[i]], name = trackNames[[i]])
    move::idData(l[[i]]) <- cbind(move::idData(l[[i]]), slot(from, "tracksData")[i, ])
  }

  moveSt <- moveStack(l)

  # However the timezone in the timestamps slot is dropped
  # when applying move::moveStack. Need to redefine the timezone...
  attr(moveSt@timestamps, "tzone") <- attr(moveSt@data$timestamp, "tzone")
  
  moveSt
  
})



##############################################################

setAs("TracksCollection", "MoveStack", function(from) {
  
  # named list
  trackNamesList <- lapply(slot(from, "tracksCollection"), function(x) {
    names(slot(x, "tracks")) })

  # Create a list of move objects
  moveList <- lapply(slot(from, "tracksCollection"), function(x) {
    l <- as.list(names(slot(x, "tracks")))
    for (i in seq_along(l)) {
      l[[i]] <- .createNamedMoveObject(slot(x, "tracks")[[i]], name = l[[i]])
      move::idData(l[[i]]) <- cbind(data.frame(move::idData(l[[i]])), slot(x, "tracksData")[i, ])
    }
    return(l)
  })
  
  lst <- list()
  counter <- 1
  
  for (j in seq_along(moveList)) {
    ml <- moveList[[j]]
    for (i in seq_along(ml)) {
      lst[[counter]] <- ml[[i]]
      counter <- counter + 1
    }
  }
  
  moveSt <- move::moveStack(lst)
  
  # However the timezone in the timestamps slot is dropped
  # when applying move::moveStack. Need to redefine the timezone...
  attr(moveSt@timestamps, "tzone") <- attr(moveSt@data$timestamp, "tzone")
  
  moveSt
})


###############################################################

setAs("Tracks", "MoveBurst", function(from) {
  
  # Simple case if Tracks object just has one Track
  if (length(slot(from, "tracks")) == 1) {
    
    m <- as(slot(from, "tracks")[[1]], "Move")
    v <- rep(names(slot(from, "tracks")), from@tracksData$n - 1)
    mb <- move::burst(m, v)
    
  } else {
    
    # Declaration and definition of character vector for burstIds
    v <- character(sum(from@tracksData$n) - 1)
        
    # Names of the Track objects will be used as burstIds
    trNames <- names(slot(from, "tracks")) 
    
    # Creation of the vector that is used to 'burst' the move object
    counter <- 1
    for (i in seq_along(trNames)) {
      if (i < length(trNames)) {
        v[counter:(counter + slot(from, "tracksData")$n[i] - 2)] <- trNames[i]
        # Two Track objects are by a connection with the burstId = undefined
        v[counter + slot(from, "tracksData")$n[i] - 1] <- "undefined"
        counter <- counter + slot(from, "tracksData")$n[i]
      } else {
        v[counter:(counter + slot(from, "tracksData")$n[i] - 2)] <- trNames[i]
      }
    }
    
    # check if data columns equal
    dataList <- lapply(slot(from, "tracks"), function(x) x@data)
    dataNamesList <- lapply(dataList, function(x) names(x))
    maxColCount <- max(sapply(dataNamesList, function(x) length(x)))
    commonNames <- Reduce(intersect, dataNamesList)
    
    # All Tracks have same data.frame columns
    if (maxColCount == length(commonNames)) {
      
      stidfList <- lapply(slot(from, "tracks"), function(x) as(x, "STIDF"))
      
      # Check chronological order of Track objects - requirement for MoveBurst objects!
      timeFirstIndexVec <- unlist(lapply(stidfList, function(x) {index(x@time[1])}),
                                   recursive = FALSE, use.names = FALSE)
      stopifnot(xts::isOrdered(timeFirstIndexVec))
      
      stidf <- do.call(rbind, stidfList)
      
      # After calling rbind the time zone [and maybe row.names (?)] need to be set again
      xts::indexTZ(stidf@time) <- xts::indexTZ(stidfList[[1]]@time)
      #rowNames <- unlist(lapply(slot(from, "tracks"), function(x) attr(x@data, "row.names")), 
      #                   use.names = FALSE, recursive = FALSE)
      #attr(stidf@data, "row.names") <- rowNames
      
      # Check for duplicated timestamps / relocations
      # (may be the case if the Tracks object was created out of a MoveBurst,
      # what on the other hand guarantees that the data slots are 'well structured'.)
      if (!xts::isOrdered(zoo::index(stidf@time), strictly = TRUE)) {
        # Exclude the duplicated instances from stidf ...
        dt <- diff(zoo::index(stidf@time))
        w_dt0 <- which(dt == 0)
        stidf <- stidf[-w_dt0]
        # ... and set a new timeIndex ...
        stidf@time$timeIndex <- 1:length(stidf)
        # ... and adjust the bursts vector.
        v <- v[-w_dt0]
      }
      
      m <- .createNamedMoveObject(stidf)
      
      mb <- move::burst(m, v)
      
    } else {
      
      if (!requireNamespace("plyr", quietly = TRUE)) {
        stop("Package plyr (function plyr::rbind.fill) is required to coerce 
              from Tracks objects to MoveBurst objects, if the Tracks object has different 
              attributes in the @data slots of the corresponding Track objects.")
      }
      
      data <- do.call(plyr::rbind.fill, dataList)
      
      l <- slot(from, "tracks")
      
      xCoordsList <- lapply(l, function(x) x@sp@coords[ , 1])
      yCoordsList <- lapply(l, function(x) x@sp@coords[ , 2])
      timeList <- lapply(l, function(x) as.POSIXct(zoo::index(x@time)))
      
      # Check chronological order of Track objects
      timeFirstIndexVec <- unlist(lapply(timeList, function(x) {x[1]}),
                                  recursive = FALSE, use.names = FALSE)
      stopifnot(xts::isOrdered(timeFirstIndexVec))
      
      proj <- l[[1]]@sp@proj4string
      
      x <- do.call(c, xCoordsList)
      y <- do.call(c, yCoordsList)
      time <- do.call(c, timeList)
      
      m <- move::move(x, y, time, data, proj)
      
      dimnames(m@coords) <- dimnames(from@tracks[[1]]@sp@coords)
      dimnames(m@bbox) <- dimnames(from@tracks[[1]]@sp@bbox)
      
      # 'Hack' to adjust the data slot of the move object to 
      # be consistent with the Track object.
      namesD <- names(data)
      namesMD <- names(m@data)
      if (any(!(namesD %in% namesMD))) {
        # add dropped columns to m@data again with keeping the column order
        whichMoveSpecCols <- which(!(namesMD %in% namesD))
        moveSpecCols_Colnames <- namesMD[whichMoveSpecCols]
        m@data <- cbind(data, m@data[moveSpecCols_Colnames])
        # delete the added columns from m@idData
        drop <- names(m@idData[which(names(m@idData) %in% namesD)])
        m@idData <- m@idData[!names(m@idData) %in% drop]
      }
      
      # 'Burst' the move object
      mb <- move::burst(m, v)
      
      }
    }
})
