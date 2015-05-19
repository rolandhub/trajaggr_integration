
#===============================================================================
#
# Coercion from Track to ltraj
#
#===============================================================================


setAs("Track", "ltraj", function(from) {

  xy <- data.frame(from@sp@coords)
  
  date <- zoo::index(from@time)
  
  id <- "id"
  burst <- "burst"
  
  # if data / "infolocs" in Track object...
  if (length(from@data) >= 1) {
    
    namesData <- names(from@data)
    varToDrop <- namesData %in% c("x", "y", "date", "dx", "dy", "dist", "dt", "R2n", "abs.angle", "rel.angle")

    il <- from@data[!varToDrop]
    namesInfolocs <- namesData[!varToDrop]
    names(il) <- namesInfolocs    
    
    new_ltraj <- adehabitatLT::as.ltraj(xy, date, id, burst, infolocs = il)
    
    attributes(new_ltraj[[1]])$row.names <- attr(from@data, "row.names")
    
    attr(attributes(new_ltraj[[1]])$infolocs, "row.names") <- row.names(from@data)
    
  } else {
    
    # no data / 'infolocs'
    new_ltraj <- adehabitatLT::as.ltraj(xy, date, id, burst, infolocs = NULL)
    
    attributes(new_ltraj[[1]])$row.names <- attr(from@data, "row.names")
    
  }
  
  new_ltraj
  
})



#===============================================================================
#
# Coercion from Tracks to ltraj
#
#===============================================================================


setAs("Tracks", "ltraj", function(from) {
  
  burstNames <- names(from@tracks)

  if (length(burstNames) != length(unique(burstNames))) {
    burstNames <- paste(burstNames, 1:length(burstNames), sep = "_")
  }
  
  # Track data slots need to be compared because they will be saved in the ltraj object
  # as the attribute infolocs (type data.frame) which is restricted to have the 
  # same attributes in all infolocs data.frame objects.
  # First the column names need to be extracted and compared...
  colNamesList <- lapply(from@tracks, function(x) names(x@data))
  maxColCount <- max(sapply(colNamesList, function(x) length(x)))
  commonNames <- Reduce(intersect, colNamesList)
  
  # If the column names are not identical that need to be fixed...
  # (It is assumed that identical column names imply identical attributes.)
  if (maxColCount > length(commonNames)) {
  
    if (!requireNamespace("plyr", quietly = TRUE)) {
  
      stop("Package plyr (function plyr::rbind.fill) is required to coerce 
            from Tracks objects to ltraj objects, if the Tracks object has different 
            attributes in the data slots of the corresponding Track objects.")
      
    } else {
      
      dataFrameList <- lapply(from@tracks, function(x) x@data)
      tmpDataFrame <- do.call(plyr::rbind.fill, dataFrameList)
      
      start_ind <- 1
      end_ind <- 0
      
      for (i in seq_along(from@tracks)) {
        
        nrow_cur <- nrow(dataFrameList[[i]])
        end_ind <- end_ind + nrow_cur
        
        from@tracks[[i]]@data <- tmpDataFrame[c(start_ind:end_ind), , drop = FALSE]
        attr(from@tracks[[i]]@data, "row.names") <- attr(dataFrameList[[i]], "row.names")
        
        # Adjusting the levels of data of type factor...
        for (j in seq_along(from@tracks[[i]]@data)) {
          
          if (is.factor(from@tracks[[i]]@data[[j]])) {
        
            attrName <- names(from@tracks[[i]]@data[j])
        
            from@tracks[[i]]@data[[j]] <- 
              factor(from@tracks[[i]]@data[[j]],
                     levels = levels(dataFrameList[[i]][[attrName]]))
          }
        }
        
        start_ind <- start_ind + nrow_cur 
        
      }
    }
  }
  
  # Create list of ltraj objects
  ltrajList <- lapply(from@tracks, function(x) as(x, "ltraj"))
  
  names(ltrajList) <- NULL
  
  for (i in seq_along(ltrajList)) {  
    adehabitatLT::burst(ltrajList[[i]]) <- burstNames[[i]]
  }
  
  # Create one overall ltraj object
  do.call(adehabitatLT::c.ltraj, ltrajList)
  
})



#===============================================================================
#
# Coercion from TracksCollection to ltraj
#
#===============================================================================


setAs("TracksCollection", "ltraj", function(from) {
  
  # ids vector
  ids <- names(from@tracksCollection)
  
  # Track data slots need to be compared because they will be saved in the ltraj object
  # as the attribute infolocs (type data.frame) which is restricted to have the 
  # same attributes in all infolocs data.frame objects.
  # First the column names need to be extracted and compared...
  colNamesList <- vector("list", sum(from@tracksCollectionData$n))
  counter <- 0
  for (i in seq_along(from@tracksCollection)) {
    for (j in seq_along(from@tracksCollection[[i]]@tracks)) {
      counter <- counter + 1
      colNamesList[[counter]] <- names(from@tracksCollection[[i]]@tracks[[j]]@data)
    }
  }
  
  maxColCount <- max(sapply(colNamesList, function(x) length(x)))
  commonNames <- Reduce(intersect, colNamesList)
  
  # If the column names are not identical that issue need to be fixed...
  # (It is assumed that identical column names imply identical attributes.)
  if (maxColCount > length(commonNames)) {
  
    if (!requireNamespace("plyr", quietly = TRUE)) {
  
      stop("Package plyr (function plyr::rbind.fill) is required to coerce 
            from TracksCollection objects to ltraj objects, if the Tracks objects has different 
            attributes in the @data slots of the appendent Track objects.")
      
    } else {
      
      dataFrameList <- vector("list", length(colNamesList))
      
      counter <- 0
      for (i in seq_along(from@tracksCollection)) {
        for (j in seq_along(from@tracksCollection[[i]]@tracks)) {
          counter <- counter + 1
          dataFrameList[[counter]] <- from@tracksCollection[[i]]@tracks[[j]]@data
        }
      }
      
      # All data.frame objects will be extended by adding missing columns 
      # filled with NA values...
      tmpDataFrame <- do.call(plyr::rbind.fill, dataFrameList)
      
      start_ind <- 1
      end_ind <- 0
      counter <- 0
      for (h in seq_along(from@tracksCollection)) {
      
        for (i in seq_along(from@tracksCollection[[h]]@tracks)) {
          counter <- counter + 1
          nrow_cur <- nrow(dataFrameList[[counter]])
          end_ind <- end_ind + nrow_cur
      
          from@tracksCollection[[h]]@tracks[[i]]@data <- 
            tmpDataFrame[c(start_ind:end_ind), , drop = FALSE]
          
          attr(from@tracksCollection[[h]]@tracks[[i]]@data, "row.names") <- 
            attr(dataFrameList[[counter]], "row.names")
      
          # Adjusting the levels of data of type factor...
          for (j in seq_along(from@tracksCollection[[h]]@tracks[[i]]@data)) {
            if (is.factor(from@tracksCollection[[h]]@tracks[[i]]@data[[j]])) {
              attrName <- names(from@tracksCollection[[h]]@tracks[[i]]@data[j])
              from@tracksCollection[[h]]@tracks[[i]]@data[[j]] <- 
                factor(from@tracksCollection[[h]]@tracks[[i]]@data[[j]],
                       levels = levels(dataFrameList[[counter]][[attrName]]))
            }
          }
          
          start_ind <- start_ind + nrow_cur 
          
        }        
      }
    }
  }
  
  # Create an ltraj object out of each Tracks object from the TracksCollection
  ltrajList <- lapply(from@tracksCollection, function(x) { as(x, "ltraj") })
  
  # Delete names
  names(ltrajList) <- NULL

  if (length(ltrajList) != length(ids)) { stop("Stop: length(ltrajList) != length(ids)") }
  
  # The id needs to be set ...
  for (i in seq_along(ltrajList)) {
    adehabitatLT::id(ltrajList[[i]]) <- rep(ids[i], length(ltrajList[[i]]))
  }  
  
  # Burst names - already set - but ...
  burstNames <- unlist(lapply(ltrajList, function(x) {
    lapply(x, function(X) attr(X, "burst"))
  }))
  # ... need to be adjusted if they are not unique ...
  if (length(burstNames) != length(unique(burstNames))) {    
    for (j in seq_along(ltrajList)) {
      for (i in seq_along(ltrajList[[j]])) {
        newBurstName <- paste(attr(ltrajList[[j]][[i]], "id"),
                              attr(ltrajList[[j]][[i]], "burst"), sep = "_")
        attr(ltrajList[[j]][[i]], "burst") <- newBurstName
      }
    }
  }
  
  # Create overall ltraj object
  do.call(adehabitatLT::c.ltraj, ltrajList)
  
})
