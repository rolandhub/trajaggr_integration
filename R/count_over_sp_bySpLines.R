# Functions to count the number of 'trajectories' objects over objects of 
# class 'SpatialGrid' or 'SpatialPolygons'. The trajectories objects
# are coerced to 'SpatialLines' and hence the presence of trajectory
# objects related to the geometries of the 'Spatial' object is based
# on the intersection of the trajectories' segments with these geometries.
# These functions are not exported.

####################
# Track
####################

# count trajectories over space, x = Track, y = SpatialGrid or SpatialPolygons
count_traj_over_space_bySpL.Track <- function(x, y) {
  stopifnot(identicalCRS(x,y))
  spLinesDf <- SpatialLinesDataFrame(
    as(x, "SpatialLines"), data = data.frame(count = 1), match.ID = FALSE)
  countDf <- over(y, spLinesDf, returnList = F)
  countDf[is.na(countDf$count), ] <- 0
  ret <- addAttrToGeom(y, countDf, match.ID = FALSE)
}


# # @rdname countBySpL
# # @aliases Track,SpatialPolygons
# setMethod("countBySpL", signature(x = "Track", y = "SpatialPolygons"),
#           count_traj_over_space_bySpL.Track)
# 
# # @rdname countBySpL
# # @aliases Track,SpatialGrid
# setMethod("countBySpL", signature(x = "Track", y = "SpatialGrid"),
#           count_traj_over_space_bySpL.Track)
# 
# # SpatialPixels does not work!

####################


####################
# Tracks
####################

# count trajectories over space, x = Tracks, y = SpatialGrid or SpatialPolygon
count_traj_over_space_bySpL.Tracks <- function(x, y) {
  stopifnot(identicalCRS(x,y))
  spLines <- as(x, "SpatialLines")
  spLinesDf <- SpatialLinesDataFrame(
    spLines, data = data.frame(count = rep(1, length(spLines))), match.ID = FALSE)
  countDf <- over(y, spLinesDf, returnList = F, fn = sum)
  countDf[is.na(countDf$count), ] <- 0
  ret <- addAttrToGeom(y, countDf, match.ID = FALSE)
}

####################


####################
# TracksCollection
####################
  
# count trajectories over space, x = TracksCollection, y = SpatialGrid or SpatialPolygon
count_traj_over_space_bySpL.TracksCollection <- function(x, y) {
  stopifnot(identicalCRS(x,y))
  spLines <- do.call(rbind, c(lapply(x@tracksCollection, function(z) as(z, "SpatialLines")),
                     makeUniqueIDs = TRUE)) # ???
  spLinesDf <- SpatialLinesDataFrame(
    spLines, data = data.frame(count = rep(1, length(spLines))), match.ID = FALSE)
  countDf <- over(y, spLinesDf, returnList = F, fn = sum)
  countDf[is.na(countDf$count), ] <- 0
  ret <- addAttrToGeom(y, countDf, match.ID = FALSE)
}

####################