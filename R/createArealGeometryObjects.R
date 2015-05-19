
#' Function to create objects inheriting from class \code{Spatial} with an area-measured character
#' @name createSpatialArealObjFromPoints
#' @description Here comes the description .... TODO
#' @param x an object of class \code{SpatialPoints} or \code{SpatialPointsDataFrame}
#' @param desDim integer indicating the desired dimension of the return object along 
#' the larger side of the bbox of the input object \code{x}
#' @param out the desired class of the return object
#' @return an object of class \code{SpatialPolygons} (default), \code{SpatialPixels} or
#' \code{SpatialGrid} as defined by the \code{out} argument
#' @examples
#' ## load data
#' ## Load example toy data
#' load(system.file("extdata","trajaggr_TestData.RData", package = "trajaggr"), verbose = FALSE)
#' require(sp)
#' # Create SpatialPolygons covering points of a TracksCollection
#' points <- as(TrColl, "SpatialPointsDataFrame")
#' spPoly <- createSpatialArealObjFromPoints(points)
#' class(spPoly)
#' # Create SpatialPixels
#' spPix <- createSpatialArealObjFromPoints(points, desDim = 8, out = "SpatialPixels")
#' class(spPix)
#' spPix
#' # Create a SpatiaGrid
#' pointsB1 <- as(Track_B1, "SpatialPointsDataFrame")
#' spGrid_B1 <- createSpatialArealObjFromPoints(pointsB1, desDim = 9, out = "SpatialGrid")
#' class(spGrid_B1)
#' slot(spGrid_B1, "grid")
#' @export
createSpatialArealObjFromPoints <- function(x, desDim = 10, out = "SpatialPolygons") {
  
  # Preparation
  extents <- c(diff(x@bbox[1,]), diff(x@bbox[2,]))
  max_ext <- max(extents)
  largeSideIndex <- which(extents == max_ext)
  if (length(largeSideIndex) > 1) {largeSideIndex <- largeSideIndex[1]}
  csize <- extents[largeSideIndex]/desDim
  dimX <- ceiling(extents[1]/csize) #+ 1
  dimY <- ceiling(extents[2]/csize) #+ 1
  
  # Create grid topolgy
  csize <- csize + csize/desDim
  gt <- GridTopology(x@bbox[ , 1], c(csize,csize), c(dimX, dimY))
  ret <- SpatialGrid(gt, proj4string = x@proj4string)
  # Create Output
  if (!is(ret, out)) {
    ret <- as(ret, out)
  }
  ret
}
