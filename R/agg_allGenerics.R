########################################################
# Definition of generic functions related to aggregation
# and their documentation
########################################################

#
# over generic
#

#' Spatio-temporal overlay of \code{Track} objects with \code{Spatial} or \code{STF} objects
#' @name over
#' @description Spatio-temporal overlay of \code{Track} objects with objects inheriting 
#' from class \code{Spatial} or \code{STF} objects as well as their \code{data.frame} counterparts.
#' 
#' For the spatial or spatio-temporal geometries of \code{x} the indices or attributes from the \code{Track} 
#' object are returned.
#' If \code{x} inherts from class \code{Spatial} the returned object is extended, 
#' such that it respects the temporal information of the \code{Track} object. In particular that means that 
#' the returned object has one value (may also be NA) for each combination from the spatial geometries with 
#' the timestamps from the \code{Track} object.
#' 
#' If \code{fn} and \code{weight.points} are adequately specified a weighted aggregation is performed.
#' 
#' For further details please see the vignette!
#' 
#' @param x object of class \code{Spatial} or \code{STF} (see section 'Usage')
#' @param y \code{Track} object
#' @param returnList logical, indicates if a list should be returned
#' @param fn (optional) aggregation function, that expects a vector of weights as its second argument 
#' if the argument \code{weight.points} is specified to perform a weighted aggregation.
#' @param ... optional additional arguments to \code{fn}
#' @param use.data logical or a vevtor of indices or a vector of column names, indicating if
#' indices (\code{use.data = FALSE}) or (optional aggregated) attribute values should be returned
#' @param weight.points \code{NULL}, "byTime", "byDist" or "equal", indicating the basis of the 
#' weights that should be used for a weighted aggregation of attribute values
#' @return The returned object depends on the specified arguments and it is consistent with the \code{over}
#' methods defined in the packages \pkg{sp} and \pkg{spacetime}. For further details please see also the vignette.
#' @note The weight.points argument should just be specified if a function that expects a vector of weights as 
#' its second argument is passed to \code{fn}.
#' @rdname over
#' @examples
#' ## Load example toy data
#' load(system.file("extdata","trajaggr_TestData.RData", package = "trajaggr"), verbose = FALSE)
#' ## Example 1: overlay of Track and Spatial object
#' # returnList = FALSE returns index vector
#' over_sp <- over(spPolys, Track_A1, returnList = FALSE)
#' dim(spPolys)
#' dim(Track_A1)
#' over_sp
#' # returnList = TRUE returns list of indices
#' over_sp <- over(spPolys, Track_A1, returnList = TRUE)
#' over_sp[1:7]
#' # use.data = TRUE and returnList = TRUE returns list of data.frames
#' over_sp <- over(spPolys, Track_A1, returnList = TRUE, use.data = TRUE)
#' over_sp[1:7]
#' # use.data = TRUE and returnList = FALSE returns data.frames 
#' # with (first) data element
#' over_sp <- over(spPolys, Track_A1, returnList = FALSE, use.data = TRUE)
#' over_sp[1:7, ]
#' ##
#' ## Example 2: overlay of Track and STF object with two time intervals
#' # returnList = FALSE returns index vector
#' over_stf <- over(stf_Polys_2t, Track_A1, returnList = FALSE)
#' dim(stf_Polys_2t)
#' dim(Track_A1)
#' over_stf
#' # returnList = TRUE returns list of indices
#' over_stf <- over(stf_Polys_2t, Track_A1, returnList = TRUE)
#' over_stf
#' # use.data = TRUE and returnList = TRUE returns list of data.frames
#' over_stf <- over(stf_Polys_2t, Track_A1, returnList = TRUE, use.data = TRUE)
#' over_stf
#' # use.data = TRUE and returnList = FALSE returns data.frames with (first) 
#' # data element
#' over_stf <- over(stf_Polys_2t, Track_A1, returnList = FALSE, use.data = TRUE)
#' over_stf
#' # use.data = TRUE and returnList = FALSE and fn = mean returns aggregated values
#' over_stf_mean <- over(stf_Polys_2t, Track_A1, returnList = FALSE, fn = mean, 
#' use.data = TRUE)
#' over_stf_mean
#' # use.data = TRUE and returnList = FALSE and fn = weighted.mean 
#' # and additionally specified weight.points = 'byTime' returns weighted 
#' # aggregated values
#' over_stf_wmean <- over(stf_Polys_2t, Track_A1, returnList = FALSE, 
#' fn = weighted.mean, weight.points = "byTime", use.data = TRUE)
#' over_stf_wmean
#' @exportMethod over
if (!isGeneric("over"))
  setGeneric(name = "over", function(x, y, returnList = FALSE, fn = NULL, ...)
    standardGeneric("over"))
#####





#
# count generic
#

#' Counting trajectories over spatial or spatio-temporal geometries
#' @name count
#' @description Counting trajectories over spatial or spatio-temporal geometries. An \code{Track} object is 
#' counted for a particualr geometry if at least one of its points
#' spatially or spatio-temporally intersects that particular geometry.
#' 
#' Works also with \code{data.frame} counterparts for the \code{by} argument.
#' 
#' @param x an object of class \code{Track}, \code{Tracks} or \code{TracksCollection}
#' @param by an object of class \code{Spatial} or \code{STF} (see section 'Usage')
#' @param byID logical; indicating if the counting should be performed individually for each
#' \code{Tracks} object (in the case that a \code{TracksCollection} is passed to the 
#' \code{x} argument).
#' @return A \code{data.frame} counterpart object of the \code{by} argument's object.
#' The \code{data.frame} of the returned object contains an attribute 'count' representing the 
#' number of counted Track objects for each spatial or spatio-temporal geometry of \code{by}.
#' @rdname count
#' @examples
#' ## Example 1 with toy data
#' ## load example toy data
#' load(system.file("extdata","trajaggr_TestData.RData", package = "trajaggr"), verbose = FALSE)
#' countTrC_res <- count(TrColl, spPolys)
#' class(countTrC_res)
#' dim(spPolys)
#' dim(countTrC_res)
#' slot(countTrC_res, "data")
#' countTrC_byID <- count(TrColl, spPolys, byID = TRUE)
#' class(countTrC_byID)
#' head(slot(countTrC_byID, "data"))
#' countTrC_bySTF <- count(TrColl, stf_Polys_2t)
#' class(countTrC_bySTF)
#' slot(countTrC_bySTF, "data")
#' ##
#' ## Example 2 with wild boars example data
#' ## load data
#' data(wildboars_4Ind_ltraj)
#' wb_TrColl <- as(wildboars_4Ind_ltraj, "TracksCollection")
#' library(sp)
#' wb_spPoly <- createSpatialArealObjFromPoints(as(wb_TrColl,
#'  "SpatialPointsDataFrame"), desDim = 8, out = "SpatialPixels")
#' wb_count <- count(wb_TrColl, wb_spPoly)
#' slot(wb_count, "data")
#' @exportMethod count
if (!isGeneric("count"))
  setGeneric(name = "count", function(x, by, byID = FALSE)
    standardGeneric("count"))
##


#
# aggregate generic
#

#' Spatio-temporal aggregation of trajectory data
#' @name aggregate
#' @description The method \code{aggregate} performs spatio-temporal (optionally weighted) aggregation of
#'  trajectory data over spatial or spatio-temporal geometries.
#' 
#' For further detailed information please see the vignette!
#' 
#' @param x object of class \code{Track}, \code{Tracks} or \code{TracksCollection} 
#' (see section 'Usage')
#' @param by object of class \code{SpatialGrid}, \code{SpatialPixels}, \code{SpatialPolyons}
#' or \code{STF} with \code{SpatialPixels} or \code{SpatialPolyons} in the \code{sp} slot. \code{data.frame}
#' counterparts of these objects are accepted as well.
#' @param FUN aggregation function used to calculate the aggregated values, optional a function that expects
#' a vector with weights as its second argument to perform a weighted aggregation.
#' @param ... optional additional arguments to \code{FUN}
#' @param simplify logical, should the return object be simplified (if possible)
#' @param use.data TRUE (all data), an index vector or a vector of column names, indicating which 
#' data should be considered from the data slot(s) of the trajectories object.
#' @param weight.points \code{NULL}, 'byTime', 'byDist' or 'equal', indicates the weighting of 
#' points' attribute data when calculating the aggregated values from a \code{Track} object's
#' attributes for a spatio-temporal geometry.
#' @param weight.tracks \code{NULL}, 'byTime', 'byDist' or 'equal', indicates the weighting of 
#' track objects when calculating the aggregated values from several \code{Track} objects' parts
#' for a spatio-temporal geometry.
#' @param byID logical, indicating if the aggregation should be performed individually for each 
#' \code{Tracks} object if \code{x} is a \code{TracksCollection}
#' @return An object of class \code{STFDF} is returned, whose data slot contains the aggregated
#' values for each spatio-temporal geometry.
#' @note The arguments \code{weight.points} and \code{weight.tracks} should just be specified, 
#' if a function is passed to \code{FUN} that expects a vector with weights!
#' @rdname aggregate
#' @examples
#' ## load toy example data
#' load(system.file("extdata","trajaggr_TestData.RData", package = "trajaggr"), verbose = FALSE)
#' ## Aggregation over STF objects
#' # Aggregate Track over STF with two time intervals
#' # FUN = max
#' agg <- aggregate(Track_A1, stf_Polys_2t, FUN = max)
#' class(agg)
#' dim(stf_Polys_2t)
#' dim(agg)
#' slot(agg, "data")
#' # FUN = sum
#' agg_0 <- aggregate(Track_A1, stf_Polys_2t, FUN = sum, na.rm= TRUE)
#' slot(agg_0, "data")
#' # FUN = weighted.mean, weighted by time
#' agg_1 <- aggregate(Track_A1, stf_Polys_2t, FUN = weighted.mean, 
#' weight.points = "byTime")
#' slot(agg_1, "data")
#' ##
#' # Aggregate Tracks over STF with two time intervals
#' agg_2 <- aggregate(Tracks_A, stf_Polys_2t, FUN = weighted.mean, na.rm = TRUE, 
#' weight.points = "byTime")
#' slot(agg_2, "data")
#' ##
#' # Aggregate TracksCollection over STF with two time intervals
#' agg_3 <- aggregate(TrColl, stf_Polys_2t, FUN = weighted.mean, na.rm = TRUE, 
#' weight.points = "byTime")
#' slot(agg_3, "data")
#' ##
#' # Aggregate TracksCollection seperated by individuals over STF with two time 
#' # intervals,
#' agg_3 <- aggregate(TrColl, stf_Polys_2t, FUN = weighted.mean, na.rm = TRUE, 
#' weight.points = "byTime", byID = TRUE)
#' slot(agg_3, "data")
#' @exportMethod aggregate
if (!isGeneric("aggregate"))
  setGeneric(name = "aggregate", function(x, ...)
    standardGeneric("aggregate"))

#if (!isGeneric("aggregateBy"))
  setGeneric(name = "aggregateBy", function(x, by, FUN, ...)
    standardGeneric("aggregateBy"))
##


# # Not exported but internally used.
# # For short description see: over_sp_IgnDom.R
# #
# # overIgnDom generic
# #
# # 
# # Method overIgnDom.
# # @name overIgnDom
# # @description This is the description... ; ) !!! TODO
# # Auch geschrieben in roxygen docu for generic!
# # @param x an object of class \code{Spatial}
# # @param y an object of class \code{Track}, \code{Tracks} or \code{TracksCollection}
# # @param returnList indicates if a list should be returned
# # @param fn function to be used
# # @param ... optional arguemnts to fn
# # @param use.data should data from the \code{data} slot of \code{y} be considered: 
# # FALSE, TRUE, index vector or an data attribute names vector
# # @param weight.points weighting of \code{Track} point measurement values: 
# # NULL, "byTime", "byDist" or "equal"
# # @return \code{vector}, \code{list} or \code{data.frame}
# # @rdname overIgnDom
# # @exportMethod overIgnDom

if (!isGeneric("overIgnDom"))
  setGeneric(name = "overIgnDom", function(x, y, returnList = FALSE, fn = NULL, ...)
    standardGeneric("overIgnDom"))
##


# # Not exported and not used:
# # For short description see: count_over_sp_bySpLines.R
# #
# # countBySpL generic
# #
# # Method countBySpL.
# # @name countBySpL
# # @description Count trajectories objects represented by SpatialLines over spatial objects.
# # @param x an object of class 'Track', 'Tracks' or 'TracksCollection'
# # @param y an object of class 'Spatial' (see section 'Usage')
# # @rdname countBySpL
# # @exportMethod countBySpL
# #if (!isGeneric("countBySpL"))
#   #setGeneric(name = "countBySpL", function(x, y, ...)
#   setGeneric(name = "countBySpL", function(x, y)
#     standardGeneric("countBySpL"))