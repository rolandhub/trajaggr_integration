########################################################
# Definition of generic functions related to coercion
# and their documentation
########################################################


###################
#
# as.Track
#
#------------------

#' Coercion to \code{Track} objects
#' @name as.Track
#' @description Coercion of \code{Move}, \code{MoveBurst} and \code{ltraj} objects
#' to objects of class \code{Track}.
#' 
#' Just \code{ltraj} objects that contain exactly one trajectory may be coerced to \code{Track} objects.
#' 
#' For further details please see the vignette!
#' 
#' @param obj object of class \code{Move}, \code{MoveBurst} or \code{ltraj}
#' @return Object of class \code{Track}
#' @rdname as.Track
#' @note The method may be used in two ways: as(obj, "Track") or as.Track(obj)
#' @exportMethod as.Track
#' @examples
#' # load example data
#' data(wildboars_4Ind_ltraj)
#' # check object
#' class(wildboars_4Ind_ltraj)
#' # Subset a Track, adehabitatLT required
#' require(adehabitatLT)
#' oneLtraj <- wildboars_4Ind_ltraj[1]
#' class(oneLtraj)
#' length(oneLtraj)
#' summary(oneLtraj[[1]])
#' # Coerce to Track
#' track <- as(oneLtraj, "Track")
#' class(track)
#' slot(track, "data")
#' slot(track, "connections")
if (!isGeneric("as.Track"))
  setGeneric("as.Track", function(obj) {
    standardGeneric("as.Track")
  })

#' @rdname as.Track
#' @aliases as.Track,Move
setMethod("as.Track", "Move",
          function(obj) { as(obj, "Track") })

#' @rdname as.Track
#' @aliases as.Track,MoveBurst
setMethod("as.Track", "MoveBurst", 
          function(obj) { as(obj, "Track") })

#' @rdname as.Track
#' @aliases as.Track,ltraj
setMethod("as.Track", "ltraj", 
          function(obj) { as(obj, "Track") })


#####################
#
# as.Tracks
#
#--------------------

#' Coercion to \code{Tracks} objects
#' @name as.Tracks
#' @description Coercion of \code{MoveBurst}, \code{MoveStack} and \code{ltraj} objects
#' to objects of class \code{Tracks}.
#'
#' Just \code{ltraj} objects that contain more than one trajectory of one individual may be coerced to \code{Tracks} objects. 
#' 
#' For further details please see the vignette!
#' 
#' @param obj object of class \code{MoveBurst}, \code{MoveStack} or \code{ltraj}
#' @return Object of class \code{Tracks}
#' @rdname as.Tracks
#' @note The method may be used in two ways: as(obj, "Tracks") or as.Tracks(obj).
#' @examples
#' ## load pigeons example data
#' data(pigeon_R_moveStack)
#' class(pigeon_R_moveStack)
#' unique(slot(pigeon_R_moveStack, "trackId"))
#' length(pigeon_R_moveStack)
#' # coerce to Tracks
#' pigeon_R_Trcs <- as.Tracks(pigeon_R_moveStack)
#' class(pigeon_R_Trcs)
#' dim(pigeon_R_Trcs)
#' names(slot(pigeon_R_Trcs, "tracks"))
#' @exportMethod as.Tracks
if (!isGeneric("as.Tracks"))
  setGeneric("as.Tracks", function(obj) {
    standardGeneric("as.Tracks")
  })

#' @rdname as.Tracks
#' @aliases as.Tracks,MoveBurst
setMethod("as.Tracks", "MoveBurst",
          function(obj) { as(obj, "Tracks") })

#' @rdname as.Tracks
#' @aliases as.Tracks,MoveStack
setMethod("as.Tracks", "MoveStack",
          function(obj) { as(obj, "Tracks") })

#' @rdname as.Tracks
#' @aliases as.Tracks,ltraj
setMethod("as.Tracks", "ltraj",
          function(obj) { as(obj, "Tracks") })




#####################
#
# as.TracksCollection
#
# -------------------

#' Coercion to \code{TracksCollection} objects
#' @name as.TracksCollection
#' @description Coercion of \code{MoveStack} and \code{ltraj} objects
#' to objects of class \code{TracksCollection}.
#' 
#' Just \code{ltraj} objects that contain trajectories from more than one individual 
#' may be coerced to \code{TracksCollection} objects. 
#' 
#' For further details please see the vignette!
#' 
#' @param obj object of class \code{MoveStack} or \code{ltraj}
#' @return Object of class \code{TracksCollection}
#' @rdname as.TracksCollection
#' @note The method may be used in two ways: as(obj, "TracksCollection") or 
#' as.TracksCollection(obj).
#' @examples
#' # load wild boars example data
#' data(wildboars_4Ind_ltraj)
#' # check object
#' class(wildboars_4Ind_ltraj)
#' length(wildboars_4Ind_ltraj)
#' require(adehabitatLT)
#' summary(wildboars_4Ind_ltraj)
#' # Coerce to TracksCollection
#' tracksColl <- as(wildboars_4Ind_ltraj, "TracksCollection")
#' class(tracksColl)
#' dim(tracksColl)
#' @exportMethod as.TracksCollection
if (!isGeneric("as.TracksCollection"))
  setGeneric("as.TracksCollection", function(obj) {
    standardGeneric("as.TracksCollection")
  })

#' @rdname as.TracksCollection
#' @aliases as.TracksCollection,MoveStack
setMethod("as.TracksCollection", "MoveStack",
          function(obj) { as(obj, "TracksCollection") })

#' @rdname as.TracksCollection
#' @aliases as.TracksCollection,ltraj
setMethod("as.TracksCollection", "ltraj",
          function(obj) { as(obj, "TracksCollection") })




#####################
#
# as.ltraj
#
# -------------------

#' Coercion to \code{ltraj} objects
#' @name as.ltraj
#' @description Coercion of \code{Track}, \code{Tracks} and \code{TracksCollection} objects
#' to objects of class \code{ltraj}.
#' 
#' For further details please see the vignette.
#' 
#' @param obj object of class \code{Track}, \code{Tracks} or \code{TracksCollection}
#' @return Object of class \code{ltraj}
#' @rdname as.ltraj
#' @note The method may be used in two ways: as(obj, "ltraj") or as.ltraj(obj).
#' @examples
#' ## Load example toy data
#' load(system.file("extdata","trajaggr_TestData.RData", package = "trajaggr"), verbose = FALSE)
#' ## Example 1: coerce Track to ltraj
#' slot(Track_A1, "data")
#' slot(Track_A1, "time")
#' ltraj_Tr_A1 <- as(Track_A1, "ltraj")
#' class(ltraj_Tr_A1)
#' require(adehabitatLT)
#' summary(ltraj_Tr_A1)
#' ltraj_Tr_A1[[1]]
#' attr(ltraj_Tr_A1[[1]], "infolocs")
#' ##
#' ## Example 2: coerce TracksCollection to ltraj
#' slot(TrColl, "tracksCollectionData")
#' ltraj_TrColl <- as(TrColl, "ltraj")
#' class(ltraj_TrColl)
#' require(adehabitatLT)
#' summary(ltraj_TrColl)
#' slot(TrColl[2][2], "data")
#' attr(ltraj_TrColl[[4]], "infolocs")
#' @exportMethod as.ltraj
if (!isGeneric("as.ltraj"))
  setGeneric("as.ltraj", function(obj) {
    standardGeneric("as.ltraj")
  })

#' @rdname as.ltraj
#' @aliases as.ltraj,Track
setMethod("as.ltraj", "Track",
          function(obj) { as(obj, "ltraj") })

#' @rdname as.ltraj
#' @aliases as.ltraj,Tracks
setMethod("as.ltraj", "Tracks",
          function(obj) { as(obj, "ltraj") })

#' @rdname as.ltraj
#' @aliases as.ltraj,TracksCollection
setMethod("as.ltraj", "TracksCollection",
          function(obj) { as(obj, "ltraj") })




######################
#
# as.Move
#
# --------------------

#' Coercion to \code{Move} objects
#' @name as.Move
#' @description Coercion of \code{Track} objects to objects of class \code{Move}.
#' 
#' For further details please see the vignette!
#' 
#' @param obj object of class \code{Track} 
#' @return Object of class \code{Move}
#' @rdname as.Move
#' @note The method may be used in two ways: as(obj, "Move") or as.Move(obj).
#' @examples
#' ## Load example toy data
#' load(system.file("extdata","trajaggr_TestData.RData", package = "trajaggr"), verbose = FALSE)
#' move_Tr_A1 <- as.Move(Track_A1)
#' slot(Track_A1, "data")
#' class(move_Tr_A1)
#' slot(move_Tr_A1, "data")
#' slot(Track_A1, "time")
#' slot(move_Tr_A1, "timestamps")
#' @exportMethod as.Move
if (!isGeneric("as.Move"))
  setGeneric("as.Move", function(obj) {
    standardGeneric("as.Move")
  })

#' @rdname as.Move
#' @aliases as.Move,Track
setMethod("as.Move", "Track",
          function(obj) { as(obj, "Move") })



######################
#
# as.MoveBurst
#
# --------------------

#' Coercion to \code{MoveBurst} objects
#' @name as.MoveBurst
#' @description Coercion of \code{Tracks} objects to objects of class \code{MoveBurst}.
#' 
#' For further details please see the vignette!
#' 
#' @param obj object of class \code{Tracks}
#' @return Object of class \code{MoveBurst}
#' @rdname as.MoveBurst
#' @note The method may be used in two ways: as(obj, "MoveBurst") or as.MoveBurst(obj).
#' 
#' The coercion to \code{MoveBurst} objects just works if the package \pkg{move}
#' or at least the package \pkg{sp} is explicitly loaded.
#' 
#' @examples
#' ## Load example toy data
#' load(system.file("extdata","trajaggr_TestData.RData", package = "trajaggr"), verbose = FALSE)
#' ## Coerce Tracks to MoveBurst
#' require(move)
#' moveB_Trcs_A <- as.MoveBurst(Tracks_A)
#' class(moveB_Trcs_A)
#' slot(moveB_Trcs_A, "data")
#' slot(Tracks_A[1], "data")
#' slot(Tracks_A[2], "data")
#' @exportMethod as.MoveBurst
if (!isGeneric("as.MoveBurst"))
  setGeneric("as.MoveBurst", function(obj) {
    standardGeneric("as.MoveBurst")
  })

#' @rdname as.MoveBurst
#' @aliases as.MoveBurst,Tracks
setMethod("as.MoveBurst", "Tracks",
          function(obj) { as(obj, "MoveBurst") })



######################
#
# as.MoveStack
#
# --------------------

#' Coercion to \code{MoveStack} objects
#' @name as.MoveStack
#' @description Coercion of \code{Tracks} and \code{TracksCollection} objects
#' to objects of class \code{MoveStack}.
#' 
#' For further details please see the vignette!
#' 
#' @param obj object of class \code{Tracks} or \code{TracksCollection}
#' @return Object of class \code{MoveStack}
#' @rdname as.MoveStack
#' @note The method may be used in two ways: as(obj, "MoveStack") or as.MoveStack(obj).
#' 
#' The coercion to \code{MoveStack} objects just works if the package \pkg{move}
#' or at least the package \pkg{sp} is explicitly loaded.
#' 
#' @examples
#' ## Load example toy data
#' load(system.file("extdata","trajaggr_TestData.RData", package = "trajaggr"), verbose = FALSE)
#' ## Example 1: Coerce Tracks to MoveStack
#' require(move)
#' move_Trcs_A <- as.MoveStack(Tracks_A)
#' slot(Tracks_A[1], "data")
#' class(move_Trcs_A)
#' slot(move_Trcs_A, "data")[1:6, ]
#' slot(Tracks_A[2], "time")
#' slot(move_Trcs_A, "timestamps")[7:11]
#' ##
#' ## Example 2: Coerce TracksCollection to MoveStack
#' move_TrColl <- as.MoveStack(TrColl)
#' class(move_TrColl)
#' dim(TrColl)
#' length(unique(slot(move_TrColl, "trackId")))
#' slot(TrColl[2][2], "data")
#' slot(move_TrColl, "data")[18:21, 1, drop = FALSE]
#' @exportMethod as.MoveStack
if (!isGeneric("as.MoveStack"))
  setGeneric("as.MoveStack", function(obj) {
    standardGeneric("as.MoveStack")
  })

#' @rdname as.MoveStack
#' @aliases as.MoveStack,TracksCollection
setMethod("as.MoveStack", "TracksCollection",
          function(obj) { as(obj, "MoveStack") })

#' @rdname as.MoveStack
#' @aliases as.MoveStack,Tracks
setMethod("as.MoveStack", "Tracks",
          function(obj) { as(obj, "MoveStack") })
