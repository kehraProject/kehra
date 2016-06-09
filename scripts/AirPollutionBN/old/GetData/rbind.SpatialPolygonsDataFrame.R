#' Get sp feature IDs
#' @aliases IDs IDs.default IDs.SpatialPolygonsDataFrame
#' @param x The object to get the IDs from
#' @param \dots Pass-alongs
#' @rdname IDs
IDs <- function(x,...) {
  UseMethod("IDs",x)
}
#' @method IDs default
#' @S3method IDs default
#' @rdname IDs
IDs.default <- function(x,...) {
  stop("Currently only SpatialPolygonsDataFrames are supported.")
}
#' @method IDs SpatialPolygonsDataFrame
#' @S3method IDs SpatialPolygonsDataFrame
#' @rdname IDs
IDs.SpatialPolygonsDataFrame <- function(x,...) {
  vapply(slot(x, "polygons"), function(x) slot(x, "ID"), "")
}

#' Assign sp feature IDs
#' @aliases IDs<- IDs.default<-
#' @param x The object to assign to
#' @param value The character vector to assign to the IDs
#' @rdname IDs<-
"IDs<-" <- function( x, value ) {
  UseMethod("IDs<-",x)
}
#' @method IDs<- SpatialPolygonsDataFrame
#' @S3method IDs<- SpatialPolygonsDataFrame
#' @rdname IDs<-
"IDs<-.SpatialPolygonsDataFrame" <- function( x, value) {
  spChFIDs(x,value)
}

#' rbind SpatialPolygonsDataFrames together, fixing IDs if duplicated
#' @param \dots SpatialPolygonsDataFrame(s) to rbind together
#' @param fix.duplicated.IDs Whether to de-duplicate polygon IDs or not
#' @return SpatialPolygonsDataFrame
#' @author Ari B. Friedman, with key functionality by csfowler on StackExchange
#' @method rbind.SpatialPolygonsDataFrame
#' @export rbind.SpatialPolygonsDataFrame
rbind.SpatialPolygonsDataFrame <- function(..., fix.duplicated.IDs=TRUE) {
  dots <- as.list(substitute(list(...)))[-1L]
  dots_names <- as.character(dots) # store names of objects passed in to ... so that we can use them to create unique IDs later on
  dots <- lapply(dots,eval)
  names(dots) <- NULL
  # Check IDs for duplicates and fix if indicated
  IDs_list <- lapply(dots,IDs)
  dups.sel <- duplicated(unlist(IDs_list))
  if( any(dups.sel) ) {
    if(fix.duplicated.IDs) {
      dups <- unique(unlist(IDs_list)[dups.sel])
      # Function that takes a SPDF, a string to prepend to the badID, and a character vector of bad IDs
      fixIDs <- function( x, prefix, badIDs ) {
        sel <-  IDs(x) %in% badIDs
        IDs(x)[sel] <- paste( prefix, IDs(x)[sel], sep="." )
        x
      }
      dots <- mapply(FUN=fixIDs , dots, dots_names, MoreArgs=list(badIDs=dups) )
    } else {
      stop("There are duplicated IDs, and fix.duplicated.IDs is not TRUE.")
    }
  }
  # One call to bind them all
  pl = do.call("rbind", lapply(dots, function(x) as(x, "SpatialPolygons")))
  df = do.call("rbind", lapply(dots, function(x) x@data))
  SpatialPolygonsDataFrame(pl, df)
}