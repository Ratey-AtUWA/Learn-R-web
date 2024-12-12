#' @title Make a new map extent sf object (mainly for the maptiles and ggmap packages) by expanding the bounding box of another sf object.
#'
#' @param sfx The sf object around which we want to draw a map 
#' @param amt The scaling factor as a proportion of axis range. The default is 0.05, so 5% of the axis range will be subtracted from the minimum axis value and added to the maximum axis value.
#'
#' @return An sf object containing two points, the lower left and upper right corners defining a rectangular map area.
#' @export
#'
#' @examples pad_bbox(st_as_sf(data.frame(E=c(400000,401000), 
#'                                        N=c(6460000, 6461000)), 
#'                             coords=c("E","N"), 
#'                             crs=st_crs(32750)))

pad_bbox <- function(sfx, amt=0.05){
  require(sf)
  if(class(sfx)[1]!="sf") stop("input must have class `sf`")
  if(!is.numeric(amt)) stop("amt argument must be numeric")
  bbx <- st_bbox(sfx)
  xrng <- bbx$xmax - bbx$xmin
  yrng <- bbx$ymax - bbx$ymin
  sfout <- st_as_sf(data.frame(x=c(bbx$xmin-(xrng*amt), bbx$xmax+(xrng*amt)),
                               y=c(bbx$ymin-(yrng*amt), bbx$ymax+(yrng*amt))),
                    coords=c("x","y"), crs=st_crs(sfx))
  return(sfout)
}