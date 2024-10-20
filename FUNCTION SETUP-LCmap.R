#' Title
#'
#' @return
#' @export
#'
#' @examples
LCmap.setup <- function(){
  require(sf); require(prettymapr)
  git <- "https://github.com/Ratey-AtUWA/Learn-R-web/raw/main/"
  source(paste0(git,"scalebar_use_sf_prettymapr.R"))
  LC_site <- st_read(paste0(git, "LC_UWA_study_area.kml"))
  LC_edge <- st_read(paste0(git, "LC_LakeEdge.kml"))
  LCpaths <- st_read(paste0(git, "LC_paths.kml"))
  LCroads <- st_read(paste0(git, "LCroads.kml"))
  LCrec <- st_read(paste0(git, "LC_rec.kml"))
  LC_mapdata <- list(LC_site,LC_edge,LCpaths,LCroads,LCrec)
  return(LC_mapdata)
}
