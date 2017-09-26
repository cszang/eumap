##' @title Provide a map layer for maps of various regions
##' @param .data data set to plot on the map
##' @param .lon name of the longitude variable as character
##' @param .lat name of the latitude variable as character
##' @param .xlim longitudinal boundaries of the map 
##' @param .ylim latitudinal boundaries of the map
##' @param .region_colour line colour for the region map
##' @param .region_fill fill colour for the region map
##' @return an object of class "gg"
##' @import ggplot2
##' @import maps
##' @import PBSmapping
##' @importFrom data.table setnames
##' @export
regmap <- function(.data, region = "europe",
                  .lon = "lon", .lat = "lat",
                 .xlim = NULL,
                 .ylim = NULL,
                 .region_colour = "#BBBBBB",
                 .region_fill = "#DDDDDD") {

  if ("purrr" %in% (.packages())) {
    map <- maps::map
  }

  .region <- match.arg(region,
                      c("europe", "amazonia"))

  if (is.null(.xlim)) {
    .xlim <- switch(.region,
                   europe = c(-10, 30),
                   amazonia = c(-78, -47))
  }

  if (is.null(.ylim)) {
    .ylim <- switch(.region,
                   europe = c(35, 60),
                   amazonia = c(-18, 11))
  }
  
  world <- map_data("world")
  data.table::setnames(world, c("X","Y","PID","POS","region","subregion"))
  world <- clipPolys(world, xlim = .xlim, ylim = .ylim, keepExtra = TRUE)
  
  ggplot(data = .data,
         aes_string(x = .lon, y = .lat)) +
    geom_polygon(data = world, aes(x = X,  y = Y, group = PID),
                 colour = .region_colour, fill = .region_fill) +
    theme_minimal() +
    xlim(.xlim) + ylim(.ylim) +
    xlab("Longitude") + ylab("Latitude") +
    coord_quickmap()
}
