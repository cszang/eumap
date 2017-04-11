##' @title Provide a map layer for maps of Europe
##' @param .data data set to plot on the map
##' @param .lon name of the longitude variable as character
##' @param .lat name of the latitude variable as character
##' @param xlim longitudinal boundaries of the map
##' @param ylim latitudinal boundaries of the map
##' @return an object of class "gg"
##' @import ggplot2
##' @import maps
##' @import PBSmapping
##' @importFrom data.table setnames
##' @export
eumap <- function(.data, .lon = "lon", .lat = "lat",
                 .xlim = c(-10, 30),
                 .ylim = c(35, 60),
                 .eu_colour = "#BBBBBB",
                 .eu_fill = "#DDDDDD") {
  
  world <- map_data("world")
  data.table::setnames(world, c("X","Y","PID","POS","region","subregion"))
  world <- clipPolys(world, xlim = xlim, ylim = ylim, keepExtra = TRUE)
  
  ggplot(data = .data,
         aes_string(x = .lon, y = .lat)) +
    geom_polygon(data = world, aes(x = X,  y = Y, group = PID),
                 colour = .eu_colour, fill = .eu_fill) +
    theme_minimal() +
    xlim(.xlim) + ylim(.ylim) +
    xlab("Longitude") + ylab("Latitude") +
    coord_quickmap()
}
