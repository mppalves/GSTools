#' @title Create Map
#' @name create_map
#' @description Wrapper function to automate the process of creating maps and
#'   save them in jpg.
#' @usage create_map(data,color_range = c("#FFFFFF","#ff2323"),color_option =
#'   "D",map_name = NULL, save_jpg = list(T, width = 80, height = 64),
#'   cathegorical = F, limits = NULL)
#' @param data Data frame or Matrix with 3 columns: data, lon, lat in that order.
#' @param color_range Hexa decimal or colour names to describe the range in
#'   which the color values will be ploted
#' @param color_option Color option for package \link{viridis} in case of
#'   cathegorical values
#' @param map_name Name of the map used for saving and as a map tittle
#' @param save_jpg list of width and height values for jpeg file
#' @param cathegorical Boolean to define if the values are continuous or
#'   cathegories
#' @param limits If defined, sets up the upper and lower bounds to the values
#'   plotted in the map.
#' @examples create_map(map_change, save_jpg = list(T, width = 80, height = 64), map_name = "climate scenario 1",
#'           cathegorical = F, color_range = c("#FF0F47", "#FFFFFF", "#59BF34DB"),
#'           limits = c(-75,75))
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @import ggplot2
#' @import ggmap
#' @import tidyverse
#' @import viridis
#' @export

create_map = function(data,color_range = c("#FFFFFF","#ff2323"),color_option = "D",map_name = NULL, save_jpg = list(T, width = 80, height = 64), cathegorical = F, limits = NULL){
color_range[2]
  world <- c(
    left = -170,
    bottom = -60,
    right = 170,
    top = 80
  )

  map <- ggmap::get_stamenmap(world, zoom = 2, maptype = "terrain-background")

  if(cathegorical){
      data[,1] = as.factor(data[,1])
  }

  MapPoints <- ggmap::ggmap(map) +

    geom_point(aes(x = data[,2], y = data[,3], colour = data[,1]), data = data) +
    if(cathegorical){
      #scale_colour_manual(name = colnames(data)[1], values = terrain.colors(length(unique(data[,1]))));
      scale_color_viridis(discrete = TRUE, option = color_option)
    }else{
      scale_color_gradientn(colours = color_range, name = colnames(data)[1], limits=limits);
    }


  MapPoints

  if(save_jpg[[1]]==TRUE){

    ggsave(paste0(map_name,"_", str_remove(str_remove(Sys.time(),":"),":"), ".jpg"),
           width = save_jpg[[2]],
           height = save_jpg[[3]],
           units = "cm",
           limitsize = TRUE)

    return(MapPoints)
    stringrs::str_remove()
  }else{
    return(MapPoints)
  }

}

