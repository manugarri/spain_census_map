setwd("YOUR WORK DIR HERE")

if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.us.r-project.org")
  require(rgdal)
}
  
if (!require(rgeos)) {
  install.packages("rgeos", repos = "http://cran.us.r-project.org")
  require(rgeos)
}
if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.us.r-project.org")
  require(rgdal)
}
if (!require(raster)) {
  install.packages("raster", repos = "http://cran.us.r-project.org")
  require(raster)
}
if(!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cloud.r-project.org")
  require(ggplot2)
}
if(!require(viridis)) {
  install.packages("viridis", repos="http://cloud.r-project.org")
  require(viridis)
}
if(!require(dplyr)) {
  install.packages("dplyr", repos = "https://cloud.r-project.org/")
  require(dplyr)
}
if(!require(gtable)) {
  install.packages("gtable", repos = "https://cloud.r-project.org/")
  require(gtable)
}
if(!require(grid)) {
  install.packages("grid", repos = "https://cloud.r-project.org/")
  require(grid)
}
if(!require(tidyr)) {
  install.packages("tidyr", repos = "https://cloud.r-project.org/")
  require(tidyr)
}

data_spain <- read.csv("Censuses2011_2.csv", stringsAsFactors = F)
data_spain$municipality_code <- as.numeric(separate(data_spain, Municipality.of.residence, "municipality_code", " ")$municipality_code)
data_spain$People <- as.numeric(data_spain$People)
data_spain$Average.age <- as.numeric(data_spain$Average.age)
ogrListLayers("Municipios_ETRS89_30N/Municipios_ETRS89_30N.shp")
municipalities_spain <- readOGR("Municipios_ETRS89_30N/Municipios_ETRS89_30N.shp", layer="Municipios_ETRS89_30N")
map_data_fortified_spain <- fortify(municipalities_spain, region = "Codigo") %>% mutate(id = as.numeric(id))
map_data_spain <- map_data_fortified_spain %>% left_join(data_spain, by = c("id" = "municipality_code"))   %>% fill(Average.age)
map_data_spain$avg_age_15 <- map_data_spain$Average.age
rm(data_spain)
rm(map_data_fortified_spain)
rm(municipalities_spain)



# same code as above but different breaks
pretty_breaks <- c(35,40,45,48, 50)
# find the extremes
minVal <- min(map_data_spain$avg_age_15, na.rm = T)
maxVal <- max(map_data_spain$avg_age_15, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]
# define a new variable on the data set just as above
map_data_spain$brks <- cut(map_data_spain$avg_age_15, 
                          breaks = brks, 
                          include.lowest = TRUE, 
                          labels = labels)

brks_scale <- levels(map_data_spain$brks)
labels_scale <- rev(brks_scale)


theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}


p <- ggplot() +
  geom_polygon(data = map_data_plot, aes(fill = brks, 
                                         x = long, 
                                         y = lat, 
                                         group = group)) +
  # municipality outline
  geom_path(data = map_data_plot, aes(x = long, 
                                      y = lat, 
                                      group = group), 
            color = "white", size = 0.1) +
  coord_equal() +
  theme_map() +
  theme(
    legend.position = c(0.7, 0.03),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 14, hjust = 0, color = "#4e4d47"),
    legend.title = element_text(size = 20),
    plot.title = element_text(size = 28, hjust = 0.8, color = "#4e4d47"),
    plot.subtitle = element_text(size = 20, hjust = 0.8, face = "italic", color = "#4e4d47"),
    plot.caption = element_text(size = 14, hjust = 0.95, color = "#4e4d47"),
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.border = element_blank()
  ) +
  labs(x = NULL, 
       y = NULL, 
       title = "Spain's regional demographics", 
       subtitle = "Average age in Spanish municipalities, 2011", 
       caption = "Author: Manuel Garrido (@manugarri) Original Idea: Timo Grossenbacher (@grssnbchr), Geometries: ArcGis Data: INE, 2011;") + 
  scale_fill_manual(
    values = rev(magma(8, alpha = 0.8)[2:7]),
    breaks = rev(brks_scale),
    name = "Average age",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70/length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    )
  )
extendLegendWithExtremes(p)