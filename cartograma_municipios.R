library(ggplot2)
library(gridExtra)
library(viridis)
# devtools::install_github("jbaileyh/geogrid")
library(geogrid)

input_file <- file.path("data", "municipios_simplified_002.geojson")
original_shapes <- read_polygons(input_file)

raw <- read_polygons(input_file)
raw@data$xcentroid <- sp::coordinates(raw)[,1]
raw@data$ycentroid <- sp::coordinates(raw)[,2]

clean <- function(shape) {
  shape@data$id = rownames(shape@data)
  shape.points = fortify(shape, region="id")
  shape.df = merge(shape.points, shape@data, by="id")
}

result_df_raw <- clean(raw)
result_df_raw$couleur <- as.numeric(levels(result_df_raw$codigo_dep)[result_df_raw$codigo_dep])
rawplot <- ggplot(result_df_raw) +
  geom_polygon(aes(x = long, y = lat, fill = couleur, group=group)) +
  #geom_text(aes(xcentroid, ycentroid, label = substr(nombre_dep, 1, 4)), size = 2,color = "white") +
  coord_equal() +
  scale_fill_viridis() +
  guides(fill = FALSE) +
  theme_void()

rawplot


#par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
#for (i in 1:6) {
  #new_cells <- calculate_grid(shape = original_shapes, grid_type = "hexagonal", seed = i)
  #plot(new_cells, main = paste("Seed", i, sep = " "))
#}


#par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
#for (i in 1:6) {
#  new_cells <- calculate_grid(shape = original_shapes, grid_type = "regular", seed = i)
#  sp::plot(new_cells, main = paste("Seed", i, sep = " "))
#}



new_cells_hex <- calculate_grid(shape = original_shapes, grid_type = "hexagonal", seed = 3)
resulthex <- assign_polygons(original_shapes, new_cells_hex)

new_cells_reg <- calculate_grid(shape = original_shapes, grid_type = "regular", seed = 3)
resultreg <- assign_polygons(original_shapes, new_cells_reg)


result_df_hex <- clean(resulthex)
result_df_hex$couleur <- as.numeric(levels(result_df_hex$codigo_dep)[result_df_hex$codigo_dep])

result_df_reg <- clean(resultreg)
result_df_reg$couleur <- as.numeric(levels(result_df_reg$codigo_dep)[result_df_reg$codigo_dep])

hexplot <- ggplot(result_df_hex) +
  geom_polygon(aes(x = long, y = lat, fill = couleur, group=group)) +
  #geom_text(aes(V1, V2, label = substr(nombre_dep, 1, 4)), size = 2, color = "white") +
  scale_fill_viridis() +
  coord_equal() +
  guides(fill = FALSE) +
  theme_void()

regplot <- ggplot(result_df_reg) +
  geom_polygon(aes(x = long, y = lat, fill = couleur, group=group)) +
  #geom_text(aes(V1, V2, label = substr(nombre_dep, 1, 4)), size = 2, color = "white") +
  coord_equal() +
  scale_fill_viridis() +
  guides(fill = FALSE) +
  theme_void()

grid.arrange(rawplot, regplot, hexplot, layout_matrix = rbind(c(1, 1), c(2, 3)))
