library(sf) # ベクタデータ用クラスと関数
library(terra) # ラスタデータ用クラスと関数
library(spData) # 地理データをロード
library(spDataLarge) # 大きい地理データをロード

class(world)
names(world)

plot(world, max.plot = 10)

world_mini <- world[1:2, 1:3]
world_mini

world_dfr <- st_read(system.file("shapes/world.shp", package = "spData"))
world_tbl <- read_sf(system.file("shapes/world.shp", package = "spData"))
class(world_dfr)
class(world_tbl)

plot(world[3:6])
plot(world["pop"])

world_asia <- world[world$continent == "Asia", ]
asia <- st_union(world_asia)

plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

plot(world["continent"], reset = FALSE)
cex <- sqrt(world$pop) / 10000
world_cents <- st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

india <- world[world$name_long == "India", ]
dev.new()
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(st_geometry(world_asia), add = TRUE)
dev.off()

lnd_point <- st_point(c(0.1, 51.5)) # sfg object
lnd_geom <- st_sfc(lnd_point, crs = "EPSG:4326") # sfc object
lnd_attrib <- data.frame( # data.frame object
  name = "London",
  temperature = 25,
  date = as.Date("2023-06-21")
)
lnd_sf <- st_sf(lnd_attrib, geometry = lnd_geom) # sf object

lnd_sf

class(lnd_sf)

st_point(c(5, 2)) # XY point
st_point(c(5, 2, 3)) # XYZ point
st_point(c(5, 2, 1), dim = "XYM") # XYM point
st_point(c(5, 2, 3, 1)) # XYZM point

# rbind 関数により行列の作成が簡単になった。
## MULTIPOINT
multipoint_matrix <- rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
st_multipoint(multipoint_matrix)
## LINESTRING
linestring_matrix <- rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
st_linestring(linestring_matrix)

## POLYGON
polygon_list <- list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
st_polygon(polygon_list)

## 穴あきポリゴン
polygon_border <- rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
polygon_hole <- rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
polygon_with_hole_list <- list(polygon_border, polygon_hole)
st_polygon(polygon_with_hole_list)

## MULTILINESTRING
multilinestring_list <- list(
  rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)),
  rbind(c(1, 2), c(2, 4))
)
st_multilinestring(multilinestring_list)

## MULTIPOLYGON
multipolygon_list <- list(
  list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))),
  list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
)
st_multipolygon(multipolygon_list)

## GEOMETRYCOLLECTION
geometrycollection_list <- list(
  st_multipoint(multipoint_matrix),
  st_linestring(linestring_matrix)
)
st_geometrycollection(geometrycollection_list)

# sfc POINT
point1 <- st_point(c(5, 2))
point2 <- st_point(c(1, 3))
points_sfc <- st_sfc(point1, point2)
points_sfc

# sfc POLYGON
polygon_list1 <- list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
polygon1 <- st_polygon(polygon_list1)
polygon_list2 <- list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
polygon2 <- st_polygon(polygon_list2)
polygon_sfc <- st_sfc(polygon1, polygon2)
st_geometry_type(polygon_sfc)

# sfc MULTILINESTRING
multilinestring_list1 <- list(
  rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)),
  rbind(c(1, 2), c(2, 4))
)
multilinestring1 <- st_multilinestring((multilinestring_list1))
multilinestring_list2 <- list(
  rbind(c(2, 9), c(7, 9), c(5, 6), c(4, 7), c(2, 7)),
  rbind(c(1, 7), c(3, 8))
)
multilinestring2 <- st_multilinestring((multilinestring_list2))
multilinestring_sfc <- st_sfc(multilinestring1, multilinestring2)
st_geometry_type(multilinestring_sfc)

# sfc GEOMETRY
point_multilinestring_sfc <- st_sfc(point1, multilinestring1)
st_geometry_type(point_multilinestring_sfc)

st_crs(points_sfc)

# 'EPSG' CRS code を参照する識別子で CRS を設定
points_sfc_wgs <- st_sfc(point1, point2, crs = "EPSG:4326")
st_crs(points_sfc_wgs) # print CRS (only first 4 lines of output shown)

v <- c(1, 1)
v_sfg_sfh <- sfheaders::sfg_point(obj = v)
v_sfg_sfh # sf をロードせずに出力

v_sfg_sf <- st_point(v)
print(v_sfg_sf) == print(v_sfg_sfh)

# matrices
m <- matrix(1:8, ncol = 2)
sfheaders::sfg_linestring(obj = m)
# data_frames
df <- data.frame(x = 1:4, y = 4:1)
sfheaders::sfg_polygon(obj = df)

sfheaders::sfc_point(obj = v)
sfheaders::sfc_linestring(obj = m)
sfheaders::sfc_polygon(obj = df)

df_sf <- sfheaders::sf_polygon(obj = df)
st_crs(df_sf) <- "EPSG:4326"

sf_use_s2()

india_buffer_with_s2 <- st_buffer(india, 1) # 1 メートル
sf_use_s2(FALSE)
india_buffer_without_s2 <- st_buffer(india, 1) # 1 メートル

sf_use_s2(TRUE)

raster_filepath <- system.file("raster/srtm.tif", package = "spDataLarge")
my_rast <- rast(raster_filepath)
class(my_rast)

my_rast

dev.new()
plot(my_rast)
dev.off()

single_raster_file <- system.file("raster/srtm.tif", package = "spDataLarge")
single_rast <- rast(raster_filepath)

new_raster <- rast(
  nrows = 6, ncols = 6, resolution = 0.5,
  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
  vals = 1:36
)

multi_raster_file <- system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast <- rast(multi_raster_file)
multi_rast

nlyr(multi_rast)

multi_rast3 <- subset(multi_rast, 3)
multi_rast4 <- subset(multi_rast, "landsat_4")

multi_rast34 <- c(multi_rast3, multi_rast4)

luxembourg <- world[world$name_long == "Luxembourg", ]
st_area(luxembourg) # 最近の sf の場合 s2 パッケージが必要

attributes(st_area(luxembourg))

st_area(luxembourg) / 1000000

units::set_units(st_area(luxembourg), km^2)

res(my_rast)

repr <- project(my_rast, "EPSG:26912")
res(repr)

summary(world)
# - Its geometry type?
#   multipolygon
# - The number of countries?
#   177
# - Its coordinate reference system (CRS)?
#   epsg:4326

plot(world["continent"], reset = FALSE)
cex <- sqrt(world$pop) / 10000
world_cents <- st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)
# - What does the `cex` argument do (see `?plot`)?
#   It specifies the size of the circles
# - Why was `cex` set to the `sqrt(world$pop) / 10000`?
#   So the circles would be visible for small countries but not too large for large countries, also because area increases as a linear function of the square route of the diameter defined by `cex`
# - Bonus: experiment with different ways to visualize the global population.
dev.new()
plot(st_geometry(world_cents), cex = world$pop / 1e9)
plot(st_geometry(world_cents), cex = world$pop / 1e8)
plot(world["pop"])
plot(world["pop"], logz = TRUE)
dev.off()
# Similarities: global extent, colorscheme, relative size of circles
#
# Differences: projection (Antarctica is much smaller for example), graticules, location of points in the countries.
#
# To understand these differences read-over, run, and experiment with different argument values in this script: https://github.com/Robinlovelace/geocompr/raw/main/code/02-contpop.R
#
# `cex` refers to the diameter of symbols plotted, as explained by the help page `?graphics::points`. It is an acronym for 'Chacter symbol EXpansion'.
# It was set to the square route of the population divided by 10,000 because a) otherwise the symbols would not fit on the map and b) to make circle area proportional to population.

nigeria <- world[world$name_long == "Nigeria", ]
dev.new()
plot(st_geometry(nigeria), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(world[0], add = TRUE)
world_coords <- st_coordinates(world_cents)
text(world_coords, world$iso_a2)
dev.off()

# Alternative answer:
nigeria <- world[world$name_long == "Nigeria", ]
africa <- world[world$continent == "Africa", ]
dev.new()
plot(st_geometry(nigeria), col = "white", lwd = 3, main = "Nigeria in context", border = "lightgrey", expandBB = c(0.5, 0.2, 0.5, 0.2))
plot(st_geometry(world), lty = 3, add = TRUE, border = "grey")
plot(st_geometry(nigeria), col = "yellow", add = TRUE, border = "darkgrey")
a <- africa[grepl("Niger", africa$name_long), ]
ncentre <- st_centroid(a)
ncentre_num <- st_coordinates(ncentre)
text(x = ncentre_num[, 1], y = ncentre_num[, 2], labels = a$name_long)
dev.off()

my_raster <- rast(
  ncol = 10, nrow = 10,
  vals = sample(0:10, size = 10 * 10, replace = TRUE)
)
dev.new()
plot(my_raster)
dev.off()

nlcd <- rast(system.file("raster/nlcd.tif", package = "spDataLarge"))
dim(nlcd) # dimensions
res(nlcd) # resolution
ext(nlcd) # extent
nlyr(nlcd) # number of layers
cat(crs(nlcd)) # CRS
dev.new()
plot(nlcd)
dev.off()

cat(crs(nlcd))
## The WKT above describes a two-dimensional projected coordinate reference system.

## It is based on the GRS 1980 ellipsoid with  North American Datum 1983  and the Greenwich prime meridian.

## It used the Transverse Mercator projection to transform from geographic to projected CRS (UTM zone 12N).

## Its first axis is related to eastness, while the second one is related to northness, and both axes have units in meters.

## The SRID of the above CRS is "EPSG:26912".
