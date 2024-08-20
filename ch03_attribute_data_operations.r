library(tidyverse)
library(sf) # Chapter 2 で紹介したベクタデータパッケージ
library(terra) # Chapter 2 で紹介したラスタデータパッケージ
library(dplyr) # データフレーム操作用 tidyverseパッケージ
library(spData) # Chapter 2 で紹介した空間データパッケージ

methods(class = "sf") # sf オブジェクトのメソッド、最初の 12

st_sf(data.frame(n = world$name_long), g = world$geom)

class(world) # sf オブジェクトであり、(tidy) データフレームである
dim(world) # ２次元オブジェクトで、 177 行 11 列

world_df <- st_drop_geometry(world)
class(world_df)
ncol(world_df)

world[1:6, ] # 位置で行を抽出
world[, 1:3] # 位置で列を抽出
world[1:6, 1:3] # 位置で行と列を抽出
world[, c("name_long", "pop")] # 名称で列を抽出
world[, c(T, T, F, F, F, F, F, T, T, F, F)] # 論理値で抽出
world[, 888] # 存在しない列番号

i_small <- world$area_km2 < 10000
summary(i_small) # 論理ベクトル
small_countries <- world[i_small, ]

small_countries <- world[world$area_km2 < 10000, ]

small_countries <- subset(world, area_km2 < 10000)

world1 <- select(world, name_long, pop)
names(world1)

# name_long から pop までの全ての列
world2 <- select(world, name_long:pop)

# subregion と area_km2 以外全ての列
world3 <- select(world, -subregion, -area_km2)

world4 <- select(world, name_long, population = pop)

world5 <- world[, c("name_long", "pop")] # 名称で列を抽出
names(world5)[names(world5) == "pop"] <- "population" # 列めいを変更

pull(world, pop)
world$pop
world[["pop"]]

slice(world, 1:6)

world7 <- filter(world, area_km2 < 10000) # 面積の小さい国
world7 <- filter(world, lifeExp > 82) # 平均寿命が高い

world7 <- world |>
  filter(continent == "Asia") |>
  select(name_long, continent) |>
  slice(1:5)

world8 <- slice(
  select(
    filter(world, continent == "Asia"),
    name_long, continent
  ),
  1:5
)

world9_filtered <- filter(world, continent == "Asia")
world9_selected <- select(world9_filtered, continent)
world9 <- slice(world9_selected, 1:5)

world_agg1 <- aggregate(pop ~ continent,
  FUN = sum, data = world,
  na.rm = TRUE
)
class(world_agg1)

world_agg2 <- aggregate(world["pop"],
  by = list(world$continent), FUN = sum,
  na.rm = TRUE
)
class(world_agg2)
nrow(world_agg2)

world_agg3 <- world |>
  group_by(continent) |>
  summarize(pop = sum(pop, na.rm = TRUE))

world_agg4 <- world |>
  group_by(continent) |>
  summarize(Pop = sum(pop, na.rm = TRUE), Area = sum(area_km2), N = n())

world_agg5 <- world |>
  st_drop_geometry() |> # 速くするためジオメトリを削除
  select(pop, continent, area_km2) |> # 関心ある列のみの部分集合
  group_by(continent) |> # 大陸でグループ化し要約
  summarize(Pop = sum(pop, na.rm = TRUE), Area = sum(area_km2), N = n()) |>
  mutate(Density = round(Pop / Area)) |> # 人口密度を計算
  slice_max(Pop, n = 3) |> # 上位３件のみ
  arrange(desc(N)) # 国数で並べ替え
