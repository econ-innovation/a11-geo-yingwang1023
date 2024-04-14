getwd()
setwd("/Users/yingwang/Documents/bigdata/作业/a11-geo-yingwang1023")
# 读取数据
rm(list = ls())
library(readr)
hefei <- read.table("hefei.txt", header=TRUE)
library(sf)
hefei_points <- st_as_sf(hefei, coords = c("lng", "lat"), crs = 4326)
# 读取开发区坐标
library(sp)
dv_zone1 <- read_sf(dsn = "G341022合肥经济技术开发区.txt")
dv_zone3 <- read_sf(dsn = "G342020合肥高新技术产业开发区区块二.txt")
dv_zone2 <- read_sf(dsn = "G342020合肥高新技术产业开发区区块一.txt")
# 找出位于开发区内部的企业
intersection1 <- st_within(hefei_points, dv_zone1)
num_companies_zone1 <- sum(!is.na(intersection1))

intersection2 <- st_within(hefei_points, dv_zone2)
num_companies_zone2 <- sum(!is.na(intersection2))

intersection3 <- st_within(hefei_points, dv_zone3)
num_companies_zone3 <- sum(!is.na(intersection3))

cat("Number of companies within Development Zone 1:", num_companies_zone1, "\n")
cat("Number of companies within Development Zone 2:", num_companies_zone2, "\n")
cat("Number of companies within Development Zone 3:", num_companies_zone3, "\n")

# 计算开发区1km，3km，5km范围内的企业数量
radius <- c(1000, 3000, 5000) # in meters

for (r in radius) {
  # Buffer the development zones
  dv_zone1_buffer <- st_buffer(dv_zone1, dist = r)
  dv_zone2_buffer <- st_buffer(dv_zone2, dist = r)
  dv_zone3_buffer <- st_buffer(dv_zone3, dist = r)
  # Calculate the intersection with the buffered zones
  intersection1_buffer <- st_intersects(hefei_points, dv_zone1_buffer)
  intersection2_buffer <- st_intersects(hefei_points, dv_zone2_buffer)
  intersection3_buffer <- st_intersects(hefei_points, dv_zone3_buffer)
  # Count the number of companies within the buffered zones
  num_companies_zone1_buffer <- sum(sapply(intersection1_buffer, length))
  num_companies_zone2_buffer <- sum(sapply(intersection2_buffer, length))
  num_companies_zone3_buffer <- sum(sapply(intersection3_buffer, length))
  cat("Number of companies within", r/1000, "km from Development Zone 1:", num_companies_zone1_buffer, "\n")
  cat("Number of companies within", r/1000, "km from Development Zone 2:", num_companies_zone2_buffer, "\n")
  cat("Number of companies within", r/1000, "km from Development Zone 3:", num_companies_zone3_buffer, "\n")
}

#2画图###############
library(ggplot2)
# 绘制合肥市的三个开发区
map <- ggplot() +
  geom_sf(data = dv_zone1, fill = "lightblue") +
  geom_sf(data = dv_zone2, fill = "lightgreen") +
  geom_sf(data = dv_zone3, fill = "lightyellow") +
  theme_minimal()
print(map)
# 将企业添加到地图上
map_with_companies <- ggplot() +
  geom_sf(data = dv_zone1, fill = "lightblue") +
  geom_sf(data = dv_zone2, fill = "lightgreen") +
  geom_sf(data = dv_zone3, fill = "lightyellow") +
  geom_sf(data = hefei_points, color = "gray", size = 1) + # 用红色标记企业
  theme_minimal()
print(map_with_companies)


#4. 将合肥市的所有企业都着点在“合肥市”地图上————————————————
# 读取合肥地图的地理信息数据
hefei_map <- st_read("合肥市.json")
# 绘制合肥市地图
hefei_plot <- ggplot() +
  geom_sf(data = hefei_map) +
  theme_minimal()
print(hefei_plot)
# 将合肥市的企业位置添加到地图上
hefei_plot_with_companies <- hefei_plot +
  geom_sf(data = hefei_points, color = "gray", size = 1) + # 用红色标记企业
  labs(title = "Map of Hefei with Enterprises") # 添加标题
print(hefei_plot_with_companies)








