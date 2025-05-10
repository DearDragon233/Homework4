# 加载必要的包
library(terra)
library(sf)
library(ggplot2)

# 1️⃣ 读取数据
admin_sf <- st_read("Data/Resource/TJ-district.json")  # 天津行政区划
tianjin_shp <- vect("Data/Resource/TJ-Landkind(2018)-1m/tianjin100.shp") |> st_as_sf()  # 天津市土地数据

# 2️⃣ 修复无效几何，并转换坐标系统（确保使用 WGS 84）
tianjin_shp <- st_make_valid(tianjin_shp)  
tianjin_shp <- st_transform(tianjin_shp, crs = 4326)

# 3️⃣ 获取地图的边界范围
map_bbox <- st_bbox(tianjin_shp)
lon_min <- map_bbox["xmin"]
lon_max <- map_bbox["xmax"]
lat_min <- map_bbox["ymin"]
lat_max <- map_bbox["ymax"]

# 4️⃣ 绘制土地亚类分布地图
map_plot <- ggplot() +
  geom_sf(data = admin_sf, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = tianjin_shp, aes(fill = as.factor(亚类)), color = NA) +
  scale_fill_viridis_d(name = "Landkind Subclass") +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +  # **expand = FALSE 避免坐标范围缩放**
  theme_minimal() +
  labs(title = "Tianjin Landkind Subclass Map") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 4),  # **确保经度刻度显示**
    axis.ticks.x = element_line(linewidth = 1),  # **使用 `linewidth` 替代 `size`**
    legend.position = "bottom",  # **图例置底**
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# 展示最终地图
print(map_plot)
