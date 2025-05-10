# 加载包
library(terra)
library(sf)
library(ggplot2)

# 读取数据
admin_sf <- st_read("Data/Resource/TJ-district.json") 
tianjin_shp <- vect("Data/Resource/TJ-Landkind(2018)-1m/tianjin100.shp") |> st_as_sf() 

# 修复无效几何，转换坐标系统
tianjin_shp <- st_make_valid(tianjin_shp)  
tianjin_shp <- st_transform(tianjin_shp, crs = 4326)

# 获取地图的边界范围
map_bbox <- st_bbox(tianjin_shp)
lon_min <- map_bbox["xmin"]
lon_max <- map_bbox["xmax"]
lat_min <- map_bbox["ymin"]
lat_max <- map_bbox["ymax"]

# ggplot2启动！
map_plot <- ggplot() +
  geom_sf(data = admin_sf, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = tianjin_shp, aes(fill = as.factor(亚类)), color = NA) +
  scale_fill_viridis_d(name = "Landkind Subclass") +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +
  theme_minimal() +
  labs(title = "Tianjin Landkind Subclass Map") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 4), 
    axis.ticks.x = element_line(linewidth = 1), 
    legend.position = "bottom", 
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

print(map_plot)
