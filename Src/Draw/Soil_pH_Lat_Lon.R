# 加载包
library(terra)
library(ggplot2)
library(viridis) 
library(cowplot) 
library(grid)

# 读取土壤 pH 值tif
ph_raster <- rast("Data/Resource/TJ-土壤酸碱度-250/phh2o_0_5cm_mean.tif")

# 转换坐标系统为 WGS84 (EPSG:4326)
if(grepl("PROJCRS", crs(ph_raster))) {
  ph_raster <- project(ph_raster, "EPSG:4326")
}

# 将pH 值缩小 10 倍（源数据中为*10处理，现在/10）
ph_raster <- ph_raster / 10  

# 提取有效数据
ph_values <- values(ph_raster, mat = FALSE)
valid_cells <- which(!is.na(ph_values))

# 获取经纬度
coords <- xyFromCell(ph_raster, valid_cells)
ph_data <- data.frame(
  longitude = coords[, 1],
  latitude = coords[, 2],
  ph_value = ph_values[valid_cells]
)

# 限制 pH 范围 0-14，去除异常值
ph_data <- subset(ph_data, ph_value >= 0 & ph_value <= 14)

# ggplot2启动！
plot_longitude <- ggplot(ph_data, aes(x = longitude, y = ph_value, color = ph_value)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(name = "pH Value") +
  labs(title = "Soil pH Variation by Longitude", x = "Longitude (°E)", y = "pH Value") +
  theme_minimal()

plot_latitude <- ggplot(ph_data, aes(x = latitude, y = ph_value, color = ph_value)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(name = "pH Value") +
  labs(title = "Soil pH Variation by Latitude", x = "Latitude (°N)", y = "pH Value") +
  theme_minimal() +
  coord_flip()

# 使用 cowplot 拼接
title_plot <- ggdraw() + 
  draw_label("Soil pH Spatial Distribution in Tianjin", fontface = "bold", size = 14, x = 0.5, y = 0.85)

combined_plot <- plot_grid(
  plot_longitude,
  plot_latitude,
  ncol = 1,
  rel_heights = c(0.7, 0.7) 
)

final_plot <- plot_grid(title_plot, combined_plot, ncol = 1, rel_heights = c(0.5, 9))
print(final_plot)
