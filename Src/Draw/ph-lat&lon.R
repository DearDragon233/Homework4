# 加载必要的包
library(terra)      # 处理栅格数据
library(ggplot2)    # 绘图
library(viridis)    # 颜色渐变
library(cowplot)    # 拼接图表
library(grid)

# 读取土壤 pH 值栅格数据
ph_raster <- rast("Data/Resource/TJ-土壤酸碱度-250/phh2o_0_5cm_mean.tif")

# 转换坐标系统为 WGS84 (EPSG:4326)，确保数据正确投影
if(grepl("PROJCRS", crs(ph_raster))) {
  ph_raster <- project(ph_raster, "EPSG:4326")
}

# 对 pH 值缩小 10 倍
ph_raster <- ph_raster / 10  

# 提取有效数据
ph_values <- values(ph_raster, mat = FALSE)
valid_cells <- which(!is.na(ph_values))

# 获取对应的经纬度
coords <- xyFromCell(ph_raster, valid_cells)
ph_data <- data.frame(
  longitude = coords[, 1],  # 提取经度
  latitude = coords[, 2],   # 提取纬度
  ph_value = ph_values[valid_cells]  # pH 值
)

# 限制 pH 范围 0-14，去除异常值
ph_data <- subset(ph_data, ph_value >= 0 & ph_value <= 14)

# 绘制 pH 随经度变化的散点图
plot_longitude <- ggplot(ph_data, aes(x = longitude, y = ph_value, color = ph_value)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(name = "pH Value") +
  labs(title = "Soil pH Variation by Longitude", x = "Longitude (°E)", y = "pH Value") +
  theme_minimal()

# 绘制 pH 随纬度变化的散点图
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
  plot_longitude,  # pH vs Longitude（上方）
  plot_latitude,   # pH vs Latitude（下方）
  ncol = 1,        # 纵向排列
  rel_heights = c(0.7, 0.7)  # 调整高度比例
)

final_plot <- plot_grid(title_plot, combined_plot, ncol = 1, rel_heights = c(0.5, 9))

# 保存并展示最终结果
ggsave("tianjin_soil_ph_trends.png", final_plot, width = 10, height = 12, dpi = 300)
print(final_plot)
