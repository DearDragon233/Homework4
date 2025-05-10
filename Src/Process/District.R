# 加载包
library(terra)
library(sf)
library(dplyr)

# 读取天津市行政区划
admin_sf <- st_read("Data/Resource/TJ-district.json")

# 读取土壤地块数据
shp_path <- "Data/Resource/TJ-Landkind(2018)-1m/tianjin100.shp"
tianjin_shp <- vect(shp_path) |> st_as_sf()  # 转 sf 格式方便空间分析

# 读取土壤因子tif
clay_raster <- rast("Data/Resource/TJ-土壤黏粒含量-250/clay_0_5cm_mean.tif")
nitrogen_raster <- rast("Data/Resource/TJ-土壤全氮含量-250/nitrogen_0_5cm_mean.tif")
ph_raster <- rast("Data/Resource/TJ-土壤酸碱度-250/phh2o_0_5cm_mean.tif")
ocd_raster <- rast("Data/Resource/TJ-土壤有机碳密度-250/ocd_0_5cm_mean.tif")

# 提取每个地块的土壤特征
clay_values <- extract(clay_raster, tianjin_shp, fun = mean, na.rm = TRUE)[,2]
nitrogen_values <- extract(nitrogen_raster, tianjin_shp, fun = mean, na.rm = TRUE)[,2]
ph_values <- extract(ph_raster, tianjin_shp, fun = mean, na.rm = TRUE)[,2]
ocd_values <- extract(ocd_raster, tianjin_shp, fun = mean, na.rm = TRUE)[,2]

# 合成数据框
soil_data <- tianjin_shp %>%
  mutate(
    clay = clay_values,
    nitrogen = nitrogen_values,
    ph = ph_values,
    ocd = ocd_values
  )
# 查看坐标系
st_crs(soil_data)
st_crs(admin_sf)
# 统一 soil_data 到 admin_sf 的坐标系
soil_data <- st_transform(soil_data, st_crs(admin_sf))
# 进行几何修复
admin_sf <- st_make_valid(admin_sf)
sf::sf_use_s2(FALSE)

# 空间叠加，找到每个地块所在的行政区
soil_data <- st_join(soil_data, admin_sf[, c("name")])

# 按区计算土壤因子平均值
summary_by_district <- soil_data %>%
  st_drop_geometry() %>%
  group_by(name) %>%
  summarise(
    clay_mean = mean(clay, na.rm = TRUE),
    nitrogen_mean = mean(nitrogen, na.rm = TRUE),
    ph_mean = mean(ph, na.rm = TRUE),
    ocd_mean = mean(ocd, na.rm = TRUE)
  )

# 调整成长数据格式用于可视化排序
library(tidyr)
summary_long <- summary_by_district %>%
  pivot_longer(cols = ends_with("_mean"), names_to = "factor", values_to = "value") %>%
  group_by(name) %>%
  arrange(desc(value), .by_group = TRUE)

# 可视化部分
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(stringr)

# 去掉 _mean 后缀，并转成英文
summary_long <- summary_long %>%
  mutate(
    factor = str_remove(factor, "_mean"),
    factor = case_when(
      factor == "clay" ~ "Clay",
      factor == "nitrogen" ~ "Nitrogen",
      factor == "ph" ~ "pH",
      factor == "ocd" ~ "OCD", 
      TRUE ~ factor
    )
  )

# 根据地理位置进行分组
split_districts <- list(
  c("和平区", "南开区", "河西区", "河东区", "红桥区", "河北区"),   # 中心区
  c("滨海新区", "北辰区", "东丽区", "西青区", "塘沽区", "蓟州区"),   # 东部区
  c("武清区", "宝坻区", "静海区", "宁河区", "津南区", "NA")       # 西部和北部区域，其实津南是南边的（）
)

# 为每一组画图并
plots <- lapply(split_districts, function(d_group) {
  ggplot(summary_long %>% filter(name %in% d_group), 
         aes(x = reorder(factor, value), y = value, fill = factor)) +
    geom_col(width = 0.6, color = "white") +
    facet_wrap(~ name, scales = "free_y", ncol = 3) +
    coord_flip() +
    scale_fill_viridis(discrete = TRUE, option = "D") +
    labs(
      title = "Importance Rank in Districts",  # 增加标题
      x = "Soil Factor", y = "Average Value", fill = "Soil Factor"
    ) +
    theme_minimal(base_size = 14) +  # 增大整体字体
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      strip.text = element_text(size = 10, face = "bold"),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 8),  # 统一调整x轴文字大小
      legend.position = "left",  # 图例左调
      legend.justification = "top",  # 图例调整为上方
      legend.box = "horizontal",
      legend.text = element_text(size = 8),  # 图例文字大小
      panel.spacing = unit(1, "lines"),
      axis.ticks.x = element_line(size = 0.5)  # 让x轴刻度线可见
    ) +
    scale_x_discrete() +  # 替换为离散的x轴刻度
    scale_fill_manual(values = c("Clay" = "#F4A300", "OCD" = "#D95F02", "Nitrogen" = "#1B9E77", "pH" = "#7570B3")) +  # 设置颜色
    scale_y_continuous(labels = scales::comma)  # Y轴显示格式
}) 

for (i in 1:length(plots)) {
  print(plots[[i]])
}

