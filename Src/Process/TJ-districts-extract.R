# 加载必要的包
library(sf)
library(terra)
library(randomForest)
library(caret)

# 读取天津市区级行政区划的 JSON 文件
json_path <- "D:\\GithubP\\Homework4\\Data\\Resource\\天津市.json"
tianjin_districts <- st_read(json_path)

# 读取土壤特征的 tif 文件
clay_raster <- rast("Data/Resource/TJ-土壤黏粒含量-250/clay_0_5cm_mean.tif")
nitrogen_raster <- rast("Data/Resource/TJ-土壤全氮含量-250/nitrogen_0_5cm_mean.tif")
ph_raster <- rast("Data/Resource/TJ-土壤酸碱度-250/phh2o_0_5cm_mean.tif")
ocd_raster <- rast("Data/Resource/TJ-土壤有机碳密度-250/ocd_0_5cm_mean.tif")

# 读取数据后统一设置 CRS
# 确保所有数据的 CRS 一致
target_crs <- "EPSG:4326"  # 假设目标 CRS 是 EPSG:4326，你可按需修改
clay_raster <- project(clay_raster, target_crs)
nitrogen_raster <- project(nitrogen_raster, target_crs)
ph_raster <- project(ph_raster, target_crs)
ocd_raster <- project(ocd_raster, target_crs)
tianjin_shp <- project(tianjin_shp, target_crs)

# 重新提取土壤特征数据
clay_values <- extract(clay_raster, tianjin_shp, fun = mean, na.rm = TRUE)
nitrogen_values <- extract(nitrogen_raster, tianjin_shp, fun = mean, na.rm = TRUE)
ph_values <- extract(ph_raster, tianjin_shp, fun = mean, na.rm = TRUE)
ocd_values <- extract(ocd_raster, tianjin_shp, fun = mean, na.rm = TRUE)

# 再次检查各列数据的行数
cat("clay_values 的行数：", length(clay_values), "\n")
cat("nitrogen_values 的行数：", length(nitrogen_values), "\n")
cat("ph_values 的行数：", length(ph_values), "\n")
cat("ocd_values 的行数：", length(ocd_values), "\n")
cat("tianjin_shp 的行数：", nrow(tianjin_shp), "\n")
cat("district_indices 的长度：", length(district_indices), "\n")

# 组织成数据框
soil_land_data <- data.frame(
  clay = clay_values,
  nitrogen = nitrogen_values,
  ph = ph_values,
  ocd = ocd_values,
  亚类 = tianjin_shp$亚类,  # 目标变量
  土类 = tianjin_shp$土类,  # 目标变量
  土纲 = tianjin_shp$土纲,  # 目标变量
  district = sapply(district_indices, function(x) if (length(x) > 0) tianjin_districts$name[x] else NA)
)

# 预测土地类型，选择正确的列，提取土壤特征和目标变量
train_data <- soil_data[, c("clay.ID", "nitrogen.ID", "ph.ID", "ocd.ID", "亚类")]

# 检查是否有缺失值
missing_values <- sapply(train_data, function(x) sum(is.na(x)))
if (any(missing_values > 0)) {
  cat("数据中存在缺失值，以下是各列的缺失值数量：\n")
  print(missing_values)
  train_data <- na.omit(train_data)
}

# 将目标变量 'land_type' 转换为因子类型
train_data$land_type <- as.factor(train_data$land_type)

# 初始化存储每个行政区特征重要性的矩阵
num_districts <- nrow(tianjin_districts)
local_importance <- matrix(0, nrow = num_districts, ncol = 4)
colnames(local_importance) <- c("clay", "nitrogen", "ph", "ocd")

# 循环计算每个行政区的局部特征重要性
for (i in 1:num_districts) {
  district_name <- tianjin_districts$name[i]
  district_subset <- train_data[train_data$district == district_name, ]
  
  # 检查数据是否足够训练模型
  if (nrow(district_subset) > 0) {
    # 训练随机森林模型
    district_model <- randomForest(land_type ~ clay + nitrogen + ph + ocd, data = district_subset)
    # 计算特征重要性
    local_importance[i, ] <- varImp(district_model)$importance
  } else {
    message(paste("行政区", district_name, "的数据不足，跳过该区域。"))
  }
}

# 查看局部特征重要性矩阵
print(local_importance)