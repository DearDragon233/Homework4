library(terra)

# 读取 shapefile（tianjin100）
shp_path <- "Data/Resource/TJ-Landkind(2018)-1m/tianjin100.shp"
tianjin_shp <- vect(shp_path)

# 读取土壤特征的 tif 文件
clay_raster <- rast("Data/Resource/TJ-土壤黏粒含量-250/clay_0_5cm_mean.tif")
nitrogen_raster <- rast("Data/Resource/TJ-土壤全氮含量-250/nitrogen_0_5cm_mean.tif")
ph_raster <- rast("Data/Resource/TJ-土壤酸碱度-250/phh2o_0_5cm_mean.tif")
ocd_raster <- rast("Data/Resource/TJ-土壤有机碳密度-250/ocd_0_5cm_mean.tif")


# 提取每个地块的土壤特征
clay_values <- extract(clay_raster, tianjin_shp, fun = mean, na.rm = TRUE)
nitrogen_values <- extract(nitrogen_raster, tianjin_shp, fun = mean, na.rm = TRUE)
ph_values <- extract(ph_raster, tianjin_shp, fun = mean, na.rm = TRUE)
ocd_values <- extract(ocd_raster, tianjin_shp, fun = mean, na.rm = TRUE)

# 组织成数据框
soil_data <- data.frame(
  clay = clay_values,
  nitrogen = nitrogen_values,
  ph = ph_values,
  ocd = ocd_values,
  亚类 = tianjin_shp$亚类,  # 目标变量
  土类 = tianjin_shp$土类,  # 目标变量
  土纲 = tianjin_shp$土纲   # 目标变量
)

# 查看 soil_data 数据框的列名
names(soil_data)
# 查看提取结果
head(soil_data)

#预测亚类 选择正确的列，提取土壤特征和目标变量
train_data <- soil_data[, c("clay.ID", "nitrogen.ID", "ph.ID", "ocd.ID", "亚类")]
# 检查是否有缺失值
summary(train_data)
# 去除缺失值（如果有）
train_data <- na.omit(train_data)
# 查看准备好的数据
head(train_data)

# 将目标变量 '亚类' 转换为因子类型
train_data$亚类 <- as.factor(train_data$亚类)
# 检查数据类型
str(train_data)

# 加载randomForest包
library(randomForest)
# 训练随机森林模型
rf_model <- randomForest(亚类 ~ clay.ID + nitrogen.ID + ph.ID + ocd.ID, data = train_data)
# 查看模型摘要
print(rf_model)

###OOB是9.23% 石灰性褐土、中性石质土、草甸沼泽土等表现差，进行过采样，尝试提高模型质量

# 如果没有安装这些包，请先安装
install.packages("caret")
install.packages("ROSE")

library(caret)
library(ROSE)

# 设置过采样的训练控制参数
train_control <- trainControl(method = "cv", number = 10, sampling = "up")

# 训练模型并进行过采样
model <- train(亚类 ~ clay.ID + nitrogen.ID + ph.ID + ocd.ID,
               data = train_data,
               method = "rf",  # 使用随机森林方法
               trControl = train_control)

# 查看模型的摘要
print(model)


