# 加载必要的库
library(terra)
library(randomForest)
library(caret)
library(ROSE)
library(ggplot2)
library(scales)
library(raster)

# 计算每个区域的局部特征重要性
local_importance <- matrix(0, nrow = nrow(tianjin_shp), ncol = 4)
colnames(local_importance) <- c("clay.ID", "nitrogen.ID", "ph.ID", "ocd.ID")

for (i in 1:nrow(tianjin_shp)) {
  # 提取子集数据
  subset_indices <- extract(clay_raster, tianjin_shp[i, ], method = "extract", df = TRUE)$ID
  subset_data <- train_data[subset_indices, ]
  
  # 检查子集数据是否为空
  if (nrow(subset_data) > 0) {
    try {
      # 训练子集随机森林模型
      subset_model <- randomForest(亚类 ~ clay.ID + nitrogen.ID + ph.ID + ocd.ID, data = subset_data)
      # 计算特征重要性
      local_importance[i, ] <- varImp(subset_model)$importance
    } catch (e) {
      message(paste("在处理区域", i, "时出现错误:", e$message))
    }
  } else {
    message(paste("区域", i, "的子集数据为空，跳过该区域。"))
  }
}


# 将局部特征重要性添加到地理矢量数据中
tianjin_shp$clay_importance <- local_importance[, "clay.ID"]
tianjin_shp$nitrogen_importance <- local_importance[, "nitrogen.ID"]
tianjin_shp$ph_importance <- local_importance[, "ph.ID"]
tianjin_shp$ocd_importance <- local_importance[, "ocd.ID"]

# 绘制土壤粘粒含量特征重要性空间分布地图
ggplot() +
  geom_sf(data = tianjin_shp, aes(fill = clay_importance)) +
  scale_fill_gradientn(
    colors = c("#CCE5FF", "#3399FF", "#003399"),
    name = "Clay Feature Importance",
    limits = c(min(local_importance[, "clay.ID"]), max(local_importance[, "clay.ID"]))
  ) +
  labs(title = "Spatial Distribution of Clay Feature Importance in Tianjin") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "right"
  )

# 绘制土壤全氮含量特征重要性空间分布地图
ggplot() +
  geom_sf(data = tianjin_shp, aes(fill = nitrogen_importance)) +
  scale_fill_gradientn(
    colors = c("#CCE5FF", "#3399FF", "#003399"),
    name = "Nitrogen Feature Importance",
    limits = c(min(local_importance[, "nitrogen.ID"]), max(local_importance[, "nitrogen.ID"]))
  ) +
  labs(title = "Spatial Distribution of Nitrogen Feature Importance in Tianjin") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "right"
  )

# 绘制土壤酸碱度特征重要性空间分布地图
ggplot() +
  geom_sf(data = tianjin_shp, aes(fill = ph_importance)) +
  scale_fill_gradientn(
    colors = c("#CCE5FF", "#3399FF", "#003399"),
    name = "pH Feature Importance",
    limits = c(min(local_importance[, "ph.ID"]), max(local_importance[, "ph.ID"]))
  ) +
  labs(title = "Spatial Distribution of pH Feature Importance in Tianjin") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "right"
  )

# 绘制土壤有机碳密度特征重要性空间分布地图
ggplot() +
  geom_sf(data = tianjin_shp, aes(fill = ocd_importance)) +
  scale_fill_gradientn(
    colors = c("#CCE5FF", "#3399FF", "#003399"),
    name = "OCD Feature Importance",
    limits = c(min(local_importance[, "ocd.ID"]), max(local_importance[, "ocd.ID"]))
  ) +
  labs(title = "Spatial Distribution of OCD Feature Importance in Tianjin") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "right"
  )