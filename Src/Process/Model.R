# 加载必要包
library(terra)
library(caret)
library(randomForest)
library(ggplot2)
library(ggExtra)
library(ggpmisc)
library(ggpubr)

# 读取 shapefile
shp_path <- "Data/Resource/TJ-Landkind(2018)-1m/tianjin100.shp"
tianjin_shp <- vect(shp_path)

# 读取土壤特征的 tif 文件
clay_raster <- rast("Data/Resource/TJ-土壤黏粒含量-250/clay_0_5cm_mean.tif")
nitrogen_raster <- rast("Data/Resource/TJ-土壤全氮含量-250/nitrogen_0_5cm_mean.tif")
ph_raster <- rast("Data/Resource/TJ-土壤酸碱度-250/phh2o_0_5cm_mean.tif")
ocd_raster <- rast("Data/Resource/TJ-土壤有机碳密度-250/ocd_0_5cm_mean.tif")

# 提取每个地块的土壤特征
clay_values <- extract(clay_raster, tianjin_shp, fun = mean, na.rm = TRUE)[,2]
nitrogen_values <- extract(nitrogen_raster, tianjin_shp, fun = mean, na.rm = TRUE)[,2]
ph_values <- extract(ph_raster, tianjin_shp, fun = mean, na.rm = TRUE)[,2]
ocd_values <- extract(ocd_raster, tianjin_shp, fun = mean, na.rm = TRUE)[,2]

# 组织成数据框
soil_data <- data.frame(
  clay.ID = clay_values,
  nitrogen.ID = nitrogen_values,
  ph.ID = ph_values,
  ocd.ID = ocd_values,
  亚类 = tianjin_shp$亚类
)

# 检查缺失值并去除
soil_data <- na.omit(soil_data)

# 将目标变量转换为因子
soil_data$亚类 <- as.factor(soil_data$亚类)

# 设置过采样的训练控制参数
train_control <- trainControl(method = "cv", number = 10, sampling = "up", savePredictions = "final")

# 训练模型并进行过采样
model <- train(亚类 ~ clay.ID + nitrogen.ID + ph.ID + ocd.ID,
               data = soil_data,
               method = "rf",
               trControl = train_control)

# 打印模型摘要（查看交叉验证性能）
print(model)

# 提取交叉验证结果
resamples <- model$pred

# 计算交叉验证的 Accuracy 和 Kappa
library(caret)
conf_mat_cv <- confusionMatrix(resamples$pred, resamples$obs)
acc_cv <- round(conf_mat_cv$overall['Accuracy'], 3)
kappa_cv <- round(conf_mat_cv$overall['Kappa'], 3)

# 将分类变量编码为数值（方便绘图）
resamples$obs_num <- as.numeric(resamples$obs)
resamples$pred_num <- as.numeric(resamples$pred)

# 清理非数值和NA行
resamples_clean <- resamples[complete.cases(resamples[, c("obs_num", "pred_num")]), ]

# 绘图
g <- ggplot(resamples_clean, aes(x = obs_num, y = pred_num)) +
  geom_jitter(alpha = 0.6, width = 0.2, height = 0.2, color = "#1f78b4") +
  geom_smooth(method = "lm", se = FALSE, color = "#33a02c") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  annotate("text", 
           x = min(resamples_clean$obs_num, na.rm = TRUE), 
           y = max(resamples_clean$pred_num, na.rm = TRUE),
           label = paste0("Accuracy: ", acc_cv, "\nKappa: ", kappa_cv),
           hjust = 0, vjust = 1, size = 6, family = "serif") +
  labs(x = "Observed", y = "Predicted", 
       title = "Cross Validation-New Model") +
  theme_minimal(base_size = 14, base_family = "serif")

# 添加边缘直方图
g1 <- ggMarginal(g, type = "histogram", fill = "transparent")

# 展示图像
print(g1)

# Fig.1 Resample Result 折线图
# 提取重采样结果
resampling_results <- model$results

# 将结果转换为长格式（long format）
resampling_results_long <- reshape2::melt(resampling_results, id.vars = "mtry",
                                          measure.vars = c("Accuracy", "Kappa"),
                                          variable.name = "Metric", value.name = "Value")

# 绘制折线图
ggplot(resampling_results_long, aes(x = mtry, y = Value, color = Metric, group = Metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Resample Result", x = "mtry", y = "值") +
  scale_x_continuous(breaks = resampling_results$mtry) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

# Fig.2 Feature Importance Ranking 特征重要性条形图
hist(soil_data$nitrogen.ID, breaks = 30, main = "Nitrogen Distribution")

# 提取特征重要性
importance_scores <- varImp(model)$importance
importance_scores$Feature <- rownames(importance_scores)

# 按重要性排序
importance_scores <- importance_scores[order(-importance_scores$Overall), ]

# 去掉特征名称后面的.ID
importance_scores$Feature <- gsub("\\.ID", "", importance_scores$Feature)

# 创建渐变色调色板
n_features <- nrow(importance_scores)
color_palette <- scales::gradient_n_pal(c("#CCCCCC", "#005599"))(seq(0.2, 0.8, length.out = n_features))

# 绘制特征重要性图
ggplot(importance_scores, aes(x = reorder(Feature, -Overall), y = Overall, fill = factor(Feature))) +
  geom_col(width = 0.4) +
  scale_fill_manual(values = color_palette) +
  labs(title = "Feature Importance Ranking", x = "Feature", y = "Importance") +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    plot.title = element_text(size = 16, color = "black", hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )


# 用 randomForest 单独训练一次模型（不用 caret::train）
set.seed(123)
rf_model <- randomForest(
  亚类 ~ clay.ID + nitrogen.ID + ph.ID + ocd.ID,
  data = soil_data,
  ntree = 500,  # 500 棵树，默认值
  importance = TRUE
)

# 提取 OOB error
oob_error <- data.frame(
  Trees = 1:rf_model$ntree,
  OOB = rf_model$err.rate[ , "OOB"]
)

# 绘制 OOB error-Number of Trees
oob_plot <- ggplot(oob_error, aes(x = Trees, y = OOB)) +
  geom_line(color = "#e41a1c", size = 1.2) +
  geom_point(color = "#377eb8", size = 1) +
  labs(
    title = "OOB Error-Number of Trees",
    x = "Number of Trees",
    y = "OOB Error Rate"
  ) +
  theme_minimal(base_size = 14, base_family = "serif")

# 展示图像
print(oob_plot)
