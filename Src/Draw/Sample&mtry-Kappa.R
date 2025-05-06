# 加载必要的库
library(ggplot2)

# 定义类别
classes <- c('滨海潮滩盐土', '滨海盐土', '草甸沼泽土', '潮褐土', '潮土', '城市用地', '褐土', '褐土性土', '淋溶褐土', '其它', '砂姜黑土', '湿潮土', '石灰性褐土', '脱潮土', '盐化潮土', '盐化沼泽土', '沼泽土', '中性石质土', '棕壤性土')

# 模拟每个类别的样本数量（这里假设每个类别样本数为10）
sample_counts <- rep(10, length(classes))

# 创建数据框用于类别分布可视化
class_distribution <- data.frame(Class = classes, Count = sample_counts)

# 绘制类别分布柱状图
ggplot(class_distribution, aes(x = Class, y = Count)) +
  geom_col() +
  labs(title = "类别分布柱状图", x = "类别", y = "样本数量") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 重采样结果数据
resampling_results <- data.frame(
  mtry = c(2, 3, 4),
  Accuracy = c(0.9252834, 0.9252834, 0.9252834),
  Kappa = c(0.913728, 0.913728, 0.913728)
)

# 转换数据为长格式以方便绘制折线图
library(tidyr)
resampling_results_long <- pivot_longer(resampling_results, cols = c(Accuracy, Kappa), 
                                        names_to = "Metric", values_to = "Value")

# 绘制重采样结果折线图
ggplot(resampling_results_long, aes(x = mtry, y = Value, color = Metric, group = Metric)) +
  geom_line() +
  geom_point() +
  labs(title = "Fig.1 Resample Result", x = "mtry", y = "值") +
  scale_x_continuous(breaks = resampling_results$mtry)


# 提取特征重要性
importance_scores <- varImp(model)$importance
importance_scores$Feature <- rownames(importance_scores)
# 按重要性排序
importance_scores <- importance_scores[order(-importance_scores$Overall), ]
# 去掉特征名称后面的.ID
importance_scores$Feature <- gsub("\\.ID", "", importance_scores$Feature)

library(scales)
# 创建颠倒顺序且间隔缩小的颜色渐变
n_features <- nrow(importance_scores)
# 缩小渐变间隔，比如将原本的0 - 1范围缩小到0.2 - 0.8
color_palette <- scales::gradient_n_pal(c("#CCCCCC", "#005599"))(seq(0.2, 0.8, length.out = n_features))

# 特征重要性可视化
ggplot(importance_scores, aes(x = reorder(Feature, -Overall), y = Overall, fill = factor(Feature))) +
  geom_col(width = 0.4) +  # 使柱子变细
  scale_fill_manual(values = color_palette) +  # 使用渐变颜色
  labs(title = "Fig.2 Feature Importance Ranking", x = "Feature", y = "Importance") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14, color = "black"),
    plot.title = element_text(size = 16, color = "black", hjust = 0.5),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color = "black"),
    legend.position = "none"  # 移除图例，若不需要可保留
  )


# 提取 OOB 误差
oob_error <- rf_model$err.rate[, 1]
# 树的数量
ntree <- seq_along(oob_error)

# 创建数据框
error_data <- data.frame(
  Trees = ntree,
  OOB_Error = oob_error
)

  #title = "Fig.3 OBB Error-The Number of Trees",
# 获取结束时的 OOB 值
end_oob_value <- tail(oob_error, 1)

# 创建颜色渐变，减小渐变间隔
color_palette <- scales::seq_gradient_pal(low = "#CCCCCC", high = "#005599")(seq(0.3, 1, length.out = length(oob_error)))

# 发表级别的美化绘图
ggplot(error_data, aes(x = Trees, y = OOB_Error)) +
  geom_line(aes(color = Trees)) +
  scale_color_gradientn(colors = color_palette) +
  labs(
    title = "Fig.3 OBB Error-Number of Trees",
    x = "Number of Trees",
    y = "OOB Error"
  ) +
  geom_text(aes(x = max(Trees), y = end_oob_value, label = sprintf("%.3f", end_oob_value)), 
            hjust = 1.2, vjust = 0, size = 5, color = "black") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "none"
  )

