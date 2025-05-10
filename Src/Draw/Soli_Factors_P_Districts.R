# 加载必要包
library(dplyr)
library(ggplot2)
library(foreach)
library(doParallel)
library(broom)

# 提前需运行获得model.R中的summary_long
source("Src/Process/Model.R")
#v分组
group1 <- c("和平区", "河东区", "河西区", "河北区", "南开区", "红桥区")
group2 <- c("滨海新区", "东丽区", "西青区", "北辰区", "武清区", "静海区")
group3 <- c("津南区", "宁河区", "宝坻区", "蓟州区")

# 为每个区分配组
summary_long <- summary_long %>%
  mutate(District = case_when(
    name %in% group1 ~ "Group1",
    name %in% group2 ~ "Group2",
    name %in% group3 ~ "Group3",
    TRUE ~ "Other"
  ),
  Factor = recode(factor,
                  "Organic Carbon Density" = "OCD",
                  "Clay Content" = "Clay",
                  "Nitrogen Content" = "Nitrogen",
                  "pH Value" = "pH"),
  Value = value)

# 设置并行运算
numCores <- parallel::detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

# 嵌套并行运算
results <- foreach(d = unique(summary_long$District), .combine = rbind, .packages = c("dplyr", "broom")) %:% 
  foreach(f = unique(summary_long$Factor), .combine = rbind) %dopar% {
    sub_data <- summary_long %>% filter(District == d, Factor == f)
    model <- lm(Value ~ 1, data = sub_data)
    tidy_out <- broom::tidy(model)
    data.frame(District = d,
               Factor = f,
               Estimate = tidy_out$estimate[1],
               p.value = tidy_out$p.value[1])
  }

# 停止并行
stopCluster(cl)

print(results)

# 加载包
library(dplyr)
library(ggplot2)
library(broom)

# 输入结果数据
results <- data.frame(
  District = c("Group2", "Group2", "Group2", "Group2", "Group1", "Group1", "Group1", "Group1", 
               "Group3", "Group3", "Group3", "Group3", "Other", "Other", "Other", "Other"),
  Factor = rep(c("clay", "ocd", "nitrogen", "ph"), times = 4),  # 去掉了 "_mean"
  Estimate = c(277.99, 227.96, 179.63, 79.63, 299.92, 235.40, 176.10, 79.86, 258.45, 227.07, 186.81, 78.43, 198.32, 214.38, 214.63, 76.03),
  p.value = c(1.78e-07, 4.62e-09, 8.17e-09, 5.04e-12, 5.93e-08, 2.91e-10, 7.70e-15, 6.91e-21, 
              8.26e-04, 1.36e-05, 1.36e-04, 3.10e-06, NA, NA, NA, NA)
)

# 将p值转化为星号
results$p_significance <- ifelse(results$p.value < 2e-15, "4*", 
                                 ifelse(results$p.value < 1e-8, "3*", 
                                        ifelse(results$p.value < 0.005, "2*", 
                                               ifelse(results$p.value < 0.05, "*", "ns"))))
# 对于NA值的p.value，设置为"ns"
results$p_significance[is.na(results$p.value)] <- "ns"

#为星号创建一个新的列，用来显示每个区间对应的星号
results$p_significance_factor <- factor(results$p_significance, 
                                        levels = c("4*", "3*", "2*", "*", "ns"))
# ggplot2启动！
ggplot(results, aes(x = Factor, y = Estimate, fill = District)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = p_significance, color = p_significance_factor), 
            vjust = -0.5, size = 5, position = position_dodge(width = 0.75)) + 
  scale_fill_manual(values = c("Group1" = "lightblue", "Group2" = "lightgreen", "Group3" = "lightcoral", "Other" = "lightgray")) + 
  scale_color_manual(values = c("4*" = "red", "3*" = "lightcoral", "2*" = "orange", "*" = "green", "ns" = "blue")) +  # 为星号设定颜色
  labs(title = "Soil Factor Estimates by District with P-value", x = "Soil Factor", y = "Estimate Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "District"), 
         color = guide_legend(title = "Significance", order = 2)) 

