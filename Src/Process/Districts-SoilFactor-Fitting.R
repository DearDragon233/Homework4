# 加载必要包
install.packages("foreach")
install.packages("doParallel")
library(dplyr)
library(ggplot2)
library(foreach)
library(doParallel)

# 模拟数据（你可以换成真实数据）
set.seed(123)
soil_data <- expand.grid(District = c("A", "B", "C"),
                         Factor = c("Clay", "OCD", "Nitrogen", "pH"),
                         Rep = 1:30) %>%
  mutate(Value = rnorm(n()))

# 设置并行
numCores <- parallel::detectCores() - 1
cl <- makeCluster(numCores)
registerDoParallel(cl)

# 嵌套并行运算
results <- foreach(d = unique(soil_data$District), .combine = rbind, .packages = c("dplyr", "broom")) %:%
  foreach(f = unique(soil_data$Factor), .combine = rbind) %dopar% {
    sub_data <- soil_data %>% filter(District == d, Factor == f)
    model <- lm(Value ~ 1, data = sub_data)
    tidy_out <- broom::tidy(model)
    data.frame(District = d,
               Factor = f,
               Estimate = tidy_out$estimate[1],
               p.value = tidy_out$p.value[1])
  }

# 停止并行
stopCluster(cl)

# 查看结果
print(results)
