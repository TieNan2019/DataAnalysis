#! /usr/bin/env Rscript


# 设置字体
showtext::showtext_auto(enable = TRUE)
sysfonts::font_add('YaHei', regular = "YaHei.ttf")

# 读取数据
dataset <- read.csv("dataset.csv")
dataset$健康 <- c("健康", "患病")[dataset$健康]
dataset$性别 <- c("女", "男")[dataset$性别 + 1]
dataset$健康 <- factor(dataset$健康)

# dataset$健康 <- factor(dataset$健康)

library(tidyverse)
dataset <- dataset %>%
        mutate_at( 
                vars("性别", "胸痛", "血糖",
                "心电", "心绞痛", "ST斜率",
                "主血管", "THAL", "健康"), 
                factor) %>%
        select("年龄", "性别", "胸痛", "血压", "固醇", 
        "血糖", "心率", "心绞痛", "ST下降", 
        "ST斜率", "主血管", "THAL", "健康")

summary(dataset$心电)

# 划分数据集
ratio <- 0.25
rows <- nrow(dataset)
test_data_index <- sample(1:rows, round(ratio*rows), replace = FALSE)

test_data <- dataset[test_data_index, ]
train_data <- dataset[-test_data_index, ]



# (高斯)径向基函数核支持向量机
# SVM 网格搜索
library(caret)
params_grid <- expand.grid(sigma = 2^(-10:4), C = -5:20)
# 心电 的值大多数为

svm_grid <- train(
        健康 ~ .,
        data = dataset,
        method = "svmRadial",
        preProc = c("center", "scale"),
        tuneGrid = params_grid,
)





svm_grid$bestTune
#    sigma        C
# 22 0.0009765625 16



# 用弱 SVM 集成，尽量利用可视化得到的结果
# 可以考虑做一次 PAC

library(kernlab)

svmc <- ksvm(
        健康 ~ .,
        train_data,
        C = 16,
        gamma = 0.0009765625
        # C = svm_grid$bestTune$C,
        # gamma = svm_grid$bestTune$sigma,
)
predicted <- predict(
        svmc,
        newdata = test_data,
        type = "response"
)
Metrics::ce(test_data$健康, predicted)



# 随机森林
rand_forest <- randomForest::randomForest(
        健康 ~ .,
        ntree = 500,
        train_data
)

predicted_test <- predict(
        rand_forest,
        newdata = test_data,
        type = "response"
)
Metrics::ce(test_data$健康, predicted_test)
# 错误率 : [1] 17.64706 %



