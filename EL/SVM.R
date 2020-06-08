#! /usr/bin/env Rscript


library(tidyverse)
library(caret)


fold_numbers = 10
val_times = 10


dataset <- read.csv("dataset.csv")

# 划分数据集
train_index <- createDataPartition(
        dataset$健康,
        p = .75,
        list = FALSE,
        times = 1,
)

train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]


# 数据集采样方式
train_control <- trainControl(
        # 使用 10 折交叉检验
        method = "repeatedcv",
        number = fold_numbers,
        # 重复次数
        repeats = val_times
)

# 网格搜索
# 搜索最佳参数
svm_model <- train(
        # 指定标签和特征
        健康 ~ .,
        # 训练集
        data = train_data,
        # 选择算法
        method = "svmRadial",
        # 数据预处理
        preProc = c("center", "scale", "pca"),
        trControl = train_control,
        # 显示过程内容
        verbose = TRUE,
        # 自定义调参网格
        tuneGrid = params_grid
)

predicted <- predict(
        svm_model,
        newdata = test_data
)
Metrics::ce(predicted, test_data$健康)



# 自定义性能度量函数
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 3,
                           # 预测类的概率分布
                           classProbs = TRUE,
                           # 使用 twoClassSummary 函数衡量性能指标
                           summaryFunction = twoClassSummary)


svm_model_roc <- train(
        # 指定标签和特征
        健康 ~ .,
        # 训练集
        data = train_data,
        # 选择算法
        method = "svmRadial",
        # 数据预处理
        preProc = c("center", "scale", "pca"),
        trControl = fitControl,
        # 显示过程内容
        verbose = TRUE,
        # 自定义调参网格
        tuneGrid = params_grid,
        metric = "ROC"
)
predicted_roc <- predict(
        svm_model_roc,
        newdata = test_data
)
Metrics::ce(predicted_roc, test_data$健康)





