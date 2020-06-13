#! /usr/bin/env Rscript


library(tidyverse)
library(caret)
library(caretEnsemble)


fold_numbers = 10
val_times = 3


dataset <- read.csv("dataset.csv")

train_control <- trainControl(
        method = "repeatedcv", 
        number = fold_numbers, 
        repeats = val_times,
        savePredictions = TRUE,
        classProbs = TRUE
)

train_index <- createDataPartition(
        dataset$健康,
        p = .75,
        list = FALSE,
        times = 1,
)
# train_index
# c(1, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
# 17, 19, 20, 22, 23, 24, 25, 26, 27, 30, 32, 33, 34, 
# 35, 36, 38, 40, 41, 42, 43, 46, 47, 48, 49, 52, 53, 
# 55, 58, 59, 61, 62, 63, 64, 66, 67, 69, 70, 71, 72, 
# 74, 75, 76, 77, 79, 80, 82, 83, 84, 85, 86, 89, 90, 
# 91, 93, 94, 95, 96, 97, 98, 99, 100, 102, 104, 105, 
# 107, 108, 109, 111, 113, 115, 116, 117, 118, 119, 
# 120, 121, 122, 124, 127, 128, 129, 132, 133, 134, 
# 135, 136, 138, 140, 141, 142, 143, 144, 145, 146, 
# 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 
# 158, 161, 164, 165, 166, 169, 170, 171, 172, 173, 
# 174, 176, 177, 179, 180, 181, 182, 183, 186, 188, 
# 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 
# 200, 201, 202, 203, 204, 205, 206, 207, 208, 210, 
# 211, 212, 213, 216, 219, 220, 221, 222, 223, 225, 
# 227, 228, 229, 230, 231, 232, 233, 236, 237, 238, 
# 239, 240, 242, 243, 244, 248, 249, 250, 252, 253, 
# 254, 257, 258, 259, 260, 261, 262, 263, 264, 265)

train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]


# 数据集采样方式
train_control <- trainControl(
        # 使用 10 折交叉检验
        method = "repeatedcv",
        number = fold_numbers,
        # 重复次数
        repeats = val_times,
        savePredictions = 'final',
        classProbs = TRUE#,
        # index = createFolds(train_data$健康, fold_numbers)
)


model_list <- c(
        'svmRadialSigma',
        'svmRadialCost',
        'svmPoly',
        'svmLinear2',
        'svmLinearWeights',
        'svmRadialWeights',
        # 线性核支持向量机
        'svmLinear',
        # 高斯核偏置支持向量机
        'svmRadial'
)

# 分别生成模型
models <- caretList(
        健康 ~ .,
        data = train_data,
        trControl = train_control,
        methodList = model_list,
        preProc = c("center", "scale", "pca"),
        tuneList = list(
                svmRadialSigma = caretModelSpec(
                        method="svmRadialSigma",
                        trace = FALSE,
                        tuneGrid = expand.grid(
                                sigma = 2^(-10:-6),
                                C = 15:20
                        )
                ),
                svmRadialCost = caretModelSpec(
                        method="svmRadialCost",
                        trace = FALSE,
                        tuneGrid = expand.grid(C = 1:4)
                ),
                svmPoly = caretModelSpec(
                        method="svmPoly",
                        trace = FALSE,
                        tuneGrid = expand.grid(
                                degree = 1,
                                scale = 1:3,
                                C = 1:4
                        )
                ),
                svmLinear2 = caretModelSpec(
                        method="svmLinear2",
                        trace = FALSE,
                        tuneGrid = expand.grid(cost = 1:3)
                ),
                svmLinear = caretModelSpec(
                        method="svmLinear",
                        trace = FALSE,
                        tuneGrid = expand.grid(C = 1:5)
                ),
                svmRadial = caretModelSpec(
                        method="svmRadial", 
                        trace = FALSE,
                        tuneGrid = expand.grid(sigma = 2^(-9:-5), C = 15:20)
                )

        )
)



# 模型集成
svm_stack <- caretStack(
        models, 
        method = "svmRadial", 
        metric = "Accuracy", 
        trControl = train_control
)

predicted <- predict(
        svm_stack,
        test_data
)

Metrics::ce(predicted, test_data$健康)
# [1] 0.119403

# 混淆矩阵评估
confusionMatrix(
        predicted,
        test_data$健康
)


# > confusionMatrix(
# +         predicted,
# +         test_data$健康
# + )
# Confusion Matrix and Statistics

#           Reference
# Prediction 健康 患病
#       健康   34    5
#       患病    3   25
                                         
#                Accuracy : 0.8806         
#                  95% CI : (0.7782, 0.947)
#     No Information Rate : 0.5522         
#     P-Value [Acc > NIR] : 7.658e-09      
                                         
#                   Kappa : 0.757          
                                         
#  Mcnemar's Test P-Value : 0.7237         
                                         
#             Sensitivity : 0.9189         
#             Specificity : 0.8333         
#          Pos Pred Value : 0.8718         
#          Neg Pred Value : 0.8929         
#              Prevalence : 0.5522         
#          Detection Rate : 0.5075         
#    Detection Prevalence : 0.5821         
#       Balanced Accuracy : 0.8761         
                                         
#        'Positive' Class : 健康    


# ROC 检验曲线
roc_control <- trainControl(
        method = "repeatedcv",
        number = fold_numbers,
        repeats = val_times,
        # 使用 twoClassSummary 函数衡量性能指标
        summaryFunction = twoClassSummary,
        savePredictions = TRUE,
        classProbs = TRUE
)


svm_stack_ROC <- caretStack(
        models, 
        method = "svmRadial", 
        metric = "ROC",
        tuneGrid = expand.grid(sigma = 2^(-10:4), C = 1:20),
        trControl = roc_control
)

res <- svm_stack_ROC$error %>%
        filter(C > 0)
res$C <- factor(res$C)

roc_pic = ggplot(
        data = res,
        mapping = aes(
                x = sigma,
                y = ROC,
                class = C,
                color = C
        )) +
        geom_line()
ggsave(roc_pic, file="ROC.pdf", width = 10, height = 6)

roc_pic
