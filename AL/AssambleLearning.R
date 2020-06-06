#! /usr/bin/env Rscript

# 设置字体
showtext::showtext_auto(enable = TRUE)
sysfonts::font_add('YaHei', regular = "YaHei.ttf")


# 读取数据
dataset <- read.csv("dataset.csv")

# 处理数据
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


SVM_model <- list(
        type = "Classification",
        library = "kernlab",
        loop = NULL
)

prm <- data.frame(parameter = c("C", "sigma"),
                  class = rep("numeric", 2),
                  label = c("Cost", "Sigma"))

SVM_model$parameters <- prm

svmGrid <- function(x, y, len = NULL, search = "grid") {
        library(kernlab)
        ## This produces low, middle and high values for sigma 
        ## (i.e. a vector with 3 elements). 
        sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)  

        ## 进行网格搜索:
        if(search == "grid") {
                out <- expand.grid(sigma = mean(as.vector(sigmas[-2])),
                                C = 2 ^((1:len) - 3))
        } else {
                # 进行随机搜索，要先定义参数取值范围，随后产生随机值
                rng <- extendrange(log(sigmas), f = .75)
                out <- data.frame(sigma = exp(runif(len, 
                        min = rng[1], max = rng[2])),
                        C = 2^runif(len, min = -5, max = 8))
        }

        out
}
SVM_model$grid <- svmGrid

svmFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) { 
        kernlab::ksvm(
                x = as.matrix(x), y = y,
                # 使用高斯核
                kernel = "rbfdot",
                kpar = list(sigma = param$sigma),
                C = param$C,
                prob.model = classProbs,
                ...
        )
}
SVM_model$fit <- svmFit

svmPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
        kernlab::predict(modelFit, newdata)
SVM_model$predict <- svmPred

svmProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
        kernlab::predict(modelFit, newdata, type = "probabilities")
SVM_model$prob <- svmProb

svmSort <- function(x) x[order(x$C),]
SVM_model$sort <- svmSort

SVM_model$levels <- function(x) kernlab::lev(x)

function(x) levels(x@data@get("response")[,1])



library(mlbench)
library(caret)

params_grid <- expand.grid(sigma = 2^(-10:4), C = 1:20)

fitControl <- trainControl(method = "repeatedcv",
                           # 10 折交叉验证
                           number = 10,
                           # 重复次数
                           repeats = 10)

svm_grid <- train(健康 ~ ., data = dataset, 
                   method = SVM_model, 
                   preProcess = c("center", "scale"),
                   tuneGrid = params_grid,
                   trControl = fitControl)


# 这部分弃用
# library(caretEnsemble)
# library(kernlab)


# # 读取数据
# dataset <- read.csv("heartattack.csv")

# # 处理数据
# library(tidyverse)
# dataset <- dataset %>%
#         mutate_at(
#                 vars("性别", "胸痛", "血糖",
#                 "心电", "心绞痛", "ST斜率",
#                 "主血管", "THAL", "健康"), 
#                 factor) %>%
#         select("年龄", "性别", "胸痛", "血压", "固醇", 
#         "血糖", "心率", "心绞痛", "ST下降", 
#         "ST斜率", "主血管", "THAL", "健康")


# models <- caretList(
#         X14 ~ .,
#         data=dataset,
#         trControl=trainControl(
#                 method="repeatedcv", number=10, 
#                 repeats=3, savePredictions='final', 
#                 classProbs=TRUE
#         ),
#         methodList = c(
#                 'svmRadial', 'lssvmRadial', 
#                 'lssvmPoly', 'lssvmLinear'
#         )
# )

# svm_stacking <- caretStack(
#         method = SVM_model,
#         trControl = fitControl
# )

