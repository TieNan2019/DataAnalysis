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


lpSVM <- list(
        type = "Classification",
        library = "kernlab",
        loop = NULL
)

prm <- data.frame(parameter = c("C", "sigma"),
                  class = rep("numeric", 2),
                  label = c("Cost", "Sigma"))

lpSVM$parameters <- prm

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
lpSVM$grid <- svmGrid

svmFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) { 
        kernlab::ksvm(
                x = as.matrix(x), y = y,
                kernel = "rbfdot",
                kpar = list(sigma = param$sigma),
                C = param$C,
                prob.model = classProbs,
                ...
        )
}
lpSVM$fit <- svmFit

svmPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
        kernlab::predict(modelFit, newdata)
lpSVM$predict <- svmPred

svmProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
        kernlab::predict(modelFit, newdata, type = "probabilities")
lpSVM$prob <- svmProb

svmSort <- function(x) x[order(x$C),]
lpSVM$sort <- svmSort

lpSVM$levels <- function(x) kernlab::lev(x)

function(x) levels(x@data@get("response")[,1])



library(mlbench)
library(caret)

fitControl <- trainControl(method = "repeatedcv",
                           ## 10-fold CV...
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

Laplacian <- train(健康 ~ ., data = dataset, 
                   method = lpSVM, 
                   preProcess = c("center", "scale"),
                   tuneLength = 8,
                   trControl = fitControl)





