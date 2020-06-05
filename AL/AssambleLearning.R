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
                factor)
