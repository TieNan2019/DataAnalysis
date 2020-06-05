#! /usr/bin/env Rscript


# 设置字体
showtext::showtext_auto(enable = TRUE)
sysfonts::font_add('YaHei', regular = "YaHei.ttf")

# 导入数据可视化包
library(tidyverse)

dataset <- read.csv("dataset.csv")
dataset$健康 <- c("健康", "患病")[dataset$健康]
dataset$性别 <- c("女", "男")[dataset$性别 + 1]



dataset <- dataset %>%
        mutate_at(
                vars("性别", "胸痛", "血糖",
                "心电", "心绞痛", "ST斜率",
                "主血管", "THAL", "健康"), 
                factor)


# 病情总览
# plot(dataset)

# "性别", "胸痛", "血糖", "心电", "心绞痛", 
# "ST斜率","主血管", "THAL", "健康"
# 都为因子型的离散型变量

# 检查连续变量关于健康状况的分布
# continuous_variable_plot()

# 与年龄的关系
ggplot(
        dataset,
        mapping = aes(
                x = 年龄,
                shape = 健康,
                color = 健康
        )) +
        geom_density() +
        labs(
                title = "心脏健康状况与年龄的关系",
                xlab = "年龄值",
                caption = "从图中可以看出\n在35到55之间的年龄，患上心脏病可能比较小,
                到55岁的时候，患病概率就会有一个很大的增幅"
        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.position = c(0.9, 0.9)
        )        


# 与血压的关系
ggplot(
        dataset,
        mapping = aes(
                x = 血压,
                shape = 健康,
                color = 健康
        )) +
        geom_density() +
        labs(
                title = "心脏健康状况与静息血压的关系",
                xlab = "静息血压",
                caption = "心脏病患者的血压分布与正常水平基本相符，这项属性可能没有太大的参考意义"
        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.position = c(0.9, 0.9)
        )



# 与固醇的关系
ggplot(
        dataset,
        mapping = aes(
                x = 固醇,
                shape = 健康,
                color = 健康
        )) +
        geom_density() +
        labs(
                title = "心脏健康状况与血浆类固醇含量的关系",
                xlab = "血浆类固醇含量",
                caption = "心脏病患者的血浆类固醇含量略高于正常水平"
        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.position = c(0.9, 0.9)
        )


# 与心率的关系
ggplot(
        dataset,
        mapping = aes(
                x = 心率,
                shape = 健康,
                color = 健康
        )) +
        geom_density() +
        labs(
                title = "心脏健康状况与心率的关系",
                xlab = "最高心率心率",
                caption = "心脏病患者的最高心率明显要低于正常值"
        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.position = c(0.9, 0.9)
        )



# 与 ST 下降值的关系
ggplot(
        dataset,
        mapping = aes(
                x = ST下降,
                shape = 健康,
                color = 健康
        )) +
        geom_density() +
        labs(
                title = "心脏健康状况与ST下降的关系",
                xlab = "运动引起的ST下降",
                caption = "在进行运动时，身体健康的样本ST的下降值明显要低于心脏病患者"
        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.position = c(0.9, 0.9)
        )



# 离散变量
# 性别有关
ggplot(
        dataset,
        mapping = aes(
                x = 性别,
                fill = 健康,
                color = 健康
        )) +
        geom_bar(alpha = 0.6) +
        labs(
                title = "性别和心脏病之间的关联",
                x = "性别",
                caption = "相较于男性，女性患有心脏病的概率更小"

        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.position = c(0.1, 0.9)
        )

# 胸痛类型
ggplot(
        dataset,
        mapping = aes(
                x = 胸痛,
                fill = 健康,
                color = 健康
        )) +
        geom_bar(alpha = 0.6) +
        labs(
                title = "胸痛症状和心脏病之间的关联",
                x = "胸痛症状",
                caption = "胸痛类型无症状的人，反而有更大的可能性患有心脏病"

        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.position = c(0.1, 0.9)
        )



# 空腹血糖
ggplot(
        dataset,
        mapping = aes(
                x = 血糖,
                fill = 健康,
                color = 健康
        )) +
        geom_bar(alpha = 0.6) +
        labs(
                title = "血糖是否 > 120 mg/dl和心脏病之间的关联",
                x = "血糖 > 120 mg/dl",
                caption = "仅从概率上看，可能空腹血糖是否超过阈值对于是否患有心脏病这一事实影响不是很大"

        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.position = c(0.9, 0.9)
        )


# 心电
ggplot(
        dataset,
        mapping = aes(
                x = 心电,
                fill = 健康,
                color = 健康
        )) +
        geom_bar(alpha = 0.6) +
        labs(
                title = "静息心电图结果和心脏病之间的关联",
                x = "静息心电图结果",
                caption = "左心室肥大会将患有心脏病的可能性提高很多，
                           有异常的记录过小，没有办法体现出规律"

        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank()
                # legend.position = c(0.9, 0.9)
        )


# 心绞痛
ggplot(
        dataset,
        mapping = aes(
                x = 心绞痛,
                fill = 健康,
                color = 健康
        )) +
        geom_bar(alpha = 0.6) +
        labs(
                title = "运动型心绞痛和心脏病之间的关联",
                x = "运动型心绞痛",
                caption = "运动型心绞痛对心脏病的患病概率提升很大，应该格外注意"

        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.position = c(0.9, 0.9)
        )



# ST斜率
ggplot(
        dataset,
        mapping = aes(
                x = ST斜率,
                fill = 健康,
                color = 健康
        )) +
        geom_bar(alpha = 0.6) +
        labs(
                title = "ST斜率和心脏病之间的关联",
                x = "最大运动量心电图ST斜率",
                caption = "ST斜率持平时，会有很大的患病风险"
        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.position = c(0.9, 0.9)
        )




# 主血管
ggplot(
        dataset,
        mapping = aes(
                x = 主血管,
                fill = 健康,
                color = 健康
        )) +
        geom_bar(alpha = 0.6) +
        labs(
                title = "主血管和心脏病之间的关联",
                x = "使用荧光法测定的主血管数",
                caption = "主血管数越多，患病的可能性成倍增加"
        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.position = c(0.9, 0.9)
        )




# THAL
ggplot(
        dataset,
        mapping = aes(
                x = THAL,
                fill = 健康,
                color = 健康
        )) +
        geom_bar(alpha = 0.6) +
        labs(
                title = "THAL和心脏病之间的关联",
                x = "THAL",
                caption = "THAL数越大，患病的可能性成倍增加"
        ) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.title.y = element_blank(),
                legend.position = c(0.9, 0.9)
        )

















