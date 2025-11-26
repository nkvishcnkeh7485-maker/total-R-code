# fenxi -------------------------------------------------------------------
library(readxl)
library(Matrix)
library(lme4)
library(lmerTest)
library(car)
library(sjPlot)
#data1 <- read_excel("E:/Daiyu19/env_data/环境数据/2017-2021年环境数据处理.xlsx")

setwd('C:/Users/lenovo/Desktop/论文/第二次修订')
data1 <- read_excel('./副本删减版汇总.xlsx', sheet = "2017")

#VIF检验
VIFmodel <- lm(oto~SST+SBT+MLD+Chl+SAL+U+V,data = data1)
vif_values <- vif(VIFmodel)
print(vif_values)
#结果：SST和SBT具有很强的共线性

# 对SST和SBT进行PCA
data1$SST <- as.numeric(data1$SST)
data1$SBT <- as.numeric(data1$SBT)
pca <- prcomp(data1[, c("SST", "SBT")], scale = TRUE)
data1$temp_pc1 <- pca$x[, 1]  # 提取第一主成分

# 检查主成分解释率
summary(pca)  # PC1应解释>80%方差

# 计算PC1与SST/SBT的相关系数
cor(data1$temp_pc1, data1$SST)  # 应接近1或-1
cor(data1$temp_pc1, data$SBT)  # 应接近1或-1




# 分块抽取子样本并检验
n_sub <- 10  # 抽取10个子样本
p_values <- numeric(n_sub)

for (i in 1:n_sub) {
  sample_data <- sample(data1$oto, 5000)  # 每次随机抽取5000个数据
  p_values[i] <- shapiro.test(sample_data)$p.value
}

# 计算拒绝正态性的比例（如p<0.05的比例）
cat("Proportion of subsamples rejecting normality:", mean(p_values < 0.05))




#正态性检验
oto1<-data1$oto
shapiro.test(oto1)
##结果：不符合正态性





#方差齐次性检验
data1$Year<- as.factor(data1$Year)

# 构建包含环境变量和年份的线性模型
model <- lm(oto ~ SST + SBT + SAL + MLD + Chl + U + V + Year, data = data1)
# 检验模型残差的方差齐性
leveneTest(residuals(model) ~ Year, data = data1)
# 注：更推荐使用稳健标准误或非参数方法
#结论：由于 p 值远小于 0.05，我们拒绝原假设，接受备择假设。这表明不同组数据的方差存在显著差异，即数据不满足方差齐性的假设。
#因此，推荐使用非参数的检验方法






#非参数检验oto在不同年间有没有显著差异
kruskal.test(oto~Year,data=data1)
##结果：有差异




#判定time对oto是否有显著影响，前提条件
lm1<-lm(oto~Year,data=data1)
lm2<-lm(oto~1,data=data1)
anova(lm1,lm2)
#结果：time 变量对 oto 有显著影响，加入 time 变量后，模型能够更好地解释 oto 的变异。


model <- lmer( oto ~ SBT + (SBT||time), data = data1, REML=TRUE)
summary(model)
tab_model(model)
model2 <- lmer( oto ~ SST + (SST||time), data = data1, REML=TRUE)
summary(model2)
tab_model(model2)
model3 <- lmer( oto ~ SAL + (SAL||time), data = data1, REML=TRUE)
summary(model2)
tab_model(model3)



model4<-lm(oto ~ SBT,data=data1)
summary(model4)

model5<-lm(oto ~ SAL,data=data1)
summary(model5)

model6<-lm(oto ~ SST,data=data1)



model7 <- lmer(oto~SST+SBT+SAL+MLD+Chl+U+V+(1|time),data = data1)
summary(model7)
tab_model(model7)

model8 <- lmer(oto~SST+SAL+MLD+Chl+U+V+(1|time),data = data1)
summary(model8)
tab_model(model8)


# correlation_PL_Radius ---------------------------------------------------
# 加载必要包
library(mgcv)    # 用于GAM拟合
library(ggplot2) # 用于可视化
library(readxl)  # 用于读取Excel文件

# 步骤1: 读取数据并重命名变量
# 指定文件路径和sheet名称（根据您的要求，sheet="Sheet1"）
data <- read_excel("E:/Daiyu19/在用数据/汇总.xlsx", sheet = "Sheet1")
# 检查数据结构结果
head(data) 

# 步骤2: 拟合GAM模型以捕获年龄的非线性效应
# 2.1 对耳石长度（oto_length）与年龄（age）的关系拟合GAM
gam_oto <- gam(R ~ s(age, bs = "cs"), data = data, method = "REML")
# 提取耳石长度的残差（去除年龄效应后的变异）
data$resid_oto <- residuals(gam_oto)

# 2.2 对体长（body_length）与年龄（age）的关系拟合GAM
gam_body <- gam(PL ~ s(age, bs = "cs"), data = data, method = "REML")
# 提取体长的残差（去除年龄效应后的变异）
data$resid_body <- residuals(gam_body)

# 步骤3: 执行残差回归分析 - 测试耳石与体长生长的独立关联
lm_resid <- lm(resid_oto ~ resid_body, data = data)
# 查看回归结果：关注系数、p值和R²
summary(lm_resid)
# 预期输出：如果p值 < 0.05且R²较高，表明耳石生长能显著解释体长变异，支持代理关系

# 步骤4: 可视化残差关系（散点图与回归线）
ggplot(data, aes(x = resid_body, y = resid_oto)) +
  geom_point(alpha = 0.6, color = "blue", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1.2) +
  labs(x = "Residuals of Body Length (from GAM vs Age)", 
       y = "Residuals of Otolith Length (from GAM vs Age)",
       title = "Residual Regression: Otolith vs Somatic Growth",
       subtitle = "Data from Sheet1 of 汇总.xlsx") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# 步骤5: 回归诊断图（检查线性、正态性等假设）
par(mfrow = c(2, 2))  # 设置2x2图形布局
plot(lm_resid)  # 生成诊断图（Q-Q图、残差vs拟合值图等）
# 解读诊断图：确保残差随机分布（无模式）、Q-Q点近似直线（正态性）
# S2-growth individual viration -------------------------------------------
library(ggplot2)
library(dplyr)
library(readxl)
# 读取数据
setwd('H:/Daiyu19/在用数据')
data <- read.table('./dataGAM17-21.txt', sep = ' ', header = TRUE)

# 确保Year是因子类型
data$Year <- factor(data$Year, levels = 2017:2021)

# 创建年龄分段（仅限0-110天）
break_points <- seq(0, 110, by = 10)  # 上限设为110
labels <- paste0(head(break_points, -1), "-", tail(break_points, -1))
data <- data %>%
  filter(IWDay >= 0 & IWDay <= 110) %>%  # 过滤0-110天的数据
  mutate(Age_Group = cut(IWDay, breaks = break_points, labels = labels, include.lowest = TRUE))

# 定义自定义颜色方案
year_colors <- c("#c0504d", "#8064a2", "#4f81bd", "#4bacc6", "#9bbb59")
names(year_colors) <- 2017:2021

# 创建簇状箱形图（仅显示0-110天）
final_plot <- ggplot(data, aes(x = Age_Group, y = IW, fill = Year)) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.7,
    outlier.shape = 1,
    outlier.size = 1.5,
    outlier.alpha = 0.5
  ) +
  geom_point(
    aes(color = Year),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    size = 0.8,
    alpha = 0.3
  ) +
  scale_fill_manual(values = year_colors, name = "Year") +
  scale_color_manual(values = year_colors, guide = "none") +
  labs(
    x = "Age Group (days)",
    y = "Otolith Daily Increment Width (μm)",
    caption = "Fig. S1 Distribution of Otolith Growth by Age Group and Year"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    text = element_text(family = "serif", size = 12),
    plot.caption = element_text(
      hjust = 0.5,
      size = 14,
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.25)
  )

final_plot

# 输出图形
save_path <- "H:/Daiyu19/導出圖片及PDF/clustered box.pdf"
ggsave(filename = save_path, plot = final_plot, width = 10, height = 6, dpi = 600)

# growth change 2 ---------------------------------------------------------
# 加载必要的包
library(mgcv)
library(ggplot2)
library(patchwork)
library(grid)
library(ggpubr)

setwd('E:/Daiyu19/在用数据')
# 读取数据（假设GamData已加载）
GamData <- read.table('./dataGAM17-21.txt', sep = ' ', header = TRUE)
GamData$Year <- as.factor(GamData$Year)  # 将Year转换为因子

custom_colors <- c( "#c0504d", "#8064a2", "#4f81bd","#4bacc6", "#9bbb59")

# 1. 拟合GAM模型，将年份作为固定因子
# 使用by参数为每个年份创建独立的平滑项，并添加Year作为固定效应
#gam_model <- gam(IW ~ s(IWDay, bs = "cs") + s(Year, bs = "re"), data = GamData, method = "REML")
gam_model <- gam(IW ~ s(IWDay, bs = "cs", by = as.factor(Year)) + as.factor(Year), data = GamData, method = "REML")
# 2. 检验年份的显著性
summary(gam_model)  # 查看模型摘要，检查Year的p值
anova_result <- anova(gam_model)  # ANOVA测试
print(anova_result)

p1 <- ggplot(data = GamData, mapping = aes(x = IWDay, y = IW))+ 
  scale_x_continuous(limits=c(0,110),breaks=seq(0,110,10))+
  labs(x = "Age (days)", y = "Otolith daily increment width (μm)")+
  ylim(0,8)+
  geom_smooth(mapping = aes(color = as.factor(Year)),method = "gam", formula = y ~ s(x, bs = "cs"), span = 0.40, se = TRUE, size = 1.2 )+
  # 添加垂直参考线
  geom_vline(xintercept = 40, linetype = "dashed", color = "gray40", size = 0.7) + # <--- 主要添加这行
  scale_color_manual(name="    ",values = custom_colors)+
  theme_minimal()+
  theme(panel.grid.major = element_line(colour = "gray40",linetype = "dashed"), panel.background = element_rect(fill = "aliceblue")) + 
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.background = element_rect(fill = NA)) + 
  theme(plot.subtitle = element_text(family = "serif",size = 12), 
        plot.title = element_text(family = "serif"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_text(family = "serif",size = 13,face = "bold"), 
        axis.text = element_text(colour = "black",size = 11),
        axis.text.x = element_text(colour = "black",size = 11),
        axis.text.y = element_text(colour = "black",size = 11),
        legend.text = element_text(size = 11,family = "serif",face = "bold"),
        legend.title = element_text(size = 13,family = "serif",face = "bold"), 
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank())+
  theme(text = element_text(family = "serif"))

p1

# save_path <- "E:/Daiyu19/導出圖片及PDF/growth change2.pdf"
# ggsave(filename = save_path, plot = p1, width = 10.5, height = 4.5,dpi = 600)






# GAMM --------------------------------------------------------------------
library(mgcv)
library(sjPlot)
library(readxl)

setwd('C:/Users/lenovo/Desktop/论文/第二次修订')
data1 <- read_excel('./副本删减版汇总.xlsx', sheet = "2017")

# 确保随机效应变量为因子
data1$Year <- as.factor(data1$Year)
data1$ID <- as.factor(data1$ID)

# GAMM模型
model_final <- gamm(
  oto ~ s(Age, k=3)+s(SST, k = 3) + s(SBT, k = 3) + s(SAL, k = 3) +
    s(MLD, k = 3) + s(Chl, k = 3) + U + V +
    s(Year, bs = "re") + s(ID, bs = "re"),
  data = data1,
  method = "REML"
)

# 模型结果
summary(model_final$gam)
tab_model(model_final$gam)

# ANOVA显著性检验
anova(model_final$gam)

# 平滑项edf排名
smooth_terms <- summary(model_final$gam)$s.table
smooth_ranking <- smooth_terms[order(-smooth_terms[, "edf"]), ]
print("平滑项基于edf排名:")
print(smooth_ranking)

# 可视化
plot(model_final$gam, pages=1, all.terms=TRUE)



# influence+pdp -----------------------------------------------------------
library(dismo)
library(gbm)
library(readxl)
library(grid)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(pdp)
library(RColorBrewer)
library(tidyr)
library(ggsci)
library(dplyr)
#library(caret)  # 交叉验证

par(mfrow = c(2,4))
setwd('E:/Daiyu19/env_data/环境数据')

Oto1 <- read_excel('./2017-2021年环境数据处理.xlsx', sheet = "2017-2021")

# # 对SST和SBT进行PCA
# Oto1$SST <- as.numeric(Oto1$SST)
# Oto1$SBT <- as.numeric(Oto1$SBT)
# pca <- prcomp(Oto1[, c("SST", "SBT")], scale = TRUE)
# Oto1$Tem <- pca$x[, 1]  # 提取第一主成分


Oto1 <- na.omit(Oto1)
gbm_model <- gbm(oto~SST+SBT+MLD+Chl+SAL+U+V, data = Oto1, 
                 distribution = "gaussian", 
                 n.trees = 200, 
                 shrinkage = 0.01,
                 interaction.depth=3,
                 bag.fraction = 0.5)
infl_p <- summary(gbm_model)
#infl_p


infl_p <- infl_p[order(-infl_p$rel.inf), ]
infl_p$var <- factor(infl_p$var, levels = rev(infl_p$var[order(-infl_p$rel.inf)]))

add_label <- function(plot_obj, label_text, size = 12) {
  grid::grid.draw(plot_obj)
  vp <- grid::viewport(x = 0.02, y = 0.98, width = 0.1, height = 0.1, just = c("left", "top"))
  grid::grid.text(label_text, vp = vp, gp = grid::gpar(fontsize = size))
  return(plot_obj)
}


p1 <- ggplot(data = infl_p, aes(x = var, y = rel.inf, fill="green")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.3 + 0.1),width = 0.7) +
  labs(#title = "变量相对重要性",
    y = "Relative influence (%)",
    x = " Environmental variables",
    fill = "变量类型") +
  scale_fill_manual(values =  "#8CA3C3") +
  theme_minimal()+
  scale_y_continuous(limits=c(0,70),breaks=seq(0,70,by=10),expand = expansion(mult=c(0,0)))+
  coord_flip()  +
  theme(axis.text.y = element_text(hjust = 1) )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(family = "serif",size = 12,face = 'bold'), 
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)),
        axis.text = element_text(colour = "black",size = 10),
        # axis.text.x = element_text(colour = "black",size = 11),
        # axis.text.y = element_text(colour = "black",size = 11),
        axis.ticks.x=element_line(),
        axis.line.x = element_line())+
  theme(text = element_text(family = "serif"))+
  theme(axis.ticks.length = unit(c(0,rep(0.2,5),0),"cm"))+
  theme(legend.position="none")+
  annotate("text", x = -Inf, y = Inf, label = "(a)", vjust = 1.5, hjust = -0.1, size = 12)+
  theme(plot.margin = margin(l = -2.2,t=0.5, b=-0.5,unit = "cm"))  # 减少左侧边距

p1 <- annotate_figure(p1,left = text_grob("(a)",family = "serif", x = 0.9, y = 0.98, rot = 0, vjust = 0.5, hjust = 0, size = 12,face = "bold"))



# 绘制每个自变量的部分依赖图
variables <- c("SST","SBT", "SAL", "MLD", "Chl", "U", "V")
# for (var in variables) {
#   pdp_plot <- partial(gbm_model, pred.var = var, grid.resolution = 100,n.trees=200) %>%
#     autoplot() +
#     ggtitle(paste("Partial Dependence Plot for", var))
#   print(pdp_plot)
# }

pdp_list <- list()

for (i in seq_along(variables)) {
  var <- variables[i]
  pdp_data <- partial(gbm_model, pred.var = var, grid.resolution = 100, n.trees = 200)
  
  pdp_list[[i]] <- ggplot(pdp_data, aes_string(x = var, y = "yhat")) +
    geom_line(color = "#8CA3C3", size = 1) +
    labs(title = paste0("(", letters[i+1], ") "),  # 自动生成子图标签
         x = case_when(
           var == "SST" ~ "SST (°C)",
           var == "SBT" ~ "SBT (°C)", 
           var == "SAL" ~ "SAL (PSU)",
           var == "MLD" ~ "MLD (m)",
           var == "Chl" ~ "Chl (μg/L)",
           var == "U" ~ "U (m/s)",
           var == "V" ~ "V (m/s)",
           TRUE ~ var
         ),  y = "Partial dependence on increment width (µm)") +
    theme_minimal() +
    theme(
      aspect.ratio = 1.2,
      plot.title = element_text(size = 10, hjust = 0.1,family = "serif",face = "bold"),
      axis.title = element_text(family = "serif",size = 12,face = 'bold'), 
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.text = element_text(colour = "black",size = 10),
      axis.ticks=element_line(),
      axis.line = element_line(),
      text = element_text(family = "serif"),
      panel.grid.major = element_line(linetype = "blank"),
      panel.background = element_rect(fill = NA))
}


pdp_theme <- theme(plot.title=element_text(margin=margin(t=5,b=-10,l=-16),
                                           vjust=0.2,
                                           family = "serif", 
                                           size=12),
                   plot.margin = margin(t=5, r=5, b=5, l=5), # 上边距-5pt
                   panel.spacing = unit(0.2, "cm"))
pdp_list <- lapply(pdp_list, function(p) p + pdp_theme)

# 拼合图形
final_plot <- wrap_plots(
  p1, pdp_list[[1]], pdp_list[[2]], pdp_list[[4]],  # 第一行：a + b + c + d
  pdp_list[[3]], pdp_list[[5]], pdp_list[[6]], pdp_list[[7]], # 第二行：e + f + g + h
  nrow = 2, 
  widths = c(1, 1, 1, 1)  # 调整第一列宽度使重要性图更宽
) 
# 输出图形
print(final_plot)
# 保存组合图形
ggsave("influence and pdp_3.png", plot = final_plot, width = 12, height = 7.8,dpi = 600)



# clustered box -----------------------------------------------------------

library(readxl)
library(ggplot2)
library(tidyr)
library(patchwork)
library(cowplot)
# 设置工作目录
setwd("E:/Daiyu19/在用数据")

# 读取数据
#data_1 <- read_excel("./生长-环境（簇状箱形图）.xlsx", sheet = "Sheet1")
#data_1 <- read_excel("./生长-环境（簇状箱形图）.xlsx", sheet = "Sheet3")
data_1 <- read_excel("./生长-环境（簇状箱形图）.xlsx", sheet = "Sheet4")
# 转换日期格式并提取年和月
data_1$Date <- as.Date(data_1$Date)
data_1$Year <- format(data_1$Date, "%Y")
data_1$month <- format(data_1$Date, "%m")

# 补充所有 month-Year 组合，无数据的用 NA 填充
data_1 <- data_1 %>% 
  complete(month = sprintf("%02d", 3:10),  # 强制所有月份组合
           Year = unique(Year), 
           fill = list(IW = NA, SST = NA, SBT = NA)) 

# 规范因子顺序
data_1$month <- factor(data_1$month, levels = sprintf("%02d", 3:10))
data_1$Year <- factor(data_1$Year, levels = sort(unique(data_1$Year)))

# 定义颜色方案
year_colors <- c( "#c0504d", "#8064a2", "#4f81bd","#4bacc6", "#9bbb59")

# 月份标签中英文对照
month_labels <- setNames(
  c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"),
  sprintf("%02d", 3:10)
)



# 修改后的绘图函数（所有子图默认不显示图例）
draw_boxplot <- function(data, y_var, y_label, tag_label, show_x_axis=FALSE) {
  p <- ggplot(data, aes(x = month, y = .data[[y_var]], fill = Year)) +
    geom_boxplot(
      width = 0.65,
      position = position_dodge2(preserve = "single", width = 0.6),
      na.rm = TRUE,
      color="black",
      fatten=1,
      coef=1.5,
      outliner.shape = NA,
      size=0.3
    ) +
    scale_fill_manual(values = year_colors) +
    labs(x = if(show_x_axis) "Month" else NULL, y = y_label) +
    scale_x_discrete(labels = month_labels) +
    theme_bw(base_family = "serif") +
    theme(
      axis.text.x = element_text(
        color = ifelse(show_x_axis, "black", "white"),
        size = 10, 
        face = "bold"
      ),
      axis.title = element_text(size = 13, face = "bold"),
      axis.text.y = element_text(size = 10, face = "bold"),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(2, 5, 2, 5, "pt"),
      plot.tag.position = c(0, 1.0),
      legend.position = "none"  # 所有子图默认不显示图例
    ) +
    geom_vline(
      xintercept = seq(1.5, 7.5, by = 1), 
      linetype = "dashed", 
      color = "gray", 
      linewidth = 0.2
    ) +
    labs(tag = tag_label) +
    theme(plot.tag = element_text(
      size = 12, 
      face = "bold",
      hjust = 0,
      vjust = 1,
      margin = margin(r = 5)
    ))
  
  if(!show_x_axis) {
    p <- p + theme(axis.ticks.x = element_blank())
  }
  return(p)
}

# # 生成所有子图（都不含图例）
# p1 <- draw_boxplot(data_1, "IW",  "IW (μm)",  "(a)")
# p2 <- draw_boxplot(data_1, "SST", "SST (°C)",   "(b)")
# p3 <- draw_boxplot(data_1, "SBT", "SBT (°C)",   "(c)")
# p4 <- draw_boxplot(data_1, "MLD", "MLD (m)",    "(d)")
# p5 <- draw_boxplot(data_1, "SAL", "SAL (psu)",  "(e)")
# p6 <- draw_boxplot(data_1, "Chl", "Chl (µg/L)", "(f)", show_x_axis=TRUE) +
#   theme(axis.title.x = element_text(margin = margin(t = 15)))  # 增加标题上边距


# 生成所有子图（都不含图例）
p1 <- draw_boxplot(data_1, "IW",  "IW ",  "(a)")
p2 <- draw_boxplot(data_1, "SST", "SST",   "(b)")
p3 <- draw_boxplot(data_1, "SBT", "SBT",   "(c)")
p4 <- draw_boxplot(data_1, "MLD", "MLD",    "(d)")
p5 <- draw_boxplot(data_1, "SAL", "SAL",  "(e)")
p6 <- draw_boxplot(data_1, "Chl", "Chl", "(f)", show_x_axis=TRUE) +
  theme(axis.title.x = element_text(margin = margin(t = 15)))  # 增加标题上边距










# 创建独立图例
legend_plot <- ggplot(data_1, aes(x = month, y = IW, fill = Year)) +
  geom_boxplot() +
  scale_fill_manual(values = year_colors) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(family="serif",size = 12, face = "bold"),
    legend.text = element_text(family="serif",size = 10)
  )

legend_grob <- get_legend(legend_plot)

# 组合图形
final_plot <- (p1 / p2 / p3 / p4 / p5 / p6) / 
  wrap_elements(full = legend_grob) +
  plot_layout(
    heights = c(rep(1, 6), 0.3),  # 前6个子图占1单位，图例占0.3单位
    guides = "keep"
  ) &
  theme(plot.margin = margin(5, 15, 5, 5, "pt"))



print(final_plot)

# 输出图形

#save_path <- "E:/Daiyu19/導出圖片及PDF/clustered box2.pdf"
#ggsave(filename = save_path, plot = final_plot, width = 6, height = 10,dpi = 600)


