data(doubs,package = "ade4")
library(tidyverse)
library(dplyr)
env <- doubs$env
env_tb <- as_tibble(rownames_to_column(env,var="site"))
fish <- doubs$fish
fish$total_fish <- rowSums(doubs$fish)
fish <- as_tibble(rownames_to_column(fish,var="site"))
env_fish <- merge(env_tb,fish[,c("site","total_fish")],by="site")
rownames(env_fish) <- env_fish$site
env_fish <- env_fish[,-1]
featurePlot(x = env_fish[,1:11],y=as.factor("test"),layout = c(4,3),plot = "density",scales = list(x = list(relation = "free"),y = list(relation = "free")))
library(skimr)
skim(env_fish[,1:12])
plot(env_fish$total_fish~env_fish$dfs)
abline(lm(env_fish$total_fish~env_fish$dfs))
panel.cor <- function(x, y, ...){
  par(usr = c(0, 1, 0, 1))  
  txt <- as.character(format(cor(x, y), digits = 2))  ### 求相关系数，并保留两位小数
  text(0.5, 0.5, txt, cex = 4 * abs(cor(x, y)))   ### 给定需添加相关系数的位置坐标个字号（cex越大，则数字越大）
}
pairs(~total_fish+dfs+alt+slo+flo,data=env_fish,lower.panel = panel.cor) #dfs flo 相关性较强
pairs(~total_fish+pH+har+pho+nit,data=env_fish,lower.panel = panel.cor)
pairs(~total_fish+amm+oxy+bdo,data=env_fish,lower.panel = panel.cor)
# 去除鱼总数为0值
env_fish <- env_fish[env_fish$total_fish != 0, ]
# 去除NA值 这里本来没有NA
env_fish <- env_fish[complete.cases(env_fish), ]
# 近零方差变量的删除
library(caret)
nzv(env_fish[,1:10])#, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, names = TRUE
corr <- cor(env_fish[,1:10])
rm_col  <-  findCorrelation(corr, cutoff = .75 , names = TRUE)
rm_col
env_fish  <- select(env_fish,-rm_col)
# env_fish <- env_fish[ , !names(env_fish) %in% c("dfs", "pho")]
names(env_fish)
# scale_data <- scale(env_fish[,1:7])
standard <- preProcess(env_fish)  #采用(x-mu)/std的标准化方法，与scale()函数效果一样
scale_data <- predict(standard,env_fish)
# 数据分割
set.seed(100)
idx <- createDataPartition(scale_data$total_fish, p = 0.8, list = FALSE)
train <- scale_data[idx,]
test <- scale_data[-idx,]
names(train)
# options(warin = -1)
# set.seed(100)
# ctrl <- rfeControl(functions = rfFuncs,method = "repeatedcv" , repeats = 5, verbose = FALSE)
# lmProfile <- rfe(x,y,rfeControl = ctrl)
# lmProfile
set.seed(100)
xtrain <- train[,1:7]
ytrain <- train[, 8]
rf_fit <- train(xtrain, ytrain, method="rf")
rf_fit
plot(rf_fit)
rf_pred <- predict(rf_fit,test)
rf_pred
# confusionMatrix(reference = as.factor(test$total_fish),data = rf_pred,mode = "everything")
# test$total_fish
# rf_pred