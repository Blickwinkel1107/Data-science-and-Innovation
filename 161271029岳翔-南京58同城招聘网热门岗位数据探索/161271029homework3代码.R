library(stringr)
library(plyr)
library(mice)
library(VIM)
library(ggplot2)
library(corrplot)
df = read.csv("58同城热门岗位信息.csv", stringsAsFactors = F)
theName = df$name
df$X = df$name = df$company = NULL  #删除序号、标题、公司名
df$salary[df$salary == "面议"] = NA
meanExp = meanSalary = eduLevel = rep(0, length(df$salary))
#求出每条招聘信息薪水的均值，忽略面议
for(i in 1:length(df$salary)){
  str = df$salary[i]
  if(is.na(str)){
    meanSalary[i] = NA
  }
  res = str_match_all(str, "\\d+")
  res = res[[1]]
  meanSalary[i] = c(as.numeric(res[1]), as.numeric(res[2])) %>% mean()
}
df = data.frame(df, meanSalary)
#对缺失值(面议)进行插值
dfUninterp = df
df$salary = NULL
df = df %>% mice(seed = 999) %>% complete(action = 5)
df$meanSalary.1 = NULL
#绘制各个特征top 50平均薪水图
for(item in c("address", "type", "edubg", "experience", "welfareNum")){
  meanSalaryOfEach = tapply(df$meanSalary, df[item], mean)
  meanSalaryOfEach = data.frame(rownames(meanSalaryOfEach), meanSalaryOfEach)
  rownames(meanSalaryOfEach) = 1:length(meanSalaryOfEach$rownames.meanSalaryOfEach.)
  names(meanSalaryOfEach) = c(item, "meanSalary")
  elder = meanSalaryOfEach
  meanSalaryOfEach = meanSalaryOfEach[order(elder[2], decreasing = T),]
  base1 = switch (item,
    "address" = ggplot(meanSalaryOfEach[1:50,], aes(x=reorder(address, meanSalary), meanSalary)),
    "type" = ggplot(meanSalaryOfEach[1:50,], aes(x=reorder(type, meanSalary), meanSalary)),
    "edubg" = ggplot(meanSalaryOfEach[1:50,], aes(x=reorder(edubg, meanSalary), meanSalary)),
    "experience" = ggplot(meanSalaryOfEach[1:50,], aes(x=reorder(experience, meanSalary), meanSalary)),
    "welfareNum" = ggplot(meanSalaryOfEach[1:50,], aes(x=reorder(welfareNum, meanSalary), meanSalary))
  )
  plot1 = base1 + geom_bar(stat = "identity", aes(fill=meanSalary)) + theme(axis.text.x = element_text(angle = 90))
  plot1 = plot1 + labs(x=item, title=paste(item,"vs mean salary"))
  plot1 = plot1 + scale_size_area()
  print(plot1)
  meanSalaryOfEach = merge(meanSalaryOfEach, df, all.x = T, by = item)
  base2 = switch (item,
    "address" = ggplot(df,aes(address, meanSalary)),
    "type" = ggplot(df, aes(type, meanSalary)),
    "edubg" = ggplot(df, aes(edubg, meanSalary)),
    "experience" = ggplot(df, aes(experience, meanSalary)),
    "welfareNum" = ggplot(df, aes(welfareNum, meanSalary))
  )
  print(base2 + geom_boxplot())
}

#停！
#学历要求 -> 教育等级
for(i in 1:length(df$edubg)){
  str = df$edubg[i]
  eduLevel[i] = switch (str,
    "不限" = 0,
    "技校" = 3,
    "中专" = 6,
    "高中" = 9,
    "大专" = 12,
    "本科" = 15
  )
}
#经验 -> 经验年
for(i in 1:length(df$edubg)){
  str = df$experience[i]
  if(str %in% c('不限', '10年以上', '1年以下')){
    meanExp[i] = switch (str,
      '10年以上' = 10,
      '1年以下' = 0.5,
      '不限' = 0
    )
    next
  }
  res = str_match_all(str, "\\d+")
  res = res[[1]]
  meanExp[i] = c(as.numeric(res[1]), as.numeric(res[2])) %>% mean()
}
df = data.frame(df, meanExp, meanSalary, eduLevel) #合成新数据框
df$experience = df$salary = df$edubg = NULL #丢弃原列
#地址 -> 所在地公司数量
addressFreq = count(df$address)
names(addressFreq) = c("address", "addressFreq")
df = merge(df, addressFreq, by.x = "address", by.y = "address")
df$address = NULL #删除address
#工作类型 -> 工作频数
typeFreq = count(df$type)
names(typeFreq) = c("type", "typeFreq")
df = merge(df, typeFreq, by.x = "type", by.y = "type")
df$meanSalary.1 = df$type = NULL #删除type
#打印相关关系图
corrplot(cor(df),method="shade",addCoef.col="black",order="AOE")
#K-means对招聘信息品质进行分类
wss = numeric(15) 
for (k in 1:15) 
  wss[k] <- sum(kmeans(df, centers=k, nstart=25)$withinss)
plot(1:15, wss, type="o", xlab="Number of Clusters", ylab="Within Sum of Squares") 
#k = 4时聚类效果最佳
kMeansEquals4 = kmeans(df, centers=4, nstart=25)
#依次绘制二维聚类可视化图表
df$cluster = factor(kMeansEquals4$cluster)
centers=as.data.frame(kMeansEquals4$centers)
g1 = ggplot(data=df, aes(x=meanSalary, y=addressFreq, color=cluster)) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=meanSalary,y=addressFreq, color=as.factor(c(1,2,3,4))), 
             size=10, alpha=0.3, show.legend=FALSE)
g2 = ggplot(data=df, aes(x=meanSalary, y=eduLevel, color=cluster)) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=meanSalary,y=eduLevel, color=as.factor(c(1,2,3,4))), 
             size=10, alpha=0.3, show.legend=FALSE)
g3 = ggplot(data=df, aes(x=meanSalary, y=welfareNum, color=cluster)) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=meanSalary,y=welfareNum, color=as.factor(c(1,2,3,4))), 
             size=10, alpha=0.3, show.legend=FALSE)
g4 = ggplot(data=df, aes(x=meanSalary, y=meanExp, color=cluster)) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=meanSalary,y=meanExp, color=as.factor(c(1,2,3,4))), 
             size=10, alpha=0.3, show.legend=FALSE)
g5 = ggplot(data=df, aes(x=meanSalary, y=typeFreq, color=cluster)) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=meanSalary,y=typeFreq, color=as.factor(c(1,2,3,4))), 
             size=10, alpha=0.3, show.legend=FALSE)
print(g1)
print(g2)
print(g3)
print(g4)
print(g5)
#层次聚类
df$name = theName
df$meanSalary = as.numeric(df$meanSalary)
df = df[order(df$meanSalary, decreasing=T), ]
df = df[1:25, ]
rownames(df) = df$name
hc = hclust(dist(df))
#plot(hc, hang = -1)
#hcd = as.dendrogram(hc)
plot(hc)

