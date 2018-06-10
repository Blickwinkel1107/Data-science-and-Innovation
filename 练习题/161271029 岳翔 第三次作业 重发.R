# created by Yue Xiang

library(stringr)
library(dplyr)
df = read.csv("student.csv", header = T, stringsAsFactors = F)
df$stdmath = scale(df$Math) %>% as.vector
df$stdsci = scale(df$Science) %>% as.vector
df$stdeng = scale(df$English) %>% as.vector
for(i in 1: length(df$StuId)){
	df$avgstdscore[i] = c(df$stdmath[i], df$stdsci[i], df$stdeng[i]) %>% mean()
}
bounds = quantile(df$avgstdscore, seq(0.2, 1, 0.2))
ranks = c("A", "B", "C", "D", "E")
for(i in 1: length(df$StuId)){
	score = df$avgstdscore[i]
	for(k in 1:5){
		if(score <= bounds[k]){
			df$rank[i] = ranks[k]
			break
		}
	}
}
for(i in 1: length(df$StuId)){
	name = df$StuName[i]
	res = str_match_all(name, "\\w+")
	df$firstName[i] = res[[1]][1]
	df$lastName[i] = res[[1]][2]
}
df$StuName = NULL
df = arrange(df, df$lastName, df$firstName)
