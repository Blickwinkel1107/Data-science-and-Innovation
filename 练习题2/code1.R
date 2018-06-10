# created by yx

df = read.csv("ex1.csv", stringsAsFactors = F, header = T)
t.test(x = df$before, y = df$after, paired = T, alternative = "greater")

# 95%置信水平下可以认为药物有降压作用