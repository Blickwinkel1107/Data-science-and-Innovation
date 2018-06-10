# Created by yx

library(corrplot)
library(psych)
corrMat = cor(mtcars)
corrplot(corrMat)
print(corr.test(mtcars, adjust = "none", use = "complete"))