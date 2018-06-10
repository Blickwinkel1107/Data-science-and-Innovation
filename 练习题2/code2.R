# Created by yx

x = seq(20, 65, 5)
y = c(13.2, 15.1, 16.4, 17.1, 17.9, 18.7, 19.6, 21.2, 22.5, 24.3)
df = data.frame(x, y)
mdl = lm(y ~ 1 + x)
lf = function(x){
	return(mdl$coefficients[[1]] + mdl$coefficients[[2]] * x)
}
cat("model: y = ", mdl$coefficients[[1]], " + ", mdl$coefficients[[2]], " * x\n")
print(summary(mdl))
predPoint = data.frame(x = 42)
print(predict(mdl, predPoint, interval = "prediction", level = 0.95))
op = par(mfrow = c(2, 2))
plot(mdl)
par(op)