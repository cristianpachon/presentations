library(xtable)
set.seed(1)
x = rnorm(200, 174)
a = 0.4
b = 0
epsilon = rnorm(200)
y = b + a*x + epsilon

df = data.frame(height = x, weight = y)
xtable(head(df, 10))
plot(x, y, xlab = 'weight', ylab = 'height', pch = 16, cex = 1.3, col = "blue")
lm(y ~x, data = data.frame(x = x, y = y))
abline(lm(y ~x, data = data.frame(x = x, y = y)))
4.0570 + 0.3769*174