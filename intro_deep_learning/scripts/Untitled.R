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
df$pred =  4.0570 + 0.3769*df$height
df$error = (df$pred - df$weight)^2
sqrt(1/200*sum(df$error))
epsilon = 10^(-6)
x0 = 1 
are_close = FALSE
lr = 0.1
i = 1
fx0 = x0^2
df2 = data.frame(x0 = 1, fx0 = fx0, derivative = NA, x1 = NA, fx1 = NA, error = NA)
while(i <= 100 && !are_close){
  
  derivative = 2*x0
  x1 = x0 - lr*derivative
  fx1 = x1^2
  error = abs(fx1 - fx0)
  are_close = error < epsilon
  
  df2 = rbind(df2, data.frame(x0 = x0, fx0 = fx0,derivative = derivative,
                              x1 = x1,   fx1 = fx1, error = error))
  
  x0 = x1
  fx0 = fx1
  i = i +1 
}

xtable(df2)


