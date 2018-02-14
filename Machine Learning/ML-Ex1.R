train = read.table("/Users/robinxyuan/Downloads/train.txt",header = TRUE, sep = ";");
test = read.table("/Users/robinxyuan/Downloads/test.txt",header = TRUE, sep = ";")
attach(train);
attach(test);

plot(train)

test_data = data.frame(X_test, Y_test)

p = c(1, 2, 5, 9)
color = c('blue', 'red', 'green', 'yellow')

for (i in 1:4) {
  fit = lm(Y ~ poly(x = X, degree = p[i], raw = T))
  lines(X, fit$fitted.values, col = color[i])
  mse_train = sqrt(mean((Y - fit$fitted.values)^2))
  print(cat("The MSE of ", p[i], " is ", mse_train, "\n"))
  
  pred = predict(fit, test_data)
  mse_test = sqrt(mean((Y_test - pred)^2))
  print(cat("The MSE of test data is ", mse_test, "\n"))
}





