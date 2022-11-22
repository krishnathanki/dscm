install.packages("ggplot2")

library("ggplot2")
#The predictor vector
x <- c(5.1,5.3,6.1,5.9,6.9,7.2,5.8,4.7,4.9,5.0)

#The response vector
y <- c(63,78,59,64,68,43,57,89,98,77)

#Apply lm function
relation <- lm(y~x)

relation
summary(relation)

#Find weight of a person with height 5.6
a <- data.frame(x=5.6)
result <- predict(relation,a)
print(result)

# Giving a name to the chart file.  
png(file = "linear_regression.png")  
# Plotting the chart.  
plot(y,x,col = "red",main = "Height and Weight Regression",abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")  
# Saving the file.  
dev.off()  
