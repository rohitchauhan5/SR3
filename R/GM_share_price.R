library(ggplot2)
library(zoo)

GM1 <- read.csv("../data/R-Project data set (raw 12-03-18) - GM 4Q10-3Q18.csv")
GM1 <- na.omit(GM1)
colnames(GM1) <- c("Date", "Price")

GM1$Date <- as.Date( GM1$Date, '%m/%d/%Y')

base <- ggplot(GM1, aes(Date, Price)) + geom_line()
base

GM2 <- read.zoo(GM1, format = "%Y-%m-%d")
plot(GM2)