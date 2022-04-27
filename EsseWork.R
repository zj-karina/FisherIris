library("MASS")#библиотека для линейного дискриминантного анализа
mydat = iris

index <- sample(1:nrow(mydat), round(0.70*nrow(mydat)))
train <- mydat[index,]
test <- mydat[-index,]
str(train)
z <- lda(Species ~ .,data = train)
z
#координаты центроидов
library(candisc)#библиотека для центроидов

iris.mod <- lm(cbind(Petal.Length, Sepal.Length, Petal.Width, Sepal.Width) ~ Species, data=train)

iris.can <- candisc(iris.mod, term="Species")

iris.can$means

#тест М-бокса
help(boxM)
library(biotools)#библиотека для теста М-бокса
boxM(train[, -5], train[, 5])

p=predict(z,test)
p

table(predict(z, type="class")$class, train$Species)