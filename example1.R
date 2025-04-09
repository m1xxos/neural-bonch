library(neuralnet)

x <- runif(500,-4,4)
y <- 3*x + 4

learn=data.frame(x,y)

nn <- neuralnet(y~x, hidden=0, learn)

plot(nn)

yp=predict(nn, learn)

plot(x,y,col='yellow', pch = 20, cex=5.0)

lines(x,yp,col='black')

