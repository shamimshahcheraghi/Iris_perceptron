
iris <- iris[sample(1:nrow(iris)),]
data.x <- cbind(iris[,1:4], 1)
data.y <- ifelse(iris$Species == "virginica", 1, 0)

N <- nrow(data.x)
train.x <- data.x[seq(N/2),]
train.y <- data.y[seq(N/2)]

test.x <- data.x [-seq(N/2),]
test.y <- data.y [-seq(N/2)]

perceptron.predict <- function(x, W){
  if(sum(W * x) <= 0)
    return(0)
  else
    return(1)
}



fit <- function(x, W, y, lr){
   p <- perceptron.predict(x, W)
   err <- y-p
   W <<- W + lr * err * x 
   
}

fit.minibatch <- function(x, W, y, lr){
  p <- sapply(1:nrowx, function(i) perceptron.predict(x, W))
  err <- y - p
  W <<- W + lr * rowMeans(err * t(x))
  
}



W <- runif(ncol(train.x))
lr <- 1
acc <- sapply(1:300, function(i) {
  invisible(sapply(1:nrow(train.x), function(i) fit(as.numeric(train.x[i,]), W, train.y[i], lr))) 
  pred <- sapply(1:nrow(test.x), function(i) perceptron.predict(as.numeric(test.x[i,]), W))
  acc <- sum(pred == test.y)/length(test.y)
  acc
  })



