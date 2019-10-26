require(ggplot2)
require(MASS)
require(ramify)
require(rlist)

uniform_weights <- function(dists, ...){
  k <- length(dists)
  return(rep(1/k, k))
}

reciprocal_weights <- function(dists, epsilon = 1e-2){
  return(1/(dists + epsilon))
}

gauss_weights <- function(dists, ...){
  ed <- exp(dists^2)
  return(ed/sum(ed))
}

R2 <- function(y, y_hat) 1 - sum((y-y_hat)^2)/sum((y-mean(y))^2)

D <- 2
K <- 3
N <- list(length = K*1e3)
M <- as.integer(N)

a <- matrix(0, nrow = 1000)
b <- matrix(-2, nrow = 1000)
c <- matrix(2, nrow = 1000)

x0 <- cbind(c,c)
x1 <- cbind(a,b)
x2 <- cbind(b,c)

X0 <- matrix(rnorm(2e3, sd = 1), nrow = 1e3, ncol = 2) + x0
X1 <- matrix(rnorm(2e3, sd = 1), nrow = 1e3, ncol = 2) + x1
X2 <- matrix(rnorm(2e3, sd = 1), nrow = 1e3, ncol = 2) + x2

X <- rbind(X0,X1,X2)

y0 <- matrix(0, nrow = 1000 , ncol = 1)
y1 <- matrix(1, nrow = 1000 , ncol = 1)
y2 <- matrix(2, nrow = 1000 , ncol = 1)

mu <- as.matrix(colMeans(X0), nrow = 2, ncol = 1)

y <- rbind(y0,y1,y2)

df <- data.frame(X,y)

ggplot(df, aes(X1,X2, color = y)) + geom_point()

knn <- KNN$new()
knn$fit(X, y)
y_hat <- knn$predict(X, K = 15, mod = FALSE)

cat("Training R-squared:", round(R2(y,y_hat), digits = 4))


