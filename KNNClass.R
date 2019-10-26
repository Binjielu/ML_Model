require(R6)
require(ramify)


KNN <- R6Class("KNN", 
               list(
                 X = NULL,
                 y = NULL,
                 fit = function(X,y) {
                   self$X = X
                   self$y = y
                 },
                 
                 predict = function(X, K = 9, p = 2, weight_func = gauss_weights, epsilon = 1e-2, mod = TRUE){
                   N <- nrow(X)
                   y_hat <- list()
                   
                   for(i in 1:N){
                     d <- X[i,]
                     for(k in 2:N){
                       d <- rbind(d, X[i,])
                     }
                     dist <- (rowSums((self$X - d)^p))^(1/p)
                     idx <- order(dist)[1:K]
                     gammas <- weight_func(dist[idx], epsilon)
                     if(mod){
                       y_hat <- list.append(y_hat, as.integer((gammas%*%self$y[idx])/sum(gammas)))
                     }else{
                       u <- unique(self$y[idx])
                       #print(u)
                       l <- list()
                       n <- list()
                       for(j in u){
                         l <- list.append(l, sum(gammas[which(self$y[idx]==j)]))
                         n <- list.append(n, j)
                       }
                       ma <- argmax(t(as.matrix(l)))
                       
                       y_hat <- list.append(y_hat, as.integer(n[ma]))
                     }
                   }
                   return(as.integer(y_hat))
                 }
                 
               ))
