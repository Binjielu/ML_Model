require(R6)
require(hash)
require(ramify)
require(MASS)
require(mvtnorm)
require(mvnfast)

NaiveBayes <- R6Class("NaiveBayes", 
                            list(
                              likelihoods = hash(),
                              priors = hash(),
                              w = NULL,
                              k = NULL,
                              fit = function(X, y, mod = TRUE){
                                a <- sum(cov(X))
                                b <- sum(diag(cov(X)))
                                if(b/a < 0.3){
                                  mod = FALSE
                                }
                                if(mod){
                                  self$k <- unique(y) 
                                  for(i in self$k) {
                                  X_k <- X[y==i,1:ncol(X)]
                                  self$likelihoods[[as.character(i)]] <- list(mu=as.matrix(colMeans(X_k), nrow = 1, ncol =2), sigmas = cov(X_k))
                                  self$priors[[as.character(i)]] <- length(X_k) / length(X)  
                                  }
                                }else{
                                  print("Does not exist Naive Bayes classification!")
                                }
                                
                              },
                              
                              predict = function(X, likelihood_method = mvnfunc){
                                P <- array(dim = nrow(X))
                                for(i in ls(self$likelihoods)){
                                  mu <- self$likelihoods[[i]]$mu
                                  sigmas <- self$likelihoods[[i]]$sigmas
                                  a <- likelihood_method(X, mu, sigmas) + log(self$priors[[i]])
                                  P <- cbind(P, a)
                                  colnames(P)[ncol(P)] <- i
                                }
                                b <- argmax(P[,2:ncol(P)])
                                c <- list()
                                for(i in b){
                                  c <- list.append(c, colnames(P[,2:ncol(P)])[i])
                                }
                                return(as.integer(c))
                                
                                }
                            ))

