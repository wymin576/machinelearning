rm(list=ls())
x <- 0:9;y <- c(rep(1,3),rep(-1,3),rep(1,3),-1)
x;y
z <- NULL;i=1
while (i<10) {
  z[i] <- (x[i]+x[i+1])/2
  i=i+1
}
z
w1 <- rep(1,length(x))/length(x)


myfunc <- function(w){
e <- NULL  
for (i in 1:length(z)){
e[i] <- as.integer(ifelse(x < z[i],1,-1) != y ) %*% w
}
  
g <- ifelse(x < z[which.min(e)],1,-1)

alpha <- log((1-e[which.min(e)])/e[which.min(e)])/2 

w = w*exp(-alpha*y*g)/sum(w*exp(-alpha*y*g))
return(list=list(w=w,alpha=alpha,g=g))
}
w2 <- myfunc(w1);w3 <- myfunc(w2$w)
w2;w3

f2 <- w2$alpha*w2$g + w3$alpha*w3$g

e <- NULL  
for (i in 1:length(z)){
  e[i] <- as.integer(ifelse(x > z[i],1,-1) != y ) %*% w3$w
}
e
g <- ifelse(x > z[which.min(e)],1,-1)

alpha <- log((1-e[which.min(e)])/e[which.min(e)])/2 

w = w3$w*exp(-alpha*y*g)/sum(w3$w*exp(-alpha*y*g))

f3 <-  w2$alpha*w2$g + w3$alpha*w3$g +alpha*g 

sign(f3)==y
