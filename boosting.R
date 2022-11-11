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


# boosting tree
x <- 1:10;y <- c(5.56,5.70,5.91,6.40,6.80,7.05,8.90,8.70,9.00,9.05)

z <- NULL;i=1
while (i<10) {
  z[i] <- (x[i]+x[i+1])/2
  i=i+1
}
z

myfunc <- function(y){
m <- NULL;c1 <- NULL;c2 <- NULL;
for (i in z){
  
  c1[i] <- mean(y[1:i])
  c2[i]<- mean(y[(i+1):(length(x)+1)])
  m[i] <- sum((y[1:i]-c1[i])^2)+sum((y[(i+1):(length(x)+1)]-c2[i])^2)
}
s=which.min(m);s
c1 <-mean(y[1:z[s]]) ;c2 <- mean(y[(z[s]+1):(length(x)+1)])
c1;c2  
t1 <- ifelse(x < z[s],c1,c2)
r2=y-t1;r2;L=sum(r2^2)
result=list(z=z[s],c1=c1,c2=c2,r2=r2,L=L)
return(result)
}
f1 <- myfunc(y);f1
f2 <- myfunc(f1$r2);f2
f3 <- myfunc(f2$r2);f3
f4 <- myfunc(f3$r2);f4
f5 <- myfunc(f4$r2);f5
f6 <- myfunc(f5$r2);f6
