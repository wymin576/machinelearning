
# ID3 algorithm:information gain
#empirical entroy
rm(list=ls())
setwd('D:\\2022下半年课件\\机器学习\\data')
df <- read.csv('loan.csv')
df <- df[,-1]
#empirical conditional entroy

log2_2 <- function(x) {
  result <- ifelse(x>0,log2(x),0)
  return(result)
}

Bestfeature <- function(dat){
    D <- nrow(dat)
    Ck <- as.numeric(table(dat[,ncol(dat)]))
    HD <- -t(Ck/D) %*% log2(Ck/D)
    
    Di <- list() ;Dik <- list();HDI <- list();
    HDA <- c() ;gda <- c()
    for (i in 1: (ncol(dat)-1)){

      Di[[i]]<- as.numeric(table(dat[,i]))
      Dik[[i]] <- as.matrix.noquote(table(dat[,i],dat[,ncol(dat)]))
      HDI[[i]] <- -rowSums((Dik[[i]]/Di[[i]]) * log2_2(Dik[[i]]/Di[[i]]))
      HDA[i] <- t(Di[[i]]/D)  %*% HDI[[i]]
       gda[i] <- HD-HDA[i]
    }

 return(which.max(gda))
}
Bestfeature(df) #最佳分类变量是3（有房子）

table(df[,Bestfeature(df)],df[,ncol(df)]) #分叉
Bestfeature(df[df[,3] !='是', -Bestfeature(df)]) #最佳分类变量是2（有工作）

# C4.5 algorithm:information gain ratio
Bestfeature2 <- function(dat){
  D <- nrow(dat)
  Ck <- as.numeric(table(dat[,ncol(dat)]))
  HD <- -t(Ck/D) %*% log2(Ck/D)
  
  Di <- list() ;Dik <- list();HDI <- list();
  HDA <- c() ;gda <- c();HAD <- c();igr <- c()
  for (i in 1: (ncol(dat)-1)){
    
    Di[[i]]<- as.numeric(table(dat[,i]))
    Dik[[i]] <- as.matrix.noquote(table(dat[,i],dat[,ncol(dat)]))
    HDI[[i]] <- -rowSums((Dik[[i]]/Di[[i]]) * log2_2(Dik[[i]]/Di[[i]]))
    HDA[i] <- t(Di[[i]]/D)  %*% HDI[[i]]
    gda[i] <- HD-HDA[i]
    HAD[i] <- -(Di[[i]]/D) %*% log2_2((Di[[i]]/D))
    igr[i] <- gda[i]/HAD[i]
  }
  
  return(which.max(igr))
}
Bestfeature2(df)
table(df[,Bestfeature2(df)],df[,ncol(df)]) #分叉
Bestfeature2(df[df[,3] !='是', -Bestfeature2(df)]) #最佳分类变量是2（有工作）

# Cart algorithm
gini1 <- function(x){
gini <- c();tab2 <- list()

for (i in 1:length(unique(x))) {
  df1 <- model.matrix( ~ x - 1, data=df )
  tab2[[i]] <- as.matrix(prop.table(table(df1[,i],df[,ncol(df)]),1))
  gini[i] <- as.numeric(prop.table(table(df1[,1]))) %*% (2*tab2[[i]][,1]*tab2[[i]][,2])
  
}
return(gini[which.min(gini)])

}

gini2 <- function(x){
   tab1 <-  prop.table(table(x))
   tab2 <- prop.table(table(x,df[,ncol(df)]),1)
   gini <- t(tab1) %*%( 2*(tab2[,1]*tab2[,2]))
return(gini)
}

gini <- function(x){
  if (length(unique(x) )> 2) gini1(x) else gini2(x)
}

sapply(df[,-ncol(df)],gini)
   