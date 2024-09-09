### cHANG LU ###

### Homework 1                                         ###
### Standard R ###

#Put your code in this file. Make sure you assign the relevant values to the correct variable names, which are given below. 
#Uncomment the variables as you assign your final values/functions/results to them.

### VECTORS ###
## Q1 

v1 <- c(1:8, 7:1)
v2 <- rep(c(9,4,1), length.out = 31)
v3 <- c(rbind(0.1^(seq(3,36, by = 3)), 0.2^(1:12)))

### MATRICES ###

## Q2 

matA<-matrix(0, nrow=6, ncol=6)
matA

diag(matA) =0
diag(matA[-1,-6]) =1
diag(matA[-6,-1]) =1
matA

## Q3

set.seed(42)
matB<- matB <- matrix(sample(1:10, 6*10, replace = TRUE), nrow = 6, ncol = 10)
matB
a3<- apply(matB, 1, function(row) sum(row>4))
a3
b3<-which(apply(matB, 1, function(row) sum(row == 7) == 2))
b3

## Q4

tmpFn<-function(xVec){
  f_x <- ifelse(xVec < 0, xVec^2 + xVec*2 + 3,
                ifelse(xVec < 2, xVec +3,
                       xVec^2 + xVec*4 -7))
  return(f_x)
}

xVec_values <- seq(-3,3, by =0.01)
yVec <- tmpFn(xVec_values)

plot(xVec_values, yVec, type ="l", col ="blue",
     xlab="x", ylab="f(x)", main="f(x) for -3<x<3")


## Q5
# 
quadmap <- function(start, rho, niter){
  x <- numeric(niter)

  x[1] <- start

  for (n in 2:niter){
    x[n] <- rho * x[n-1] * (1-x[n-1])
  }
  return(x)
}

a1 <- quadmap(start = 0.6, rho=2, niter = 1000)
a1

tmp <- quadmap(start = 0.95, rho = 2.99, niter = 500)

plot(tmp, type = "l")

plot(tmp[300:500], type = "l")

## Q6


In1<- function(xVec, yVec){
  zMat <- outer(xVec, yVec, FUN = ">")

  zVec <- rowSums(zMat)

  return(zVec)
}

xVec <- c(1,3,5,7,9)
yVec <- c(1,2,4)
zVec <- In1(xVec,yVec)
zVec




In2<-function(xVec, yVec){
  zVec <- sapply(xVec, function(xVec) sum(yVec < xVec))

  return(zVec)
}
zVec1 <- In2(xVec, yVec)
zVec1

## Q7

fibonacci<-function(n){
  if (n ==1 || n==2 ){
    return(1)
  }

  fibseq <- numeric(n)
  fibseq[1] =1
  fibseq[2]=1

  for (i in 3:n){
    fibseq[i] <- fibseq[i-1] + fibseq[i-2]
  }

  return(fibseq[n])
}

fibonacci(1)
fibonacci(3)
fibonacci(5)

## Q8

n<-1
result <- 1

while (result <= 10000000) {
  n <- n+1
  result <- result * n
}

n

## Q9

mat <- matrix(1:1000000, nrow=100000, ncol=10)

rowAdd<- numeric(nrow(mat))

for (i in 1:nrow(mat)){
  rowAdd[i] <- sum(mat[i,])
}

rowAddsumapply <- apply(mat, 1, sum)
rowAddrowsums <- rowSums(mat)

all.equal(rowAdd, rowAddrowsums)
all.equal(rowAdd, rowAddsumapply)