
Pone<-function(n,k,strategy,nreps) {
  SamMatrix<-matrix(nrow=nreps,ncol=2*n)
  for (i in 1:nreps) {
    SamMatrix[i,]<-sample(1:(2*n),2*n)
  }
  
  result<-apply(SamMatrix, 1, func,n=n,strategy=strategy,k=k)
  prob<-sum(result)/nreps
  return(prob)
  
}


func<-function(x,n,strategy,k){
  if(strategy==3){
    card<-sample(x,n)
  }else{
    if (strategy==1){
      b<-k
    }else{
      b<-sample(1:(2*n),1)
    }
    card<-rep(0,n)
    for (i in 1:n) {
      card[i]<-x[b]
      b<-x[b]
    }
  }
  
  if (k %in% card){
    return (1)
  }else{
    return (0)
  }
}


Pone(5,4,1,10000)
Pone(5,4,2,10000)
Pone(5,4,3,10000)


# question 2 method 1 
 Pall <-function(n,strategy,nreps){
  count<- rep(0,nreps)
  for (i in 1:nreps){
  numberOfSuccess<-rep(0,2*n)
     for (k in 1:(2*n)){
     set.seed(i)
     numberOfSuccess[k] <- Pone(n,k,strategy,1)
     }
  if (sum(numberOfSuccess)==2*n){
    count[i]= 1
  }
  }
  return(sum(count)/nreps)
}

system.time(Pall(5,1,10000))
# question 2 method 2
Pall<- function(n,strategy,nreps){
  SamMatrix<-matrix(nrow=nreps,ncol=2*n)
  for (i in 1:nreps) {
    SamMatrix[i,]<-sample(1:(2*n),2*n)
  }
  simulation <- matrix(nrow=nreps,ncol=2*n)
  for(k in 1:(2*n)){
    simulation[,k] <-apply(SamMatrix,1,func,n=n,strategy=strategy,k=k)
  }
  result = apply(simulation,1,prod)
  return(sum(result)/nreps)
}



## question 5
dloop<- function(n,nreps){
  SamMatrix<-matrix(nrow=nreps,ncol=2*n)
  for (i in 1:nreps) {
    SamMatrix[i,]<-sample(1:(2*n),2*n)
  }
  result<-apply(SamMatrix, 1, function66,n=n)
  #print(result)
  
  result66 <- table(result)/nreps
  
  return(result66)
}




function66 <- function(x,n){
k <- 4
b <- k
card<-rep(0,2*n)
for (i in 1:(2*n)){
  card[i]<-x[b]
  b<-x[b]
}
#print(card)
length <- match(k,card)
return(length)
}


dloop(50,10000)


n <- 50
pnohigherthan <- sum(dloop(n,10000)[1:n])

n <- c(50:100)
pnohigherthann <- rep(0,length(n))
for (i in 1:length(n)){
x <- n[i]
pnohigherthann[i] <- sum(dloop(x,1000)[1:50])
}
pnohigherthann

plot(n, pnohigherthann , "l")