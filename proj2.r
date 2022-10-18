
# To calculate the probability of a single prisoner's success
# Input: n (half number of boxes), k (the prisoner’s number), strategy (1, 2 or 3) 
# and nreps (the number of replicate simulations)
# The function should return the estimated probability, which should be a number
# between 0 and 1.
Pone<-function(n,k,strategy,nreps) {
  
  # generate the sample matrix, which records the card numbers of each simulation.
  # The number of columns represents the number of boxes and the number of rows 
  # represents the number of simulations. 
  SamMatrix<-matrix(nrow=nreps,ncol=2*n)
  for (i in 1:nreps) {
    SamMatrix[i,]<-sample(1:(2*n),2*n)
  }
  
  # Judge whether the prisoner k is successful in each simulation
  result<-apply(SamMatrix, 1, func,n=n,k=k,strategy=strategy)
  prob<-sum(result)/nreps
  return(prob)
}


# Judge whether the prisoner k is successful.
# Input: x is a vector and the other arguments are the same as Pone.
# If the prisoner succeeds, the function will return 1. If not, return 0.
func<-function(x,n,k,strategy){
  
  # for each strategy, we generate a vector card to record the card numbers which
  # the prisoner has found in n times.
  if(strategy==3){
    card<-sample(x,n)
  }else{
    # find first box number
    if (strategy==1){
      b<-k  
    }else{
      b<-sample(1:(2*n),1)
    }
    # find all card numbers in n times
    card<-rep(0,n)
    for (i in 1:n) {
      card[i]<-x[b]
      b<-x[b]
    }
  }
  # judge whether the number k is found and return the result
  if (k %in% card){
    return (1)
  }else{
    return (0)
  }
}



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



# Examples for n=5:
# estimated individual success probabilities
Pone(5,4,1,10000)
Pone(5,4,2,10000)
Pone(5,4,3,10000)
# estimated joint success probabilities
Pall(5,1,10000)
Pall(5,2,10000)
Pall(5,3,10000)
# Examples for n=50:
# estimated individual success probabilities
Pone(50,4,1,10000)
Pone(50,4,2,10000)
Pone(50,4,3,10000)
# estimated joint success probabilities
Pall(50,1,10000)
Pall(50,2,10000)
Pall(50,3,10000)



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
