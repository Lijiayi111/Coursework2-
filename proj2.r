
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
    return (TRUE)
  }else{
    return (FALSE)
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
# estimated individual success probabilities when k=4
Pone(5,4,1,10000)
Pone(5,4,2,10000)
Pone(5,4,3,10000)
# estimated joint success probabilities
Pall(5,1,10000)
Pall(5,2,10000)
Pall(5,3,10000)
# Examples for n=50:
# estimated individual success probabilities when k=4
Pone(50,4,1,10000)
Pone(50,4,2,10000)
Pone(50,4,3,10000)
# estimated joint success probabilities
Pall(50,1,10000)
Pall(50,2,10000)
Pall(50,3,10000)
# We can find that the estimated probabilities of individual success for each strategy
# are around 0.5, 0.4 and 0.5. And the probabilities for strategy 1 and 3 are similar.
# However, for strategy 1, the estimated probability of all prisoners succeeding 
# is around 0.3 while the probabilities for other two strategies are extremely close
# to 0.


function66 <- function(x,n){
  t <- rep(0,2*n)
  length<- rep(0,2*n)
  for (k in 1:(2*n)){ 
    b <- k
    t[k]<- k
    print("tvalue is ")
    print(t)
    card<-rep(0,2*n)
    
    # construct the card list we will take  
    for (i in 1:(2*n)){
       card[i]<-x[b]
       b<-x[b]}
    print(card)
    # the index of card when k first equals to card 
    length[k] <- match(k,card)
    print(length[k])
  }
  
  print(length)
  
  return(length)
}


dloop<- function(n,nreps){
  SamMatrix<-matrix(nrow=nreps,ncol=2*n)
  # interate the function66 by nreps time by creating nreps row of random cards
  for (i in 1:nreps) {
    SamMatrix[i,]<-sample(1:(2*n),2*n)
  }
  # apply the function 66 for every row 
  result<-apply(SamMatrix, 1, function66,n=n)
  #print(result)
  
  print(result)
  print(table(result))
  result66 <- table(result)/nreps
  
  return(result66)
}






# Question 5
dloop <- function(n,nreps){
   # the probability matrix before processing which will accomodate the at least one time occuring probability 
   preProbMatrix <- matrix(0,nrow =nreps, ncol = 2*n) 
   # the interation of nreps times 
   for (i in 1:nreps){
     # the cards behind the box are uniformally random number
     card <- sample(1:(2*n),2*n)
     # initilized the number of time of all less than n 
     count <- 0 
     for (k in 1:(2*n)){
       # initilize the loop length
       loopLength <- 1
       # k denoted the person's index 
       ## initialize the first box choosen which is equal to the prisoner number
       boxChoosen <- k
       while (loopLength <= 2*n){
         if(card[boxChoosen] == k){
           #  when at least a length occure just record as 1
           preProbMatrix[i,loopLength] <- 1
           break # immidiatly go to check next prisoner, jump out of the while loop
         }else{
           # turn to choose the next box
           boxChoosen <- card[boxChoosen]
           # length increase by 1
           loopLength  <- loopLength  + 1
         }
       }
     }
     # during every interation when all less than 50 interation happens, counter increase by 1.
     if(sum(preProbMatrix[i, (n+1):(2*n)]) == 0){
       count <- count + 1 # count increase by 1 
     }
   # sum all the column and divide by total number of column
   prob <- colSums(preProbMatrix)/nreps  
   # the probability of all less than 50
   prolessthann <- count/nreps   
   print(prolessthann)
   result <- list()
   result[[1]] <- prob
   result[[2]] <- prolessthann
   return(result)
}
}

loopLength <- 1:100
result <- dloop(50, 10000)


plot(loopLength, prob, xlab = "Length of loop", ylab = "Probability of loop",
     main = "Probabilities v.s. length of loop")

# Question 6
dlooplowerthan50 <- function(n, nreps){
  preProbMatrix <- matrix(0, nrow = nreps, ncol = 2*n)
  count <- 0 # count the number of iteration satisfied our demand 

}
dloop1 <- function(n, nreps){#与第五题大致相同，主要用于统计循环长度小于等于50的情况，任何大于50的情况都不考虑
  prob_matrix <- matrix(0, nrow = nreps, ncol = 2*n) #概率矩阵
  box <- 1:(2*n) #盒子
  b <- 0 #循环长度小于等于50的计数器
  for (i in 1:nreps) {
    ncard <- sample(1:(2*n), 2*n) #随机洗牌
    for (j in 1:(2*n)) {
      box <- j
      depth <- 1
      while (depth <= 2*n) {
        if(ncard[box] == j){
          prob_matrix[i, depth] = 1
          break
        }else{
          box = ncard[box]
          depth = depth + 1
        }
      }
    }
    if(sum(prob_matrix[i, (n+1):(2*n)]) == 0){ #51-100全为0时
      b <- b + 1 #计数器+1
    }
  }
  return(b/nreps)
}
dloop1(50, 10000)
