## Jiayi Li, S2304671 : Coding for function dloop
## Rongkai Fang, s2310813 : Coding for function Pall
## Yifan Hu, s2287683 ：Coding for function Pone
## github repo address: https://github.com/Lijiayi111/Coursework2-.git
## The contribution of our group member is roughly equal. We discussed the ideas 
## and general process of all questions together. Then each one of us was 
## responsible for formally coding and commenting and also reviewing other parts of coding. 



# To calculate the probability of a single prisoner's success
# Input: n (half number of boxes), k (the prisoner’s number), strategy (1, 2 or 3) 
# and nreps (the number of replicate simulations)
# The function should return the estimated probability, which should be a number
# between 0 and 1.
Pone<-function(n,k,strategy,nreps) {
  # initialize result and this vector is to record the results of each simulation
  result<-rep(0,nreps) 
  for (i in 1:nreps) {
    CardVector<-sample(1:(2*n),2*n)
    # Using function func to judge whether the prisoner succeed in ith simulation
    result[i]<-func(CardVector,n=n,k=k,strategy = strategy)
  }
  #return the probability
  return(sum(result)/nreps)
}


# Judge whether the prisoner k is successful.
# Input: x is a vector recording the number of cards and the other arguments are the same as Pone.
# If the prisoner succeeds, the function will return TRUE. If not, return FALSE.
func<-function(x,n,strategy,k){
  #if strategy==3,randomly open n boxes,and judge if the prisoner number in the boxes.
  if(strategy == 3){
    if(k %in% sample(x,n)){
      return (TRUE)
    }
    #'b' denote the box number. if strategy==1,make b=k.If strategy==2,make b equal a random number between 1:2n.
    #every time open a box, judge if the card number(x[b]) inside the box equal to k and make the box number = card number(b<-x[b])
  }else if(strategy == 1){
    b <- k
    for (i in 1:n) {
      if(x[b] == k){
        return (TRUE)
      }
      b<-x[b]
    }
  }else{
    b<- sample(1:(2*n),1)
    for (i in 1:n) {
      if(x[b] == k){
        return (TRUE)
      }
      b<- x[b]
    }
  }
  #if we can not find the card with their number on it,return FALSE
  return (FALSE)
  
}


#To calculate the probability of all prisoners who succeed in finding their number.
#Input:n(half number of boxes);strategy(Type of strategy);nreps(number of repetisions)
#output:The probability of all prisoners finding their number.
Pall<- function(n,strategy,nreps){
  #Create a vector named 'result',which record every time of simulation's result.
  #and initialize all the elements to 1, which denotes that all prisoners have successfully found their number.
  #then loops nreps times, if there is one prisoner who can not find his card before opening n boxes, change the 1 to 0.
  result <- rep(1,nreps)
  for(i in 1:nreps){
    #Create a vector, which elements denote the cards number,and index denote the boxes number.
    cardvector <- sample(1:(2*n),2*n)
    #for each simulation, all prisoners will go to find their card.
    for(k in 1:(2*n)){
      #Use the 'func' function to judge if the prisoner succeed in finding their number.
      #If fail,change the 1 to 0 in result[i]
      if(func(cardvector,n=n,k=k,strategy=strategy) == FALSE){
        result[i] = 0
        break
      }
    }
  }
  #return the probability of all prisoners succeeding.
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
# However, for strategy 1, the estimated probabilities of all prisoners succeeding 
# are around 0.3 while the probabilities for other two strategies are extremely small 
# and approach to 0 as the value of n grows.



## To calculate two probability at the same time. one is the at least one time occuring probability called result 1, second is 
## the probability of all less than 50 called result two.
dloop <- function(n, nreps) {
  # the probability matrix before processing which will accomodate the at least one time occuring probability
  preProbMatrix <- matrix(0, nrow = nreps, ncol = 2 * n)
  # initilized the number of time of all less than n
  count <- 0
  # the interation of nreps times
  for (i in 1:nreps) {
    # the cards behind the box are uniformally random number
    card <- sample(1:(2 * n), 2 * n)
    for (k in 1:(2 * n)) {
      # initilize the loop length
      loopLength <- 1
      # k denoted the person's index
      ## initialize the first box choosen which is equal to the prisoner number
      boxChoosen <- k
      while (loopLength <= 2 * n) {
        if (card[boxChoosen] == k) {
          #  when at least a length occure just record as 1
          preProbMatrix[i, loopLength] <- 1
          break # immidiatly go to check next prisoner, jump out of the while loop
        } else{
          # turn to choose the next box
          boxChoosen <- card[boxChoosen]
          # length increase by 1
          loopLength  <- loopLength  + 1
        }
      }
    }
    # during every interation when all less than 50 interation happens, counter increase by 1.
    if (sum(preProbMatrix[i, (n + 1):(2 * n)]) == 0) {
      count <- count + 1 # count increase by 1
    }
  }
  
  # sum all the column and divide by total number of column
  prob <- colSums(preProbMatrix) / nreps
  # the probability of all less than 50
  prolessthann <- count / nreps
  result <- list()
  # make them alist to return them all
  result[[1]] <- prob
  result[[2]] <- prolessthann
  return(result)
  
}

# return the two probability by calling the dloop function 
result <- dloop(50, 10000)

#the at least one time occuring probability
prob <- result[[1]]
# the probability of all less than 50
prolessthann <- result[[2]]

# make the x-axis form 1 to 2n
loopLength <- 1:100
# visualising the probabilities
plot(loopLength,
     prob,
     xlab = "Length of loop",
     ylab = "Probability of loop",
     main = "Probabilities v.s. length of loop")
