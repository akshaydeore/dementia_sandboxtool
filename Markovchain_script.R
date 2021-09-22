#install.packages("markovchain") 
#install.packages("diagram")
library(markovchain)

library(diagram)
library(shape)

#Method Definations

#add Node
addNode<-function(mc,nodename){
  n=nrow(mc@transitionMatrix)
  m2<-matrix(0,n+1,n+1)
  m2[n+1,n+1]=1
  m2[1:n,1:n]<-mc@transitionMatrix
  statelist=mc@states
  statelist<-c(statelist,nodename)
  return(new("markovchain",transitionMatrix=m2,states=statelist))
}

#delete Node
deleteNode<-function(mc,nodename){
  S<-mc@states
  if(!(nodename %in% S)){stop("The Markov chain object does not have a state by this name.")}
  T<-mc@transitionMatrix
  NS <- setdiff(S,nodename)
  pos<- which(S==nodename)
  matrix1<-T[-pos,-pos]
  weightdenominator=rowSums(matrix1)
  if(0 %in% weightdenominator){stop("Can't delete this node. Edit transition weights first.")}
  matrix1<-matrix1/weightdenominator
  return(new("markovchain",transitionMatrix=matrix1,states=NS))
}

#edit Node Link
editLink<-function(startNode,endNode,mc,P){
  #typeof(P)
  #p <- 0.5
  M<-mc@transitionMatrix
  X<-M[startNode,endNode]
  M[startNode,] # log 1
  M[startNode,endNode]=P
  M[startNode,] # log 2
  M[startNode,]=M[startNode,]-M[startNode,]*(P-X)
  M[startNode,endNode]<- P
  mc@transitionMatrix <- M
  MC1<- new("markovchain",transitionMatrix=M, mc@states) 
  return(MC1)
}

calculate<-function(mc){
  r <- steadyStates(mc)
  return(r)
}

  
#Program Flow
# Creating a transition matrix
trans_mat <- matrix(c(0,1,0,0,0.7,0,0.3,0,0.3,0,0,0.7,1,0,0,0),nrow = 4, byrow = TRUE)

# Vector of States
states=c("EW","GP","SP","DCT")

# create the Discrete Time Markov Chain
disc_trans <- new("markovchain",transitionMatrix=trans_mat, states, name="MC") 
disc_trans




#calling add node function
#disc_trans <- addNode(disc_trans,'NEW')
#disc_trans


#calling Edit Link function
#disc_trans <- editLink('EW','NEW',disc_trans,0.3)
#disc_trans

#disc_trans <- editLink('NEW','EW',disc_trans,0.7)
#disc_trans

#disc_trans <- deleteNode(disc_trans,'NEW')
#disc_trans

plot(disc_trans)

calculate(disc_trans)


#disc_trans[1]