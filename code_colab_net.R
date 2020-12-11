#packages
library(tidyverse)
library(statnet)
library(UserNetR)

#dataset: Collaboration network of Arxiv High Energy Physics Theory
net <- read.table("C:/Users/bjorn/Documents/NATUURKUNDE/M-2/Introduction to Complex Systems/Project 2/CA-HepTh.txt", quote="\"", col.names=c("from","to"))

a1 <- as.matrix(net$from)
a2 <- as.matrix(net$to)
net<- cbind(a1,a2) 
net_cn <- network(net, matrix.type="edgelist")

rm(a1, a2)

#Q2
#net is in the edge list format, but we need the sociomatrix format
#Do not run it!!!

#code:
#net_matrix <- as.sociomatrix(net_cn)


   #as we can see this file is rather big (35.2gb), so we refrain form using it

#Q3
#clustering coeff
gtrans(net_cn, use.adjacency = FALSE)
   #it uses the sparse graph methods 

#Q4
#Estimate the degree distribution of the networks 
a1 <- degreedist(net_cn) %>%
  t() %>%
  as.data.frame()
a2 <- select(a1,indegrees) %>%
  rename(n=indegrees)
rownames(a2) <- substring(rownames(a2),8)
a2$degree <- as.numeric(rownames(a2))

deg_dist <- a2 
rm(a1, a2)

   #deggree distribution if degree >0
   deg_dist2 <- filter(deg_dist, degree>0)

#and plot the probability mass functions
ggplot()+
  geom_point(mapping = aes(x=deg_dist$degree, y=deg_dist$n), colour="red", size=2.5)+
  labs(x="degree", y="number of vertices")
   #without degree=0
   ggplot()+
     geom_point(mapping = aes(x=deg_dist2$degree, y=deg_dist2$n), colour="red", size=2.5)+
     labs(x="degree", y="number of vertices")


#Plot a log-log plot as well
ggplot()+
  geom_point(mapping = aes(x=deg_dist$degree, y=log(deg_dist$n)), colour="red", size=2.5)+
  labs(x="degree", y="ln(number of vertices)")
   #without degree=0
   ggplot()+
     geom_point(mapping = aes(x=deg_dist2$degree, y=log(deg_dist2$n)), colour="red", size=2.5)+
     labs(x="degree", y="ln(number of vertices)")

#Q5
#Calculate the average degree of the neighbors of a randomly chosen node in one network of your choice
