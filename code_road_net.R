#packages
library(tidyverse)
library(statnet)
library(UserNetR)

#dataset: Pennsylvania road network
net <- read.table("C:/Users/bjorn/Documents/NATUURKUNDE/M-2/Introduction to Complex Systems/Project 2/roadNet-PA.txt", quote="\"", col.names=c("from","to"))

x1 <- as.numeric(net$from)
x11 <- x1+1
x2 <- as.numeric(net$to)
x22 <- x2+1
net<- cbind(x11,x22) 
net_cn <- network(net, matrix.type="edgelist")
   #vactors x11 and x22 are needed because "0" is not a suitable name for edge for function "network"
rm(x1, x11, x2, x22)

#Q2
#net is in the edge list format, but we need the sociomatrix format
#Do not run it!!!

#code:
#net_matrix <- as.sociomatrix(net_cn)
   #as we can see this file is rather big (over 100gb), so we refrain form using it

#Q3
#clustering coeff
gtrans(net_cn, use.adjacency = FALSE)
   #it uses sparse graph methods

#Q4
#Estimate the degree distribution of the networks 
a1 <- as.data.frame(net) %>%
  group_by(x11) %>%
  summarise(deg=n()) %>%
  select(deg) 

a2 <- a1 %>%
  group_by(deg) %>%
  summarise(n=n())

deg_dist <- a2
rm(a1, a2)

#and plot the probability mass functions
ggplot()+
  geom_point(mapping = aes(x=deg_dist$deg, y=deg_dist$n), colour="red", size=2.5)+
  labs(x="degree", y="number of vertices")

#Plot a log-log plot as well
ggplot()+
  geom_point(mapping = aes(x=log(deg_dist$deg), y=log(deg_dist$n)), colour="red", size=2.5)+
  labs(x="ln(degree)", y="ln(number of vertices)")

#Q5
#Calculate the average degree of the neighbors of a randomly chosen node in one network of your choice
a0 <- as.data.frame(net)
a1 <- a0 %>%
  group_by(x11) %>%
  summarise(n=n())

a2 <- left_join(a0,a1, by="x11")

a3 <- rename(a1, x22 = x11)

a4 <- left_join(a2,a3, by="x22") %>%
  rename (deg_V1=n.x, deg_V2=n.y)

a5 <- a4 %>%
  group_by(x11)%>%
  summarise(mean(deg_V2))

mean(a5$`mean(deg_V2)`)
mean(a3$n)

#Q7
#Q7
N <- sum(deg_dist$n)
avg <- 5.26

nrow(deg_dist)

deg_dist$expected <- N*dpois(deg_dist$degree, avg)
deg_dist$error <- (deg_dist$n - deg_dist$expected)^2 / deg_dist$expected
chisquared <- sum(deg_dist$error)