#Importing libraries
library(tidyverse)
library(igraph)
library(igraphdata)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

#Import the Data
currencies_data <- read.delim("flandreau_jobst_internationalcurrencies_data.txt", skip = 9)

#Countries Informations
countries_infos <- currencies_data[,c("country_A","gold","debtburden","rlong","rshort1900","rshort1890","rgdp","rgdpcap","poldemo","coverage")]
countries_infos <- unique(countries_infos)

#We create a directed graph with our dataframe
#Before that, we must divide weights based on the trade amount
graph_data <- currencies_data[currencies_data$bitrade != 0 ,c("country_A","country_B","bitrade")]
quantile(graph_data$bitrade)
breakpoints <- c(-Inf, 12000, 51000, Inf)
graph_data$category <- cut(graph_data$bitrade, breaks = breakpoints, labels = c("green", "blue", "red"))

graph <- graph_from_data_frame(graph_data,directed = TRUE, vertices=countries_infos)
E(graph)$weight <- graph_data$bitrade
graph$layout <- layout_nicely(graph)

par(mfrow=c(1,1), mar=rep(0,4))
plot(
  graph,
  vertex.size = 3,
  vertex.color = "blue",
  vertex.frame.color = "blue",
  vertex.label.cex = 0.8,
  vertex.label.color = "black",
  vertex.label.dist = 0.3,
  edge.width = 0.1,
  edge.cex = 1,
  edge.color = as.character(graph_data$category)
)

#Centralities
c_eigen  <- eigen_centrality(graph, directed = T, weights = E(graph)$weight)$vector

df <- data.frame(rgdp = V(graph)$rgdp,
                 gold = V(graph)$gold,
                 eigen = c_eigen)
ggplot(df, aes(x = eigen, y = gold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  theme_minimal()

ggplot(df, aes(x = eigen, y = rgdp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  theme_minimal()

cor.test(c_eigen, V(graph)$gold)
cor.test(c_eigen, V(graph)$rgdp)

# Clean environment
rm(list = ls())
