#Importing libraries
library(tidyverse)
library(igraph)
library(igraphdata)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

###################################
# Data importation and first plot #
###################################

#Import the Data
currencies_data <- read.delim("flandreau_jobst_internationalcurrencies_data.txt", skip = 9)

#Countries Informations
countries_infos <- currencies_data[,c("country_A","gold","debtburden","rlong","rshort1900","rshort1890","rgdp","rgdpcap","poldemo","coverage")]
countries_infos <- unique(countries_infos)

#We create a directed graph with our dataframe
#Before that, we must divide weights based on the trade amount
graph_data <- currencies_data[currencies_data$bitrade != 0 ,c("country_A","country_B","bitrade","colony")]

#Finding if a country is a colonizer of other countries
colonizer <- graph_data[,c("country_B","colony")]
grouped_colonizer <- colonizer %>%
  group_by(country_B) %>%
  summarize(Sum_colony = sum(colony))
grouped_colonizer$is_colonizer <- ifelse(grouped_colonizer$Sum_colony != 0 ,1 ,0)
# Add the variable is_colonizer to countries_info
countries_infos <- countries_infos %>%
  left_join(grouped_colonizer[,c("country_B","is_colonizer")], by = c("country_A" = "country_B"))


quantile(graph_data$bitrade)
breakpoints <- c(-Inf, 12000, 51000, Inf)
graph_data$category <- cut(graph_data$bitrade, breaks = breakpoints, labels = c("green", "blue", "red"))

graph <- graph_from_data_frame(graph_data,directed = TRUE, vertices=countries_infos)
E(graph)$weight <- graph_data$bitrade
E(graph)$colony <- graph_data$colony
graph$layout <- layout_nicely(graph)

par(mfrow=c(1,1), mar=rep(0,4))

# Graph plot
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
  edge.color = as.character(graph_data$category),
  main = "Graph Visualization with Trade Data"
)

# As we can see, the total trade is higher in the center of the graph (blue and red edges), 
# which shows that there is a group of central countries which have a big number of connections 
# with a high trade value, while being the central home of the currency implied in the trade.

################
# Centralities #
################

# We want to measure how much the importance of a node in our graph depends on 
# the importance of its neighbors. To do so, instead of using eigenvector centrality,
# since our graph is directed, we will use pagerank centrality
#
# Let's compute it for all nodes, then we check its correlation with the two variables 
# rgdp (log 1900 real gdp) and gold (1 if country_A has a currency convertible in gold in
# 1900 and 0 otherwise.)
#
# We expect there to be a correlation between rgdp and pagerank centrality:
# Indeed, economically stronger countries often have more resources to invest in infrastructure, 
# technology and international relations, which can lead to greater connectivity and influence 
# across the world. This may be due to their location and their influential system.
#
# Also, we expect pagerank centrality to be linked to convertability in gold.
# This is because in the 1900s, particularly during the late 19th and early 20th 
# centuries, the gold standard was indeed a prevalent monetary system in many
# parts of the world. So for trading purposes, talking in gold can be sometimes easier,
# to have a clearer vision on the value of the goods traded, and so trade negotiation could be 
# more successful between gold convertible money countries.

c_page_rank <- page_rank(graph,directed = T, weights = E(graph)$weight)$vector

df <- data.frame(rgdp = V(graph)$rgdp,
                 gold = V(graph)$gold,
                 page_rank = c_page_rank)
ggplot(df, aes(x = page_rank, y = gold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  theme_minimal()+
  ggtitle("Pagerank Centrality vs. gold convertability")
# Apparently, these two variables are positively correlated

ggplot(df, aes(x = page_rank, y = rgdp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  theme_minimal()+
  ggtitle("Pagerank Centrality vs. Real GDP")

# Graphically, positive correlation can be seen between rgpd and pagerank value centrality.
#
# We will confirm correlation for both variables with a correlation test

cor.test(c_page_rank, V(graph)$gold)
# We get a correlation coefficient of 0.399 with a p-value of 0.006, so we can conclude that,
# while being a little low, we cannot deny that we have a positive  correlation between having 
# a currency convertible in gold and being influential in the network.

cor.test(c_page_rank, V(graph)$rgdp)
# Here we get a coefficient of 0.67 with a very low p value, thus confirming the correlation
# between eigen vector centrality and rgpd.

#Centrality visualization
colors_rgdp <- scales::dscale(V(graph)$rgdp %>% cut(9), sequential_pal)
sizes_pagerank <- (1:9)[c_page_rank %>% cut(9)] + 1
plot(graph,
     vertex.size = sizes_pagerank,
     vertex.color = colors_rgdp,
     vertex.label = NA,
     edge.color = rgb(0,0,0,.15),
     edge.arrow.size = 0.2,
     main = "Pagerank Centrality Visualization with rgdp"
     )

#Degree centrality and being a colonizer
c_degree <- degree(graph)

df_degree <- data.frame(is_colonizer = V(graph)$is_colonizer,
                 degree = c_degree)
ggplot(df_degree, aes(x = degree, y = is_colonizer)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  theme_minimal()+
  ggtitle("Degree Centrality vs. being a colonizer")

# Apparently, colonizers have a high degree centrality
# That was expected since a colonized country have many economic, financial, and political 
# relations withthe colonizer, and so they would use the colonizer's currency for their trades,
# especially since colonizer usually tend to export and import from the colony.




#######################
# Finding communities #
#######################
graph_net <- as.undirected(graph, mode = "mutual")
graph_greedy <- cluster_fast_greedy(graph_net, weights = E(graph_net)$weight)
graph_louvain <- cluster_louvain(graph_net, weights = E(graph_net)$weight)
graph_leiden <- cluster_leiden(graph_net, objective_function = "modularity",weights = E(graph_net)$weight)
graph_edgebetween <- cluster_infomap(graph,weights = E(graph)$weight)

graph_modularity_res <- data.frame(Algorithm = "Shortest paths", Modularity = modularity(graph,graph_spectral),
                                    Nb_clusters = max(graph_spectral))
graph_modularity_res <- rbind(graph_modularity_res, c("Spectral", modularity(graph_net,graph_spectral), max(graph_spectral)))
graph_modularity_res <- rbind(graph_modularity_res, c("Greedy", modularity(graph_greedy), max(graph_greedy$membership)))
graph_modularity_res <- rbind(graph_modularity_res, c("Louvain", modularity(graph_louvain), max(graph_louvain$membership)))
graph_modularity_res <- rbind(graph_modularity_res, c("Leiden", graph_leiden$quality, max(graph_leiden$membership)))
graph_modularity_res <- rbind(graph_modularity_res, c("Edge betweenness", modularity(graph_edgebetween), max(graph_edgebetween$membership)))
graph_modularity_res

# Clean environment
rm(list = ls())
