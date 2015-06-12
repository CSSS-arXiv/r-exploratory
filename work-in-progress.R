#####
#
# the contents of this file will move into lda.R as I go. So don't try to run anything.
#
#




## Optimize hyperparameters every 20 iterations,
## after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## Now train a model. Note that hyperparameter optimization is on, by default.
## We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(200)

## NEW: run through a few iterations where we pick the best topic for each token,
## rather than sampling from the posterior distribution.
topic.model$maximize(10)

## Get the probability of topics in reviews and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities,
## so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

# from http://www.cs.princeton.edu/~mimno/R/clustertrees.R
## transpose and normalize the doc topics
topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)
write.csv(topic.docs, "dcb-topic-docs.csv")

## Get a vector containing short names for the topics
topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) {
  topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=5)$words, collapse=" ")
}
# have a look at keywords for each topic
topics.labels

# create data.frame with columns as reviews and rows as topics
topic_docs <- data.frame(topic.docs)
names(topic_docs) <- rownames(reviews)

docs_topics = t(topic_docs)
best_fits = tail(sort(docs_topics[,3]))
best_fit = names(best_fits)[length(best_fits)]
reviews[best_fit,"reviewText"]

distance = dist(topic.words, diag=T)
print(distance, digits=3)

## cluster based on shared words
png(file="./plots/dendrogram.png", bg="transparent", height=1050, width=1050)
plot(hclust(distance), 
     labels=topics.labels, font=2, lwd=3, cex=2, main="")
title(main="Cluster Dendrogram", cex.main = 4)
dev.off()
#' Calculate similarity matrix
#' Shows which reviews are similar to each other
#' by their proportions of topics. Based on Matt Jockers' method

###########
#DON'T DEMO BELOW THIS
###########


library(cluster)
topic_df_dist <- as.matrix(daisy(t(topic_docs), metric = "euclidean", stand = TRUE))
# Change row values to zero if less than row minimum plus row standard deviation
# keep only closely related reviews and avoid a dense spagetti diagram
# that's difficult to interpret (hat-tip: http://stackoverflow.com/a/16047196/1036500)
#topic_df_dist[ sweep(topic_df_dist, 1, (apply(topic_df_dist,1,min) + apply(topic_df_dist,1,sd) )) > 0 ] <- 0
topic_df_dist[ sweep(topic_df_dist, 1, (apply(topic_df_dist,1,max) - 0.01*apply(topic_df_dist,1,sd) )) > 0 ] <- 0
#' Use kmeans to identify groups of similar authors

km <- kmeans(topic_df_dist, n.topics)
# get names for each cluster
allnames <- vector("list", length = n.topics)
for(i in 1:n.topics){
  allnames[[i]] <- names(km$cluster[km$cluster == i])
}

# Here's the list of people by group
allnames

#' Visualize people similarity using force-directed network graphs

#### network diagram using Fruchterman & Reingold algorithm
# static
# if you don't have igraph, install it by removing the hash below:
# install.packages("igraph")
library(igraph)
g <- as.undirected(graph.adjacency(topic_df_dist))
layout1 <- layout.fruchterman.reingold(g, niter=500)
plot(g, layout=layout1, edge.curved = TRUE, vertex.size = 1, vertex.color= "grey", edge.arrow.size = 0, vertex.label.dist=0.5, vertex.label = NA)

# for Gephi
# this line will export from R and make the file 'g.graphml'
# in the working directory, ready to open with Gephi
write.graph(g, file="g.graphml", format="graphml")

# interactive in a web browser
# if you have a particularly large dataset, you might want to skip this section, and just run the Gephi part.
# if you don't have devtools, install it by removing the hash below:
# install.packages("devtools")

#devtools::install_github("christophergandrud/d3Network")
require(d3Network)
d3SimpleNetwork(get.data.frame(g),width = 1500, height = 800,
                textColour = "orange", linkColour = "red",
                fontsize = 10,
                nodeClickColour = "#E34A33",
                charge = -100, opacity = 0.9, file = "d3net.html")
# find the html file in working directory and open in a web browser





