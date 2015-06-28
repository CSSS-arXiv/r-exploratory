#
# Will read-in the doc_topics.txt and topic_keys.txt and generate dendrograms
#

library(readr)
library(dplyr)
library(stringr)
library(ggdendro)
library(ggplot2)

#' Calculate the Entropy of a vector of probabilities
#'
#' @param v 
#'
#' @return entropy
#' @export
#'
#' @examples
H <- function(v) {
  v <- v[v > 0]
  return(sum(-v * log(v)))
}


#' Calculate the Jensen Shannon Divergence from a matrix of probability
#' distributions
#' 
#' The distributions could, for example, be the topic probabilities as
#' identified from an LDA topic model over a set of document
#' 
#' @param w a weight vector. Should sum to 1
#' @param m the MxN probability vector, with the probs in columns
#'   
#' @return
#' @export
#' 
#' @examples
JSDivergence <- function(w, m) {
  return(H(m %*% w) - apply(m, 2, H) %*% w)
}


#' JSD distance 
#' 
#' http://enterotype.embl.de/enterotypes.html
#'
#' @param inMatrix pretty sure the probs are columnar
#' @param pseudocount prevents a zero in the numerator or denominator
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
dist.JSD <- function(inMatrix, pseudocount=0.000001, ...) {
  KLD <- function(x,y) sum(x *log(x/y))
  JSD<- function(x,y) sqrt(0.5 * KLD(x, (x+y)/2) + 0.5 * KLD(y, (x+y)/2))
  matrixColSize <- length(colnames(inMatrix))
  matrixRowSize <- length(rownames(inMatrix))
  colnames <- colnames(inMatrix)
  resultsMatrix <- matrix(0, matrixColSize, matrixColSize)
  
  inMatrix = apply(inMatrix,1:2,function(x) ifelse (x==0,pseudocount,x))
  
  for(i in 1:matrixColSize) {
    for(j in 1:matrixColSize) { 
      resultsMatrix[i,j]=JSD(as.vector(inMatrix[,i]),
                             as.vector(inMatrix[,j]))
    }
  }
  colnames -> colnames(resultsMatrix) -> rownames(resultsMatrix)
  as.dist(resultsMatrix)->resultsMatrix
  attr(resultsMatrix, "method") <- "dist"
  return(resultsMatrix) 
}

topic_keys = read_delim("./lda/topic_keys-2.txt", delim = "\t", col_names = c("id", "dirichlet", "topic"))

doc_topics = read_delim("./lda/doc_topics-2.txt", delim = "\t", col_names = c("id", "document",
                                                                            topic_keys$id))

doc_topic_mat = as.matrix(doc_topics %>% select(-document, -id), nrow=nrow(doc_topics))

top_words = str_split_fixed(topic_keys$topic, pattern=" ", 20)[,1:4]

colnames(doc_topic_mat) = apply(top_words, 1, paste, collapse=" ")

rownames(doc_topic_mat) = doc_topics$document

#normalize
doc_topic_mat = doc_topic_mat / rowSums(doc_topic_mat)

even_weight = rep(1/nrow(doc_topic_mat), nrow(doc_topic_mat))

# doc_topic_mat has the doc-probs in the rows. SO transform (t(...)) it
JSDivergence(even_weight, t(doc_topic_mat))

jsd_distance= dist.JSD(doc_topic_mat)

hc = hclust(jsd_distance)
ggdendrogram(hc, rotate=TRUE) + ggtitle("Hierachical Cluster 2\narXiv Condensed Matter Topics Extracted via LDA")

