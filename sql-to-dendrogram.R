library(DBI)
library(dplyr)
library(tidyr)
library(ggdendro)
library(ggplot2)
library(readr)
library(stringr)
library(parallel)

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



makeDendro = function(i, keyDbDF) {
  keyFileName = keyDbDF[[i, "topic"]]
  dbFileName = keyDbDF[[i, "db"]]
  print(paste("processing", i, keyFileName, dbFileName))
  topic_keys = read_delim(keyFileName, delim = "\t", col_names = c("id", "dirichlet", "topic"))
  
  con = dbConnect(RSQLite::SQLite(), dbFileName)
  
  res = dbSendQuery(con, "SELECT count(*) from doc")
  theCount = dbFetch(res)
  colnames(theCount) = c("count")
  pb <- txtProgressBar(min = 1, max = theCount$count, style = 3)
  
  res = dbSendQuery(con, "SELECT * from doc")
  topics_long = data.frame()
  while(!dbHasCompleted(res)){
    chunk = dbFetch(res, n = 50000)
    topics_long = bind_rows(topics_long, chunk)
    setTxtProgressBar(pb, nrow(topics_long))
  }
  dbClearResult(res)
  
  topics_wide = topics_long %>% spread(topicid, proportion)
  
  doc_topic_mat = as.matrix(topics_wide %>% select(-id), nrow=nrow(topics_wide))
  
  top_words = str_split_fixed(topic_keys$topic, pattern=" ", 20)[,1:4]
  
  colnames(doc_topic_mat) = apply(top_words, 1, paste, collapse=" ")
  
  jsd_distance= dist.JSD(doc_topic_mat)
  
  title = paste("Hierachical Cluster",i,"\narXiv Condensed Matter Topics Extracted via LDA", sep=" ")
  plotFileName = paste("./plots/cond-mat-hc-",i,".png", sep="")
  
  hc = hclust(jsd_distance)
  ggdendrogram(hc, rotate=TRUE) + 
    ggtitle(title)
  
  ggsave(plotFileName, width=12, height=8, dpi=100)
  
}

ldaOutputs = data.frame(topic = c("./sqlite/topic_keys_1.txt",
                                 "./sqlite/topic_keys_2.txt",
                                 "./sqlite/topic_keys_3.txt",
                                 "./sqlite/topic_keys_4.txt",
                                 "./sqlite/topic_keys_5.txt"),
                        db = c("./sqlite/doc_topics_1.db",
                                "./sqlite/doc_topics_2.db",
                                "./sqlite/doc_topics_3.db",
                                "./sqlite/doc_topics_4.db",
                                "./sqlite/doc_topics_5.db"),
                        stringsAsFactors = FALSE)

lapply(1:nrow(ldaOutputs), makeDendro, ldaOutputs)
