require(readr)
require(dplyr)
require(mallet)

# sample_documents_file is a dump of the main sample documents dataframe. I don't want to 
# recreate it everytime. If the contents of the paper_sample directory changes, remove
# the "sample_documents.RData" file in the current working director to regenerate.
sample_documents_file = "sample_documents.RData"
if(file.access(sample_documents_file)){
  
  print("No documents file - generating from scratch. Sit tight")
  
  file_list <- c(
    list.files(path="./paper_sample/", 
               pattern="*txt.gz$", 
               full.names=TRUE)
  )
  
  sizes = lapply(file_list,FUN = function(fn){file.info(fn)$size})
  
  file_list_df = data.frame(name=as.vector(file_list), size=unlist(sizes), stringsAsFactors = FALSE)
  
  big_files = file_list_df %>% filter(size>2000)
  
  sample_documents_list = lapply(big_files$name, FUN = function(fn){
    tryCatch({
      read_file(file=fn)
    }, error = function(e) {
      NA
    }
    )
  })
  
  big_files$content = unlist(sample_documents_list)
  
  sample_documents_df = big_files
  
  save(sample_documents_df, file=sample_documents_file)
} else {
  load(sample_documents_file)
}

train_documents_df = sample_documents_df %>% sample_n(100)

mallet.instances <- mallet.import(train_documents_df$name, 
                                  train_documents_df$content, 
                                  stoplist.file="./resources/stopwords.txt",
                                  token.regexp = "[a-zA-Z]+")

#create topic trainer object. 10 Topics
n.topics <- 50
topic.model <- MalletLDA(n.topics)

#load documents
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
## These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

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
train_documents_df[best_fit,"contents"]

distance = dist(topic.words, diag=T)
print(distance, digits=3)

## cluster based on shared words
filename = paste("./plots/dendrogram", nrow(train_documents_df), "png", sep=".")
titleText = paste0(nrow(train_documents_df), " Document Topic Dendrogram")

png(file=filename, bg="white", height=1050, width=1250)
plot(hclust(distance), 
     labels=topics.labels, font=2, lwd=3, cex=2, main="")
title(main=titleText, cex.main = 4)
dev.off()

