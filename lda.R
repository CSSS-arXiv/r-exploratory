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
                                  stoplist.file="./resources/stopwords.txt")

#create topic trainer object. 10 Topics
n.topics <- 10
topic.model <- MalletLDA(n.topics)

#load documents
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
## These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
