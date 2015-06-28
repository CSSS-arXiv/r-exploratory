# r-exploratory
Quick riffs on the dataset to see what is possible


# Data pre-reqs

Make a directory called "./paper_sample" and put the untarred paper_sample.tar.gz  (which is named "paper_sample" on the Google Drive). The directory should contain the "*.txt.gz" files. You can keep them compressed.

# lda.R
Quick exploration of the sample data.

# dendrogram.R
Assumes the existence of two file:
* lda/doc_topics.txt
* lda/topic_keys.txt

The first contains each document's distribution over topics. The 2nd is a listing of the top20 words associated with each topic, presumably in descending order.

The R script will perform Jensen Shannon Distance calculations over the topic distrbutions and plot them.