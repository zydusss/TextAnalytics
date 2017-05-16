
###  				  ###
### --- Text analytics classwork code --- ###
###					  ###


rm(list = ls())	# clear workspace history


install.packages("tm")
install.packages("Snowball")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RWeka")
install.packages("igraph")
install.packages("qdap")

library(tm)				# load all required packages, install if not loaded
library(Snowball)
library(SnowballC)
library(wordcloud)
library(RWeka)			
library(textir)
library(igraph)


### -- copy paste block of functions below, will invoke as needed later --- ###

#-- func to make wordcloud and view some freqs for docs 

makewordc = function(a){	# plot wordcloud func opens
  
  a.colsum = apply(a, 2, sum);
  
  min1 = min(150, length(a.colsum))	# no more than 150 terms in wordcloud
  
  words = colnames(a)[1:min1]
  
  freq = a.colsum*100
  
  wordcloud(words, freq, scale=c(8, 0.3), colors=1:10)	} # func ends

# --- func to make cluster dendograms --- #

clusdend = function(a){	# writing func clusdend() 	
  
  mydata.df = as.data.frame(inspect(a));	
  
  mydata1.df = mydata.df[, order(-colSums(mydata.df))];
  
  min1 = min(ncol(mydata.df), 40) 	# minimum dimn of dist matrix
  
  test = matrix(0,min1,min1)
  
  test1 = test
  
  for(i1 in 1:(min1-1)){ 
    
    for(i2 in i1:min1){
      
      test = sum(mydata1.df[ ,i1]-mydata1.df[ ,i2])^2
      
      test1[i1,i2] = test; test1[i2, i1] = test1[i1, i2] 	}
    
  }
  
  # making dissimilarity matrix out of the freq one
  
  test2 = test1
  
  rownames(test2) = colnames(mydata1.df)[1:min1]
  
  # now plot collocation dendogram
  
  d <- dist(test2, method = "euclidean") # distance matrix
  
  fit <- hclust(d, method="ward")
  
  plot(fit) # display dendogram
  
} # clusdend() func ends


### --- Elementary sentiment analysis --- ###

pos=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt

neg=scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt

pos.words=c(pos,"wow", "kudos", "hurray") 			# including our own positive words to the existing list

neg.words = c(neg)

# positive sentiment wordcloud

makeposwordc = function(a){	# plot wordcloud func opens
  
  pos.matches = match(colnames(a), pos.words) 		# match() returns the position of the matched term or NA
  
  pos.matches = !is.na(pos.matches)
  
  b1 = apply(a, 2, sum)[pos.matches];	 b1 = as.data.frame(b1);
  
  colnames(b1) = c("freq");
  
  wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10)  	# wordcloud of positive words
  
}	# function ends for positive wordcloud

# negative sentiment wordlist

makenegwordc = function(a){	# plot wordcloud func opens
  
  neg.matches = match(colnames(a), neg.words) 		# match() returns the position of the matched term or NA
  
  neg.matches = !is.na(neg.matches)
  
  b1 = apply(a, 2, sum)[neg.matches];	 b1 = as.data.frame(b1);
  
  colnames(b1) = c("freq");
  
  wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10)  	# wordcloud of negative words
  
}	# func ends


### --- making social network of top-40 terms --- ###

semantic.na <- function(dtm1){		# dtm has terms in columns 
  
  dtm1.new = inspect(dtm1[, 1:40]); dim(dtm1.new)	# top-25 tfidf weighted terms
  
  term.adjacency.mat = t(dtm1.new) %*% dtm1.new;	dim(term.adjacency.mat)
  
  # term.adjacency.mat1 = 1*(term.adjacency.mat != 0) 	# adjacency matrix ready
  
  ## -- now invoke igraph and build a social network --
  
  require(igraph)
  
  g <- graph.adjacency(term.adjacency.mat, weighted = T, mode = "undirected")
  
  g <- simplify(g)		 # remove loops
  
  V(g)$label <- V(g)$name	# set labels and degrees of vertices
  
  V(g)$degree <- degree(g)
  
  # -- now the plot itself
  
  set.seed(1234)		# set seed to make the layout reproducible
  
  layout1 <- layout.fruchterman.reingold(g)
  
  plot(g, layout=layout1)
  
} # func ends


### --- make similar network for the individuals, not the terms this time --- ###

ppl.semantic.na <- function(dtm1, names){		# dtm has terms in columns, names is names list corres to docs
  
  dtm2.new = inspect(dtm1[,]); dim(dtm2.new)	
  
  term.adjacency.mat2 = dtm2.new %*% t(dtm2.new);	dim(term.adjacency.mat2)
  
  if (is.null(names)) {names = seq(1, nrow(term.adjacency.mat2), 1)}
  
  rownames(term.adjacency.mat2) = as.matrix(names)
  colnames(term.adjacency.mat2) = as.matrix(names)
  
  g1 <- graph.adjacency(term.adjacency.mat2, weighted = T, mode = "undirected")
  
  g1 <- simplify(g1)		 # remove loops
  
  V(g1)$label <- V(g1)$name	# set labels and degrees of vertices
  
  V(g1)$degree <- degree(g1)
  
  # -- now the plot itself
  
  set.seed(1234)		# set seed to make the layout reproducible
  
  layout2 <- layout.fruchterman.reingold(g1)
  
  plot(g1, layout=layout2)	# its beautiful, isn't it?
  
} # func ends



### ----- copy-paste functions: block ends here --- ###







### ---------------------------------- ###
### --- ice-cream dataset exercise --- ###
### ---------------------------------- ###

# read-in data from saved notepads first

x = readLines(file.choose()) 	# first, read-in data from 'Q25.txt'/ 'IceCreamDataX.txt'

length(x)			# check its length

x1 = Corpus(VectorSource(x))  	# Constructs a source for a vector as input

x1 = tm_map(x1, stripWhitespace) 	# removes white space
x1 = tm_map(x1, tolower)		# converts to lower case
x1 = tm_map(x1, removePunctuation)	# removes punctuatuion marks
x1 = tm_map(x1, removeNumbers)		# removes numbers in the documents
x1 = tm_map(x1, removeWords, c(stopwords('english'), "phone"))
x1 = tm_map(x1, stemDocument)


# ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 3, max = 3))	

#  tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
#						  tolower = TRUE, 
#						  removePunctuation = TRUE,
#						  stripWhitespace = TRUE,
#                                                  removeNumbers = TRUE,
#                                                  stopwords = TRUE
#						  stemDocument = TRUE
#						))		# patience. Takes a minute.

# build term-document matrix with different weights

tdm1 = TermDocumentMatrix(x1, control = list(weighting = function(x) weightTfidf(x, normalize = FALSE, stopwords = TRUE)));			

tdm0 = TermDocumentMatrix(x1);			# gives regular TF weighing, Above gave TFIDF weighing

dim(tdm1)			# check its basic characteristics


## remove blank documents (i.e. columns with zero sums)

a0 = NULL; 

for (i1 in 1:ncol(tdm0)){ if (sum(tdm0[, i1]) == 0) {a0 = c(a0, i1)} }

length(a0)		# no. of empty docs in the corpus

if (length(a0) >0) { tdm01 = tdm0[, -a0]} else {tdm01 = tdm0};	dim(tdm01)	# under TF weighing

if (length(a0) >0) { tdm11 = tdm1[, -a0]} else {tdm11 = tdm1};	dim(tdm11)	# under TFIDF weighing

inspect(tdm01[1:5, 1:10])		# to view elements in tdm1, use inspect()


# convert tdms to dtms i.e. docs become rows and terms become columns
# change dtm weighting from Tf (term freq) to TfIdf (term freq Inverse Doc freq)

dtm0 = t(tdm01)				# docs are rows and terms are cols

dtm = t(tdm11)					# new dtm with TfIdf weighting

# rearrange terms in descending order of Tf and view

a1 = apply(dtm0, 2, sum);

a2 = sort(a1, decreasing = TRUE, index.return = TRUE)

dtm01 = dtm0[, a2$ix];	inspect(dtm01[1:10, 1:10])

dtm1 = dtm[, a2$ix];	inspect(dtm1[1:10, 1:10])

windows()			# opens new windows & prevents over-writing of plots

makewordc(dtm1)

windows();	semantic.na(dtm1)

windows();	clusdend(dtm01)

windows();	makeposwordc(dtm1)

windows();	makenegwordc(dtm1)



# --- Can we segment the respondents (or cluster the documents) based on term usage? --- #

### --- kmeans proc ---- ###

# better cluster on TF dtm rather than tfidf dtm for solution stability #

# wss = (nrow(dtm01)-1)*sum(apply(dtm01, 2, var))		 # Determine number of clusters by scree-plot 
# for (i in 2:8) wss[i] = sum(kmeans(dtm01, centers=i)$withinss)
# plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #


k1 = 6		# based on the scree elbow plot

a3 = kmeans(dtm0, k1);	a3$size

round(a3$size/sum(a3$size), 2)		# segmt-sizes as proportions

# -- analyze each segment for what they're saying... --- #

a4 = NULL

for (i1 in 1:max(a3$cluster)) { 
  
  a4[[i1]] = dtm1[(a3$cluster == i1),]
  
} # loop ends


# now plot wordclouds for by segment and see

par(ask = TRUE)

for (i2 in 1:max(a3$cluster)){	
  
  makewordc(a4[[i2]])	} # loop ends


par(ask = FALSE)		# close ask facility for graph making


# cluster dendograms cluster terms *within* documents
# in contrast, kmeans clusters documents themselves using word freqs across documents

# now try these examples:

windows();	clusdend(a4[[1]]);	
windows();	clusdend(a4[[2]])



### ---------------------------------------------------- ###
### --- feedback dataset exercise ---- ###
### ---------------------------------------------------- ###


preprocess = function(x){
  
  length(x)			# check its length
  
  x1 = Corpus(VectorSource(x))  	# Constructs a source for a vector as input
  
  x1 = tm_map(x1, stripWhitespace) 	# removes white space
  x1 = tm_map(x1, tolower)		# converts to lower case
  x1 = tm_map(x1, removePunctuation)	# removes punctuatuion marks
  x1 = tm_map(x1, removeNumbers)		# removes numbers in the documents
  x1 = tm_map(x1, removeWords, c(stopwords('english'), "phone"))
  # x1 = tm_map(x1, stemDocument)
  
  tdm1 = TermDocumentMatrix(x1, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)));			
  
  tdm0 = TermDocumentMatrix(x1);			# gives regular TF weighing, Above gave TFIDF weighing
  
  list(tdm0, tdm1)	} # preprocess() func ends


##-- func for preprocessing with ngrams ---- ##

preprocess.ngrams = function(x, min1, max1){
  
  length(x)			# check its length
  
  x1 = Corpus(VectorSource(x))  	# Constructs a source for a vector as input
  
  ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = min1, max = max1))	
  
  tdm1 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
                                                weighting = function(x) weightTfIdf(x, normalize=FALSE),
                                                tolower = TRUE, 
                                                removePunctuation = TRUE,
                                                stripWhitespace = TRUE,
                                                removeNumbers = TRUE,
                                                stopwords = TRUE
                                                # 						  stemDocument = TRUE
  ))		# patience. Takes a minute.
  
  tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
                                                tolower = TRUE, 
                                                removePunctuation = TRUE,
                                                stripWhitespace = TRUE,
                                                removeNumbers = TRUE,
                                                stopwords = TRUE
                                                # 						  stemDocument = TRUE
  ))		# patience. Takes a minute.
  
  test = rownames(tdm1);	test1 = gsub(" ", ".", test);	
  
  rownames(tdm0) = test1;	rownames(tdm1) = test1;
  
  list(tdm0, tdm1)	} # preprocess.ngrams() func ends



# read-in data from saved notepads first

x = readLines(file.choose()) 			# first, read-in data from 'DidntLikeInDC.txt'
# later, do so for LikedInDC.txt

## read in names list ##

names = read.table(file.choose())		# read-in namesList.txt

names = as.matrix(names);	length(names)


a1 = preprocess(x)		# apply preprocess() func to raw input data 
tdm0 = a1[[1]];	tdm1 = a1[[2]]


# a2 = preprocess.ngrams(x, 1, 2);	# apply preprocess() func to raw input data with min 1 and max 2 words per term
# tdm0 = a2[[1]];	tdm1 = a2[[2]]

dim(tdm1)			# check its basic characteristics



## remove blank documents (i.e. columns with zero sums)

a0 = NULL; 

for (i1 in 1:ncol(tdm0)){ if (sum(tdm0[, i1]) == 0) {a0 = c(a0, i1)} }

length(a0)		# no. of empty docs in the corpus

if (length(a0) >0) { tdm01 = tdm0[, -a0]} else {tdm01 = tdm0};	dim(tdm01)	# under TF weighing

if (length(a0) >0) { tdm11 = tdm1[, -a0]} else {tdm11 = tdm1};	dim(tdm11)	# under TFIDF weighing

inspect(tdm01[1:5, 1:10])		# to view elements in tdm1, use inspect()

if (length(a0)>0) {names1 = as.matrix(names[-a0,1])}	else{names1 = names}; dim(names1)


# convert tdms to dtms i.e. docs become rows and terms become columns
# change dtm weighting from Tf (term freq) to TfIdf (term freq Inverse Doc freq)

dtm0 = t(tdm01)				# docs are rows and terms are cols

dtm = t(tdm11)					# new dtm with TfIdf weighting

# rearrange terms in descending order of Tf and view

a1 = apply(dtm0, 2, sum);

a2 = sort(a1, decreasing = TRUE, index.return = TRUE)

dtm01 = dtm0[, a2$ix];	inspect(dtm01[1:10, 1:10])

dtm1 = dtm[, a2$ix];	inspect(dtm1[1:10, 1:10])



## now plot some illuminating stuff ##

windows()			# prevent overwriting existing plots

makewordc(dtm1);		# view wordcloud

windows();	 makeposwordc(dtm1);	 

windows();	 makenegwordc(dtm1)


# clusdend(dtm1)			# view dendogram 

# what if we used TF weighing for our basic descriptive analyses?

windows();	makewordc(dtm0);		# view wordcloud

clusdend(dtm0)			# view dendogram 



### --- semantic network analyses --- ###

windows();	semantic.na(dtm1)		# apply func and see

windows();	ppl.semantic.na(dtm1, names1)		# apply func on current data
