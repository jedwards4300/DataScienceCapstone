## INITIALIZE
library(tm)
library(RWeka)
 
set.seed(1234)

#get statistics on files
fileStat <- data.frame(Line_Count = c(system("wc -l final/en_US/en_US.blogs.txt | cut -f1 -d' '", intern=TRUE),
                                      system("wc -l final/en_US/en_US.news.txt | cut -f1 -d' '", intern=TRUE),
                                      system("wc -l final/en_US/en_US.twitter.txt | cut -f1 -d' '", intern=TRUE)),
                       
                       Word_Count = c(system("wc -w final/en_US/en_US.blogs.txt | cut -f1 -d' '", intern=TRUE),
                                      system("wc -w final/en_US/en_US.news.txt | cut -f1 -d' '", intern=TRUE),
                                      system("wc -w final/en_US/en_US.twitter.txt | cut -f1 -d' '", intern=TRUE)), 
                       row.names = c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt"), stringsAsFactors = FALSE)


## DOWNLOAD AND CLEAN DATA
size <- 20000

con <- file("questions.txt", "r")

quizWords <- readLines(con)
close(con)

quizWords1 <- cleanAndToken(quizWords, 1)
quizWords2 <- cleanAndToken(quizWords, 2)
quizWords3 <- cleanAndToken(quizWords, 3)


con <- file("final/en_US/en_US.news.txt", "r")
en_US.news <- readLines(con)
close(con)

wordList <- character()
ntable <- numeric()
for (i in 0:floor(as.integer(length(en_US.news))/size)) {  
  en_US.news.sample <- en_US.news[(i*size+1):min(c(((i+1)*size),length(en_US.news)))]
  tmp <- cleanAndToken(en_US.news.sample, 1)
  #write(tmp, file = paste("data/n", 1, "grams/en_US.news.", (i+1), sep = ""))
  tmp <- factor(tmp)
  tmp <- factor(tmp, levels=c(wordList, levels(tmp)[!(levels(tmp) %in% wordList)]))
  wordList <- levels(tmp)
  ntable[(length(ntable)+1):length(wordList)] <- 0
  tmp <- table(tmp)  
  ntable <- ntable + tmp    
}

ntable <- sort(ntable, decreasing = TRUE)
wordList <- factor(wordList, levels=names(ntable))
wordList <- sort(wordList)

for(i in seq.int(1,length(ntable),100)) cum_plot[(i-1)/100+1] <- sum(head(ntable, i))/sum(ntable)
plot(seq.int(1,length(ntable),100),cum_plot, type="l")


n2FreqList <- nGramTable(en_US.news,size,1,2) #floor(length(lst)/size)
n3FreqList <- nGramTable(en_US.news,size,1,10,3) #floor(length(lst)/size)
n4FreqList <- nGramTable(en_US.news,size,1,10,4) #floor(length(lst)/size)
n5FreqList <- nGramTable(en_US.news,size,1,20,5) #floor(length(lst)/size)

nGramFreq <- function(lst, size, n) {
  ptm <- proc.time()
  j <- n-1
  ngramList <- character()
  nFreqList <- numeric()
  for (i in 0:floor(length(lst)/size)) {
    tmp <- lst[(i*size+1):min(c(((i+1)*size),length(lst)))]
    tmp <- cleanAndToken(tmp, n)
    #write(tmp, file = paste("data/n", n, "grams/en_US.news.", (i+1), sep = ""))
    tmp <- strsplit(split = " ", x = tmp)
    tmp <- sapply(tmp, function(x) paste(x[1:j], collapse = " "))
    tmp <- factor(tmp)
    tmp <- factor(tmp, levels=c(ngramList, levels(tmp)[!(levels(tmp) %in% ngramList)]))
    ngramList <- levels(tmp)
    nFreqList[(length(nFreqList)+1):length(ngramList)] <- 0
    tmp <- table(tmp)
    nFreqList <- nFreqList + tmp
    print(paste("step number", (i+1)*size, "time:", (proc.time()-ptm)[1]))
  }
  nFreqList <- sort(nFreqList, decreasing = TRUE)
  return(nFreqList)
}
  
  
nGramTable <- function(lst, size, numStart, numEnd, n) {
  ptm <- proc.time()
  j <- n-1
  ngramList <- character()
  nFreqList <- numeric()
  for (i in (numStart-1):(numEnd-1)) {
    tmp <- lst[(i*size+1):min(c(((i+1)*size),length(lst)))]
    tmp <- cleanAndToken(tmp, n)
    #write(tmp, file = paste("data/n", n, "grams/en_US.news.", (i+1), sep = ""))
    tmp <- strsplit(split = " ", x = tmp)
    tmp <- sapply(tmp, function(x) paste(x[1:j], collapse = " "))
    tmp <- factor(tmp)
    tmp <- factor(tmp, levels=c(ngramList, levels(tmp)[!(levels(tmp) %in% ngramList)]))
    ngramList <- levels(tmp)
    nFreqList[(length(nFreqList)+1):length(ngramList)] <- 0
    tmp <- table(tmp)
    nFreqList <- nFreqList + tmp
    print(paste("step number", (i+1)*size, "time:", (proc.time()-ptm)[1]))
  }
 
  nFreqList <- sort(nFreqList, decreasing = TRUE)
  ntable <- Matrix(0, ncol = length(wordList), nrow = length(nFreqList), sparse = TRUE)
  
  for (i in (numStart-1):(numEnd-1)) {
    tmp <- lst[(i*size+1):min(c(((i+1)*size),length(lst)))]
    tmp <- cleanAndToken(tmp, n)
    #write(tmp, file = paste("data/n", n, "grams/en_US.news.", (i+1), sep = ""))
    tmp <- strsplit(split = " ", x = tmp)
    if(n > 2) tmp <- lapply(tmp, function(x) c(paste(x[1:j], collapse = " "),x[n]))
    tmp <- as.data.frame(matrix(unlist(tmp), ncol = 2, byrow = TRUE))
    
    tmp$V1 <- factor(tmp$V1, levels = names(nFreqList))
    tmp$V2 <- factor(tmp$V2, levels=wordList)
    tmp <- xtabs(~V1 + V2,tmp,sparse=TRUE) 
    ntable <- ntable + tmp
    print(paste("step number", (i+1)*size, "time:", (proc.time()-ptm)[1]))
  }
  return(list(table = ntable, sum = nFreqList))
}





##backup
nGramTable <- function(list = lst, part_size = size ,n = n) {
  j <- n-1
  ngramList <- character()
  nFreqList <- numeric()
  ntable <- Matrix(0, ncol = length(wordList),nrow =   1, sparse = TRUE)
  for (i in 0:floor(length(lst)/size)) { 
    tmp <- lst[(i*size+1):min(c(((i+1)*size),length(lst)))]
    tmp <- cleanAndToken(tmp, n)
    #write(tmp, file = paste("data/n", n, "grams/en_US.news.", (i+1), sep = ""))
    tmp <- strsplit(split = " ", x = tmp)
    tmp <- sapply(tmp, function(x) paste(x[1:j]))
    tmp <- factor(tmp)
    tmp <- factor(tmp, levels=c(ngramList, levels(tmp)[!(levels(tmp) %in% ngramList)]))
    ngramList <- levels(tmp)
    nFreqList[(length(nFreqList)+1):length(ngramList)] <- 0
    tmp <- table(tmp)  
    nFreqList <- nFreqList + tmp    
  }
  return(nFreqList)
}


##nGramTable <- function(list = lst, part_size = size ,n = n) {
  j <- n-1
  ngramList <- character()
  ntable <- Matrix(0, ncol = length(wordList),nrow =   1, sparse = TRUE)
  for (i in 0:floor(length(lst)/size)) {  
    tmp <- lst[(i*size+1):min(c(((i+1)*size),length(lst)))]
    tmp <- cleanAndToken(tmp, n)
    #write(tmp, file = paste("data/n", n, "grams/en_US.news.", (i+1), sep = ""))
    tmp <- strsplit(split = " ", x = tmp)
    if(n > 2) tmp <- lapply(tmp, function(x) c(paste(x[1:j]),x[n]))
    tmp <- as.data.frame(matrix(unlist(tmp), ncol = 2, byrow = TRUE))
    tmp$V1 <- factor(tmp$V1, levels=c(ngramList, levels(tmp$V1)[!(levels(tmp$V1) %in% ngramList)]))
    tmp$V2 <- factor(tmp$V2, levels=wordList)
    ngramList <- levels(tmp$V1)
    rowDiff <- length(ngramList) - ifelse(i == 0,0,dim(ntable)[1])
    ntable <- rbind(ntable, Matrix(0, ncol = length(wordList),nrow = rowDiff, sparse = TRUE))
    (ifelse(i == 0,1,dim(ntable)[2]+1):length(wordList))
    tmp <- xtabs(~V1 + V2,tmp,sparse=TRUE)  
    ntable <- ntable + tmp
  }
  
}









tmp <- as.data.frame(matrix(unlist(tmp), ncol = 2, byrow = TRUE))
tmp$V1 <- factor(tmp$V1, levels=c(ngramList, levels(tmp$V1)[!(levels(tmp$V1) %in% ngramList)]))
tmp$V2 <- factor(tmp$V2, levels=wordList)
ngramList <- levels(tmp$V1)
rowDiff <- length(ngramList) - ifelse(i == 0,0,dim(ntable)[1])
ntable <- rbind(ntable, Matrix(0, ncol = length(wordList),nrow = rowDiff, sparse = TRUE))
(ifelse(i == 0,1,dim(ntable)[2]+1):length(wordList))
tmp <- xtabs(~V1 + V2,tmp,sparse=TRUE)  
ntable <- ntable + tmp



N3gram <- NGramTokenizer(sample, Weka_control(min = 3, max = 3, delimiters = ' '))
N3gram <- strsplit(split = " ", x = N3gram)
N3gram <- lapply(N3gram, function(x) c(paste(x[1], x[2]),x[3]))
N3gram <- as.data.frame(matrix(unlist(N3gram), ncol = 2, byrow = TRUE))

t(apply(as.matrix(table(N3gram)), 1, FUN = function(x) x/sum(x)))



cleanAndToken <- function(lst,n) {
  # Clean
  removeProfaneWords <- function(x) removeWords(x, c("[^ ]*fuck[^ ]*", "[^ ]*shit[^ ]*","[^ ]*damn[^ ]*","[^ ]*asshole[^ ]*","dick","[^ ]*pussy[^ ]*","[^ ]*cocksucker[^ ]*","[^ ]*nigg[^ ]*"))
  fixCurlyQuote1 <- function(x) gsub(pattern = "(‘|’)", replacement = "'", x)
  fixCurlyQuote2 <- function(x) gsub(pattern = "(“|”)", replacement = "\"", x)
  separatePunct <- function(x) gsub(pattern = "(?!')([[:punct:]])", replacement = " \\1 ", x, perl=TRUE)
  removeSymbols <- function(x) gsub(pattern = "[^a-z| |0-9|.,;:=+-?!@#$%^&*()/'\"]", replacement = " ", x)
  standardNum <- function(x) gsub(pattern = "[[:digit:]]+", replacement = " ## ", x)
  
  lst <- fixCurlyQuote1(lst)
  lst <- fixCurlyQuote2(lst)
  lst <- tolower(lst)
  lst <- removeSymbols(lst)
  lst <- removeProfaneWords(lst)
  lst <- separatePunct(lst)
  lst <- standardNum(lst)
  
  # Token
  NGramTokenizer(lst, Weka_control(min = n, max = n, delimiters = ' '))
  
}



