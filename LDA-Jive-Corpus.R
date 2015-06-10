
################################################################################
# Read the RSS feed from the CA Infrastructure Management community, and perform
# Topic Modeling using LDA
################################################################################
library(XML)
library(RCurl)
library(RJSONIO)
library(tm) 
library(SnowballC)  
library(qdapDictionaries)
library(plyr) 
library(RColorBrewer) 
library(ggplot2) 
library(scales) 
library(apcluster)
library(proxy)
library(cluster)
library(lda)
library(LDAvis)
library(topicmodels)
library(servr)
library(Rcpp)

options(stringsAsFactors = FALSE)

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", 
                                                 package = "RCurl")))
# Extract Data from Communities
N                <- 200                                          # number of items to fetch
xml.url          <- "https://communities.ca.com/community/feeds/messages?community=2019"
xml.url          <- paste(xml.url, "&full=true&numItems=", N, sep = "")

script           <- getURL(xml.url)
doc              <- xmlParse(script)

titles        <- xpathSApply(doc,'//item/title', xmlValue)
pubdates      <- xpathSApply(doc,'//item/pubDate', xmlValue)
descriptions  <- xpathSApply(doc,'//item/description', xmlValue)
links         <- xpathSApply(doc,'//item/link',xmlValue)


## get the full text of the linked articles
articles <- sapply(links, getURL)

## write a function to extract the text from the articles
processHTML <- function(html) {
  doc <- htmlTreeParse(html, useInternalNodes=TRUE)
  text <- unlist(xpathApply(doc, "//p", xmlValue))
  ## combine each paragraph, separated by two line breaks
  text.comb <- paste(text, collapse="\n\n")
  return(text.comb)
}

## apply text extraction function to each html article
fulltext           <-  sapply(articles, processHTML)
combined.text      <- cbind(titles, fulltext)
c                  <- data.frame(combined.text)

# concatenate each entry into one long string
d <- data.frame(paste(c[, 1], c[, 2], sep = " "))

# massage the data into a "Corpus"
g <- DataframeSource(data.frame(as.character(d[,1])))
docs <- Corpus(g)

# pre-process the data - convert to lower case, strip out punctuation, etc
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("SMART"))

# Remove additional terms from this list of stop words
stop.words <- read.csv("~/Spectrum-R/Exploratory/APM Corpus/Stop Word List.csv", 
                       header = TRUE, stringsAsFactors = FALSE)
docs <- tm_map(docs, removeWords, stop.words$StopWord) 

# and remove these terms (augment this list as appropriate)
docs <- tm_map(docs, removeWords, c("apm", "ca", "cloud", "support",
                                    "teach", "file", "case", "request",
                                    "close", "open", "information"))

# Continue processing by stripping out white space, and stem the word list
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

docs <- data.frame(text=unlist(sapply(docs, `[`, "content")), 
                   stringsAsFactors = FALSE)

# tokenize on space and output as a list:
doc.list <- strsplit(docs$text, "[[:space:]]+")

# remove terms with fewer than 3 characters
doc.list <- rapply(doc.list, function(x) x[nchar(x) > 2], how = c("replace"))

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (200)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table) 

# Model tuning parameters:
K <- 6
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
set.seed(347)
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi   <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

cases    <- list(phi            = phi,
                 theta          = theta,
                 doc.length     = doc.length,
                 vocab          = vocab,
                 term.frequency = term.frequency)

# create the JSON object to feed the visualization:
json <- createJSON(phi            = cases$phi, 
                   theta          = cases$theta, 
                   doc.length     = cases$doc.length, 
                   vocab          = cases$vocab, 
                   term.frequency = cases$term.frequency)

serVis(json)

