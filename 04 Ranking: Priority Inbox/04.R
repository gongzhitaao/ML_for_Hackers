library(tm)
library(ggplot2)
library(plyr)

easyham.path <- file.path("data", "easy_ham")

## -------------------------------------------------------------------
## Functions for extracting the feature set
## -------------------------------------------------------------------

parse.email <- function(path) {
  ## Read in email body into a vector, one line per element.
  full.msg <- get.msg(path)

  ## The first empty line separates the header from the body.
  n <- which(full.msg == "")[1]
  header <- full.msg[seq(1, n-1, 1)]
  body <- full.msg[seq(n+1, length(full.msg), 1)]
  body <- body[which(body != "")]       #remove empty lines in body

  ## Extract various information for further processing.
  date <- get.date(header)
  from <- get.from(header)
  subj <- get.subj(header)
  msg <- paste(body, collapse="\n")

  return(c(date, from, subj, msg, path))
}

get.msg <- function(path) {
  con <- file(path, open="rt", encoding="latin1")
  msg <- readLines(con)
  close(con)
  return(msg)
}

get.date <- function(header) {
  date <- header[grepl("^Date: ", header)]
  ## note the blank after `:`
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
  date <- gsub("^\\s+|\\s+$", "", date)
  return(strtrim(date, 25))
}

get.from <- function(header) {
  from <- header[grepl("^From: ", header)]
  from <- strsplit(from, "[\":<>]")[[1]]
  from <- from[which(from != "" & from != " ")]
  return(tolower(from[grepl("@", from)][[1]]))
}

get.subj <- function(header) {
  subj <- header[grepl("^Subject: ", header)]
  if (length(subj) > 0)
    return(tolower(strsplit(subj, "Subject: ")[[1]][2]))
  else
    return("")
}

## With all those helper functions defined, we are now extracting
## information from the emails.
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
easyham.parse <- lapply(easyham.docs, function(p) {
  parse.email(file.path(easyham.path, p))
})

ehparse.matrix <- do.call(rbind, easyham.parse)
allparse.df <- data.frame(ehparse.matrix, stringsAsFactors=FALSE)
names(allparse.df) <- c("Date", "From.Email", "Subject", "Message",
                        "Path")

date.converter <- function(dates, pat1, pat2) {
  pat1.convert <- strptime(dates, pat1)
  pat2.convert <- strptime(dates, pat2)
  pat1.convert[is.na(pat1.convert)] <-
    pat2.convert[is.na(pat1.convert)]
  return(as.POSIXct(pat1.convert))
}

## These are the two patterns appearing in the dataset, I guess we
## need to identify the date format ourselves.  Haven't found a clever
## way of auto-detecting the date format.  Anyway for this simple
## example, let's assume that this is enough.
pat1 <- "%a, %d %b %Y %H:%M:%S"
pat2 <- "%d %b %Y %H:%M:%S"

allparse.df$Date <- date.converter(allparse.df$Date, pat1, pat2)

priority.df <- allparse.df[with(allparse.df, order(Date)),]
priority.train <- priority.df[1:round(nrow(priority.df) / 2),]

## -------------------------------------------------------------------
## Creating a weighting scheme for ranking
## -------------------------------------------------------------------

## 1. Social activity, based on senders, i.e., `from` field

## Simply count emails from each sender.  The more we get emails from
## a sender, the more important we think the sender is.  If we get x
## emails from sender A, then the social activity score for A is
## log(x+1).  The `+1` is neccessary in case x equals zero.

from.weight <- ddply(priority.train, .(From.Email), summarize,
                     Freq=length(Subject))
from.weight <- from.weight[with(from.weight, order(Freq)),]

## Plot the number of mails received from different senders
from.ex <- subset(from.weight, Freq > 6)
fig4.2 <- ggplot(from.ex) +
  geom_rect(aes(xmin=1:nrow(from.ex) - 0.5,
                xmax=1:nrow(from.ex) + 0.5,
                ymin=0,
                ymax=Freq,
                fill="lightgrey",
                color="darkblue")) +
  scale_x_continuous(breaks=1:nrow(from.ex), labels=from.ex$From.Email) +
  coord_flip() +
  scale_fill_manual(values=c("lightgrey"="lightgrey"), guid="none") +
  scale_color_manual(values=c("darkblue"="darkblue"), guide="none") +
  ylab("Number of Emails Received (truncated at 6)") +
  xlab("Sender Address") +
  theme_bw() +
  theme(axis.text.y=element_text(size=10, hjust=1))
## print(fig4.2)
## ggsave(plot=fig4.2, filename="fig4_2.pdf", height = 4.8, width = 7)

## log weight scheme, decide between ln and log10
from.test <- transform(from.weight, Weight=log(Freq+1),
                       log10Weight=log10(Freq+1))

fig4.4 <- ggplot(from.test, aes(x=1:nrow(from.test))) +
  geom_line(aes(y=Weight, linetype="ln")) +
  geom_line(aes(y=log10Weight, linetype="log10")) +
  geom_line(aes(y=Freq, linetype="Absolute")) +
  scale_linetype_manual(values=c("ln"=1, "log10"=2, "Absolute"=3),
                        name="Scaling") +
  xlab("") +
  ylab("Number of emails Receieved") +
  theme_bw() +
  theme(axis.text.y=element_blank(), axis.text.x=element_blank())
## print(fig4.4)
## ggsave(plot=fig4.4, filename="fig4_4.pdf", height = 4.8, width = 7)

## By comparison, we decide to go with natural log
from.weight <- transform(from.weight, Weight=log(Freq+1))

## 2. Senders' activity in threads

## Similar to the previous ranking scheme, this time we count users'
## activity in threads.

find.thread <- function(email.df) {
  ## We assume that threads begines with "re: "
  is.thread <- grepl("^re: ", email.df$Subject)
  threads <- email.df$Subject[is.thread]
  senders <- email.df$From.Email[is.thread]
  return(cbind(senders, threads))
}

thread.matrix <- find.thread(priority.train)

email.thread <- function(thread.matrix) {
  senders <- thread.matrix[,1]
  senders.freq <- table(senders)
  senders.matrix <- cbind(names(senders.freq), senders.freq,
                          log(senders.freq+1))
  senders.df <- data.frame(senders.matrix, stringsAsFactors=FALSE)
  row.names(senders.df) <- 1:nrow(senders.df)
  names(senders.df) <- c("From.Email", "Freq", "Weight")
  senders.df$Freq <- as.numeric(senders.df$Freq)
  senders.df$Weight <- as.numeric(senders.df$Weight)
  return(senders.df)
}

senders.df <- email.thread(thread.matrix)

## 3. Threads message activity

## Threads that have more messages sent over a short period of time
## are more active and consequently more important.  And thus, senders
## of these threads are more important.

get.thread <- function(thread.matrix, email.df) {
  threads <- unique(thread.matrix[,2])
  thread.count <- lapply(threads, function(t) {
    count.thread(t, email.df)
  })
  thread.matrix <- do.call(rbind, thread.count)
  return(cbind(threads, thread.matrix))
}

count.thread <- function(thread, email.df) {
  ## Note the `fixed=TRUE` parameter, this would match the pattern as
  ## it is, instead of treating it as a regex string.
  thread.times <- email.df$Date[grep(thread, email.df$Subject,
                                     fixed=TRUE)]
  freq <- length(thread.times)
  min.time <- min(thread.times)
  max.time <- max(thread.times)
  time.span <- as.numeric(difftime(max.time, min.time, units="secs"))
  if (freq < 2)
    return(c(NA, NA, NA))
  else {
    trans.weight <- freq / time.span
    log.trans.weight <- 10 + log(trans.weight, base=10)
    return(c(freq, time.span, log.trans.weight))
  }
}

thread.weights <- get.thread(thread.matrix, priority.train)
thread.weights <- data.frame(thread.weights, stringsAsFactors=FALSE)
names(thread.weights) <- c("Thread", "Freq", "Response", "Weight")
thread.weights$Freq <- as.numeric(thread.weights$Freq)
thread.weights$Response <- as.numeric(thread.weights$Response)
thread.weights$Weight <- as.numeric(thread.weights$Weight)
thread.weights <- subset(thread.weights, !is.na(thread.weights$Freq))

## 4. Terms frequency in active threads

## Threads with more frequent words are more important than those with
## less frequent words.  And senders of these threads deserve more
## importance score.

count.term <- function(term.vec, control) {
  vec.corpus <- Corpus(VectorSource(term.vec))
  vec.tdm <- TermDocumentMatrix(
      vec.corpus, control=list(stopwords=stopwords(),
                      removeNumbers=TRUE, removePunctuation=TRUE))
  return(rowSums(as.matrix(vec.tdm)))
}

thread.terms <- count.term(thread.weights$Thread)
thread.terms <- names(thread.terms)
term.weights <- sapply(thread.terms, function(t) {
  mean(thread.weights$Weight[grepl(t, thread.weights$Thread,
                                   fixed=TRUE)])
})
term.weights <- data.frame(
    list(Term=names(term.weights), Weight=term.weights),
    stringsAsFactors=FALSE, row.names=1:length(term.weights))
term.weights <- term.weights[which(!is.na(term.weights$Weight)),]

## 5. Terms frequency in all emails

## The more important words a email contains, the more important it
## is.  This weighting scheme is similar to the previous one where we
## only consider the words in threads.

msg.terms <- count.term(priority.train$Message)
msg.weights <- data.frame(list(Term=names(msg.terms),
                               Weight=log(msg.terms, base=10)),
                          stringsAsFactors=FALSE,
                          row.names=1:length(msg.terms))
msg.weights <- subset(msg.weights, Weight > 0)

## -------------------------------------------------------------------
## Training and testing the ranker
## -------------------------------------------------------------------

## Rank each message based on the five features mentioned above.
## Multipling the score for each feature gives us the weight for a
## message.
rank.message <- function(path) {
  ## msg: data, from, subj, msg, path
  msg <- parse.email(path)

  ## collect weight value for each features, just to recap briefly:
  w <- c()

  ## 1. The more emails from a sender, the more weights assigned to
  ## the sender.
  idx <- which(from.weight$From.Email == msg[2])
  w <- c(w, ifelse(length(idx) > 0, from.weight$Weight[idx], 1))

  ## 2. Similar to previous one, but only consider the threads.
  idx <- which(senders.df$From.Email == msg[2])
  w <- c(w, ifelse(length(idx) > 0, senders.df$Weight[idx], 1))

  ## 3. Senders envolved in threads with more messages over a shorter
  ## period of time get more weights.
  is.thread <- grepl("^re: ", msg[3])
  if (is.thread) {
    idx <- grep(msg[3], thread.weights$Thread, fixed=TRUE)
    w <- c(w, ifelse(length(idx) > 0,
                     mean(thread.weights$Weight[idx]), 1))
  } else {
    w <- c(w, 1)
  }

  ## 4. Term frequency count in thread subjects
  terms <- count.term(msg[3])
  idx <- match(names(terms), term.weights$Term)
  idx <- idx[which(!is.na(idx))]
  w <- c(w, ifelse(length(idx) > 0, mean(term.weights$Weight[idx]), 1))

  ## 5. Term frequency count in all email bodys
  msg.terms <- count.term(msg[4])
  idx <- match(names(msg.terms), msg.weights$Term)
  idx <- idx[which(!is.na(idx))]
  w <- c(w, ifelse(length(idx) > 0, mean(msg.weights$Weight[idx]), 1))

  rank <- prod(w)

  return(c(msg[1], msg[2], msg[3], rank))
}

## Training process
train.path <- priority.df$Path[1:(round(nrow(priority.df) / 2))]
train.ranks <- lapply(train.path, rank.message)
train.ranks.matrix <- do.call(rbind, train.ranks)
train.ranks.matrix <- cbind(train.path, train.ranks.matrix, "TRAINING")
train.ranks.df <- data.frame(train.ranks.matrix, stringsAsFactors=FALSE)
names(train.ranks.df) <-
  c("Message", "Date", "From", "Subj", "Rank", "Type")
train.ranks.df$Rank <- as.numeric(train.ranks.df$Rank)

priority.threshold <- median(train.ranks.df$Rank)
train.ranks.df$Priority <-
  ifelse(train.ranks.df$Rank >= priority.threshold, 1, 0)

fig4.5 <- ggplot(train.ranks.df, aes(x=Rank)) +
  geom_density(aes(fill="darkred"), alpha=0.5) +
  geom_vline(xintercept=priority.threshold, linetype="dotted") +
  xlab("Rank") + ylab("Density") +
  scale_fill_manual(values=c("darkred"="darkred"), guide="none") +
  theme_bw()
## print(fig4.5)
## ggsave(plot=threshold.plot, filename="fig4_5.pdf", height=4.7, width=7)

## Now testing...
test.path <- priority.df$Path[((round(nrow(priority.df) / 2)) + 1) :
                                nrow(priority.df)]
test.ranks <- lapply(test.path, rank.message)
test.ranks.matrix <- do.call(rbind, test.ranks)
test.ranks.matrix <- cbind(test.path, test.ranks.matrix, "TESTING")
test.ranks.df <- data.frame(test.ranks.matrix, stringsAsFactors=FALSE)
names(test.ranks.df) <-
  c("Message", "Date", "From", "Subj", "Rank", "Type")
test.ranks.df$Rank <- as.numeric(test.ranks.df$Rank)
test.ranks.df$Priority <-
  ifelse(test.ranks.df$Rank >= priority.threshold, 1, 0)

## Combine the training and testing dataset
final.df <- rbind(train.ranks.df, test.ranks.df)

fig4.6 <- ggplot(final.df, aes(x=Rank)) +
  geom_density(aes(group=Type, fill=Type), alpha=0.5) +
  geom_vline(xintercept=priority.threshold, linetype="dotted") +
  xlab("Rank") + ylab("Density")
## print(fig4.6)
## ggsave(plot=fig4.6, filename="fig4_6.pdf", height=4.7, width=7)
