#!/usr/bin/Rscript --vanilla

###########################################################
##                                                       ##
##   explore.R                                           ##
##                                                       ##
##                Author: Tony Fischetti                 ##
##                        tony.fischetti@gmail.com       ##
##                                                       ##
###########################################################

# workspace cleanup
rm(list=ls())

# options
options(echo=TRUE)
options(stringsAsFactors=FALSE)

# cli args
args <- commandArgs(trailingOnly=TRUE)

# libraries
library(dplyr)
library(magrittr)
library(assertr)
library(reshape2)
library(ggplot2)
library(rvest)

rstats <- read.table("./data/R-hashtag-data-tab.txt",
                     sep="\t", fill=TRUE, comment.char="")
rstats$V1 <- NULL
names(rstats) <- c("Month", "Day", "Time", "User", "Tweet")

# most prolific #rstats tweeters
rstats %>%
  group_by(User) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) -> prolific.rstats.tweeters

write.csv(prolific.rstats.tweeters, "prolific.csv", row.names=FALSE)

get.num.tweets <- function(handle){
  tryCatch({
    unraw <- function(raw_str){
      if(grepl("K", raw_str)){
        return(as.numeric(sub("K", "", raw_str))*1000)
      }
      raw_str <- sub(",", "", raw_str)
      return(as.numeric(raw_str))
    }
    html(paste0("http://twitter.com/", sub("@", "", handle))) %>%
      html_nodes(".is-active .ProfileNav-value") %>%
      html_text() %>%
      unraw
    },
    error=function(cond){return(NA)})
}

get.num.tweets("hadleywickham")
get.num.tweets("treycausey")
get.num.tweets("tonyfischetti")
get.num.tweets("@KirkDBorne")

bk.prolific.rstats.tweeters <- prolific.rstats.tweeters
prolific.rstats.tweeters <- bk.prolific.rstats.tweeters

sink("~/Desktop/log.txt")
amount <- prolific.rstats.tweeters %>% nrow
prolific.rstats.tweeters$num.of.tweets <- sapply(1:amount,
  function(counter){
    percent.done <- round((counter/amount)*100)
    user <- prolific.rstats.tweeters[counter,1]
    Sys.sleep(abs(rnorm(n=1, mean=2, sd=.5)))
    cat(paste0("attempting to get ", user, "'s tweet count\n"))
    num.tweets <- get.num.tweets(user)
    cat(paste0("got ", num.tweets, "\n"))
    cat(paste0("                           percent done: ", percent.done, "%\n"))
    return(num.tweets)
  })

prolific.rstats.tweeters %<>% mutate(ratio=count/num.of.tweets)

prolific.rstats.tweeters %>% write.csv("output.csv", row.names=FALSE)


