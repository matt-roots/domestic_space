library(tidyverse)
library(ggpubr)

# OPTIONS
kSingleFileMode <-  TRUE
kNovelPath <- "../CHICAGO_CORPUS/cleaned_chicago/00000001.txt"
# if you change this to something longer than the empty string ("") it'll show on the plot:
kNovelTitle <- "Democracy" 

kChadwyckPath <- "../chadwyck/txt/Nineteenth-Century_Fiction/"

# how many non-domestic sentences before domesticity starts to fall
kLag <- 25

# how fast does domesticity fall once lag elapsed (value should probably be > 90%)
kDecayRate <- 0.975

# change these terms as needed; terms should be in parentheses divided by pipes
# ? makes the character before optional; other regex functions also work
kDomesticTerms <- regex("(living room)|(bed ?room)|(family room)|(parlou?r)|(kitchen)|(home)",
                        ignore_case = TRUE)

# Text Parsing
# ============

get.novel.sentences <- function(path){
  raw.text <- read_file(path) |> str_squish()
  # split on sentence-ending punctuation and return as a vector of chars
  return(unlist(map(raw.text, str_split, '[.?!]')))
}

# this is the function that determines how domestic a sentence is
# this should return (something that can be coerced to) a number
get.sentence.domesticity <- function(sentence){
  domesticity <- str_detect(sentence, kDomesticTerms)
  return(domesticity)
}

get.novel.domesticity <- function(novel.sentences, domestic.lag, decay.rate){
  # get domesticity ratings for each sentence
  domesticity.vals <- unlist(map(novel.sentences, get.sentence.domesticity))
  
  # set the lag counter
  lag.counter <- domestic.lag
  
  # create vector for probabilities and set [1] (special case)
  domesticity.probs <- vector("double", length(novel.sentences))
  domesticity.probs[1] <- ifelse(test = domesticity.vals[1],
                                yes = 0.75,
                                no = 0.5)
  for (i in seq_along(novel.sentences)[-1]){ # skip the first
    prev.domesticity <- domesticity.probs[i - 1]
    # current sentence is not domestic:
    if (!domesticity.vals[i]) {
      lag.counter <- max(lag.counter - 1, 0) # tick towards decaying
      # decay cur probability if counter is at 0:
      domesticity.probs[i] <- ifelse(test = (lag.counter == 0),
                                     yes = prev.domesticity * decay.rate,
                                     no = prev.domesticity)
    } else { # domestic sentence
      # reset counter
      lag.counter <-  domestic.lag
      # increase domestic probability
      domesticity.probs[i] <- (prev.domesticity + domesticity.vals[i]) / 2
    }
  }
  # return the data as a tibble (for safer printing)
  domesticity.df <- tibble(
    sentence.num = seq_along(novel.sentences),
    sentence.domesticity = domesticity.vals,
    running.prob = domesticity.probs
  )
  return(domesticity.df)
}

make.domesticity.plot <- function(domesticity.df, novel.title = NOVEL.TITLE, multiplot = FALSE){
  plt <- ggplot(data = domesticity.df,
                aes(x = sentence.num,
                    y = running.prob)) +
    geom_smooth(formula = y ~ x, method = "loess", span = 0.15) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), 
                       breaks = seq(0, 1, 0.25),
                       limits = c(0,1)) +
    theme_pubr()
  # if doing a single plot, add some details
  if (!multiplot) {
    if (nchar(novel.title)) {
      novel <- novel.title
    } else {
      novel <- str_extract(kNovelPath, "(?<=/)[^/]+$")
    }
    title <- paste("Domestic probabilities for", novel)
    plt <- plt + 
      geom_line() +
      labs(title = title,
           y = "Probability (domestic space)",
           x = "Sentence number")
  } else {
    # add simple title
    plt <- plt
    
  }
  return(plt)
}

domesticity.plot.from.path <- function(path, lag){
  sentences <- get.novel.sentences(path)
  domesticity.probs <- get.novel.domesticity(sentences, 
                                             domestic.lag = kLag,
                                            decay.rate = kDecayRate)
  return(make.domesticity.plot(domesticity.probs))
  
}

domesticity.plot.from.path(kNovelPath)
ggsave("operationalizing_model/domesticity-plot.jpg", width = 1600, height = 1600, units = "px")

corpus.files <- list.files(kChadwyckPath, full.names = TRUE)

build.corpus.sheets <- function(corpus.files, sheet.size = 100, lag = LAG, decay = DECAY.RATE){
  # pre-allocate so we don't have to move huge plot objects around
  plt.length <- ceiling(length(corpus.files) / sheet.size)
  sheets <- vector(mode = "list", plt.length)
  
  # loop through the files in large batches
  j = 1
  for (i in seq(1, length(corpus.files), sheet.size)){
    # get the batch from current to next, or end
    cur.files <- corpus.files[i:(i+sheet.size-1)]
    cur.files <- cur.files[!is.na(cur.files)]
    
    # generate the data and plots
    cur.sentences <- map(cur.files, get.novel.sentences)
    cur.probs <- map(cur.sentences, get.novel.domesticity, lag, decay)
    cur.plts <- suppressWarnings(map2(cur.probs, cur.files, make.domesticity.plot, multiplot = TRUE))
    cur.plts <- map(cur.plts, rremove, "axis.title")
    cur.plts <- map(cur.plts, rremove, "axis.text")
    
    # combine using ggarrange
    cols = ceiling(sqrt(sheet.size))
    return(cur.plts)
    sheet <- ggarrange(plotlist = cur.plts, 
                       ncol = cols)
    # increment and index along
    sheets[j] <- sheet
    j <- j + 1
  }
}
