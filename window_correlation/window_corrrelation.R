# Extracts passages around particular domestic terms and then calculates relative word frequencies for those 
# windows versus the corpus as a whole

library(tidyverse)
library(readxl)
library(udpipe)
source("../tools/build_dtm.R")

# import and filter metadata
chadwyck.metadata.path <- "../chadwyck/metadata.xlsx"
C19.metadata <- read_xlsx(chadwyck.metadata.path) |> 
  filter(subcorpus == "Nineteenth-Century_Fiction") |> 
  arrange(id)

read.files <- function(path){
  txt.files <- list.files(path, full.names = TRUE)
  raw.text <- map(txt.files, read_file)
  clean.text <- map(raw.text, str_squish)
  return(clean.text)
}

# path to the relevant .txt files
chadwyck.txt.path <- "../chadwyck/txt/Nineteenth-Century_Fiction/"
clean.text <- read.files(chadwyck.txt.path)

# POS tag the novels
m_eng <- udpipe_download_model(language = "english-ewt", overwrite = FALSE)
m_eng <- udpipe_load_model("../english-ewt-ud-2.5-191206.udpipe")

tag_novels <- function(corpus, corpus_names, model){
  tagged.novels <- vector("list", length(corpus))
  for (i in seq_along(corpus)){
    tagged.novel <- udpipe_annotate(model, corpus[[i]],
                                    doc_id = corpus_names[[i]],
                                    tagger = "default",
                                    parser = "none") |> 
                                    as.data.frame()
    tagged.novels[[i]] <- tagged.novel
  }
  return(tagged.novels)
}

# This takes a couple of hours (could be made parallel)
# Note that the model takes up about 5.5 GB of memory in R
if (file.exists("chadwyck-pos.rds")){
  print("Loading model from file...")
  all.tagged.novels <- read_rds("chadwyck-pos.rds")
} else {
  print("POS model not found: generating now...")
  print("(This may take several hours)")
  all.tagged.novels <- tag_novels(clean.text[1:2], C19.metadata$title, m_eng)
  write_rds(all.tagged.novels, "chadwyck-pos.rds")
}

# get the list of terms
domestic.terms.df <- read_xlsx("window_correlation/2022-10-14_domestic-terms_short.xlsx")
domestic.terms <- domestic.terms.df$term
domestic.terms.long.df <- read_xlsx("window_correlation/2022-10-16_domestic-terms_long.xlsx")
domestic.terms <- domestic.terms.long.df$term


# find target word in a novel and generate sentences windows around them
get.novel.windows <- function(parsed.novel, target.word, before, after){
  # find target words in novel lemmas
  hits <- which(unlist(
    map(target.word, grepl, x = parsed.novel$lemma)
    ))
  hit.sentences <- parsed.novel[hits,]$sentence_id
  
  # get start and end sentence_id for each window
  num.sentences <- max(parsed.novel$sentence_id)
  start.sentence.id <- pmax(hit.sentences - before, 1)
  end.sentence.id <- pmin(hit.sentences + after, num.sentences)
  
  windows <- data.frame(start.sentence.id, end.sentence.id)
  return(windows)
}

# generate a random sample of sentence windows from a parsed novel
sample.windows <- function(parsed.novel, num.windows, window.before, window.after){
  num.sentences <- max(parsed.novel$sentence_id)
  window.centers <- sample.int(num.sentences, num.windows, replace = TRUE)
  window.starts <- pmax(window.centers - window.before, 1)
  window.ends <- pmax(window.centers + window.after, num.sentences)
  windows <- data.frame(window.starts, windows.ends)
  return(windows)
}

# turns a pair of sentence_ids into a string containing the matching POS from
# the same window. if lemma == TRUE, returns the lemmas instead
get.window.POS <- function(start, end, tagged.novel, pos, lemma = FALSE){
  slice <-
    tagged.novel |> filter((sentence_id >= start) &
                             (sentence_id <= end)) |> filter(upos == pos)
  if (lemma){
    return(paste(slice$lemma, collapse = " ", sep = ""))
  } else {
    return(paste(slice$token, collapse = " ", sep = ""))
  }
}

# given a set of windows, a novel and a POS, extracts all the relevant tokens
pos.from.windows <- function(tagged.novel, windows, pos, lemma){
  tokens <- map2(windows[,1], windows[,2], 
                 get.window.POS, 
                 tagged.novel = tagged.novel, 
                 pos = pos, lemma = lemma)
  return(paste(tokens, collapse = " ", sep = ""))
}

# yes, not technically collocates, whatever
# returns all terms that appear within the appropriate window length of the term
# across the corpus
get.term.correlates <- function(term, tagged.corpus, filenames, pos, 
                                before = 5, after = 5, lemma = FALSE){
  # get list of windows for all novels
  windows <- map(tagged.corpus, get.novel.windows, 
                 target.word = term, 
                 before = before, 
                 after = after)
  n.windows <- map(windows, nrow) |> unlist() |> sum()
  
  # get a list of all the terms from all the different contexts
  collocates <- map2(tagged.corpus, windows, pos.from.windows, pos = pos, lemma = lemma)
  names(collocates) <- filenames
  return(
    data.frame(term = term, count = n.windows, collocates)
  )
}

# build table of correlates for each term
correlates.df <- map(domestic.terms, get.term.correlates, 
  tagged.corpus = all.tagged.novels[1:125],
  filenames <- C19.metadata$title[1:125],
  pos = "NOUN", before = 1, after = 3) |> bind_rows()


# merge text contexts for a word and return a matrix of frequencies
count.contexts <- function(correlates.df, min_count = 5, counts = FALSE){
  contexts <- correlates.df |> select(-term, -count) |> unite(col = context, sep = " ") |> unlist()
  context.len <- str_count(contexts, pattern = fixed(" ")) + 1
  dtm <- dtm.from.list(contexts, doc.names = correlates.df$term, term.count.min = min_count, remove.stopwords = FALSE)
  if (counts){
    return(dtm)
  } else{ 
    frequencies <- dtm / context.len
    return(frequencies)
    }
}
frequencies <- count.contexts(correlates.df, min_count = 5, counts = FALSE)
counts <- count.contexts(correlates.df, min_count = 5, counts = TRUE)

# compute baseline stats
novel.texts <- map(all.tagged.novels, pull, token)
novel.texts <- map(novel.texts, paste, sep = " ", collapse = " ")
all.dtm <- dtm.from.list(novel.texts, 
                         doc.names = C19.metadata$title, 
                         remove.stopwords = FALSE, 
                         term.count.min = 0)
all.counts <- Matrix::colSums(all.dtm)
all.frequencies <- all.counts / sum(all.counts)

# create a table of frequencies for each term
write.freq.tables <- function(frequencies, all.counts, all.frequencies){
  all.freq.tables <- vector("list", nrow(frequencies))
  for (i in seq.int(nrow(frequencies))){
    cur.term <- rownames(frequencies)[i]
    cur.correlates <- names(cur.frequencies)
    cur.correlates <- cur.correlates[order(cur.correlates)]
    cur.frequencies <- frequencies[i,]
    cur.frequencies <- cur.frequencies[order(names(cur.frequencies))]
    cur.base.counts <- all.counts[which(names(all.counts) %in% names(cur.frequencies))]
    cur.base.counts <- cur.base.counts[order(names(cur.base.counts))]
    cur.base.frequencies <- all.frequencies[which(names(all.frequencies) %in% names(cur.frequencies))]
    cur.base.frequencies <- cur.base.frequencies[order(names(cur.base.frequencies))]
    cur.obs.exp <- cur.frequencies / cur.base.frequencies
    term.df <- data.frame(cur.correlates, 
                          cur.frequencies, 
                          obs.exp = cur.obs.exp, 
                          corpus.frequencies = cur.base.frequencies, 
                          corpus.counts = cur.base.counts)
    term.df <- term.df |> 
      arrange(-obs.exp) |> 
      filter(corpus.counts >= 5)
    all.freq.tables[[i]] <- term.df
    filename <- paste("window_correlation/window-correlates/", cur.term, "-correlates.csv", sep = "")
    write_csv(term.df, file = filename)
  }
  return(all.freq.tables)
}

frequency.tables <- write.freq.tables(frequencies, all.counts, all.frequencies)
