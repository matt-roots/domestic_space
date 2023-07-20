# Script to sample passages at random and then filter them for likely examples
# of non-domesticity

library(tidyverse)

# constants
# =========
# paths and files
kCorpusPath <- "../FullChadwyckBritishParse/" # corpus directory
kCorpusFiles <- list.files(kCorpusPath, full.names = TRUE)
# domestic seed terms
kDomesticTerms <- read_csv("2023-03-17_term-list.csv", 
                           col_names = "term") |> pull(term)
# annotators names for dividing up the table
kAnnotators <- c("matt", "mark", "alex", "jessica", "svenja")


# OCR filtering
# =============
# currently, we're not doing any OCR filtering, since the poor OCR texts are disproportionately gothic novels

# OCR.table <- read_csv("chadwyck_OCR-quality.csv") |> 
#   filter(Nationality == "British" | Nationality == "Irish")


# passage filtering
# =================

# Helper function that returns a numerical vector to filter passages based on use of domestic terms. 
# Note that `passages` is expected to be list of parsed tables as generated with UDPipe, not actual passages
# NB also that we use lemmas for pattern matching, which should be changed if you need to match inflections

# collapse terms into a single regex because R can't match multiple patterns
terms.pattern <- paste(kDomesticTerms, collapse = "|")
filterPassages <- function(cur.passages, terms = kDomesticTerms){
  # collapse lemmas together to reconstruct sentences (use lemmas for better vocab-based filtering)
  passages.text <- lapply(cur.passages, function(x) paste(x$lemma, collapse = " ")) |> 
    unlist()
  passages.indices <- str_which(passages.text, pattern = terms.pattern, negate = TRUE)
  return(passages.indices)
}


# passage sampling
# ================

# sample n passages at random
if (file.exists("nondomestic_passage_extraction/2023-04-20_nondomestic-passages_full.rds")){
  all.passages <- read_rds("nondomestic_passage_extraction/2023-04-20_nondomestic-passages_full.rds")
} else {
  print("Nondomestic passages not found. Generating now...")
  all.passages <- vector(mode = "list", length = length(kCorpusFiles))
  for (i in seq_along(kCorpusFiles)){
    print(paste0("Extracting passages for ", kCorpusFiles[i]))
    # get the parse
    cur.novel.parse <- read_csv(file = kCorpusFiles[i],
                                col_select = -1) |> suppressMessages()
    # add a column with the filename
    cur.novel.parse$filename <- kCorpusFiles[i]
    # sample random sentence numbers
    cur.novel.length <- max(cur.novel.parse$sentence_id)
    set.seed(i)
    cur.starts <- sample(1:(cur.novel.length - 6), 50)
    # slice out just the windows around the sampled sentences
    cur.passages <- lapply(cur.starts, function(x) cur.novel.parse[which(cur.novel.parse$sentence_id >= x & 
                                                                 cur.novel.parse$sentence_id <= x + 6),])
    cur.passages <- cur.passages[filterPassages(cur.passages)]
    cur.passages <- cur.passages[1:(min(length(cur.passages), 20))]
    all.passages[[i]] <- cur.passages
  }
  # write to file
  write_rds(all.passages, "nondomestic_passage_extraction/2023-04-20_nondomestic-passages_full.rds")
}
# bind all the individual tables together
# full.passage.table <- bind_rows(all.passages)
# write_csv(full.passage.table, "nondomestic_passage_extraction/2023-04-20_nondomestic-passages_full.csv")

# create a sampled subset instead for easier evaluation
# ======
all.passage.tables <- unlist(all.passages, recursive = FALSE)
# as a sentence-per-row table
set.seed(42)
# to sample another set of passages, increase the number sampled and drop the first n passages
sample.passage.tables <- sample(all.passage.tables, 1250)
# create an index to track which passage is which
for (i in seq_along(sample.passage.tables)){
  cur.annotator <- kAnnotators[((i + length(kAnnotators)) %% length(kAnnotators)) + 1] # 1-based indexing is awful
  sample.passage.tables[[i]] <- sample.passage.tables[[i]] |> 
    mutate(passage_number = i) |> 
    mutate(annotator = cur.annotator)
}
# bind everything into a table so we can get back to the parse data if necessary (rename if resampling)
extracted.passage.table <- bind_rows(sample.passage.tables)
write_csv(extracted.passage.table, "nondomestic_passage_extraction/2023-05-09_extracted-passages_parse.csv")
# drop first passages if more passages are needed
sample.passage.tables <- sample.passage.tables[751:1250]

# extract the text and glue it together into something human readable
extracted.passages <- lapply(sample.passage.tables, function(x) x["sentence"]) |> 
  lapply(function(x) distinct(x, sentence) |> pull(sentence)) |> 
  lapply(paste, collapse = " ") |> 
  unlist()
# grab passage number and annotator name from first row of each table
extracted.passage.numbers <- lapply(sample.passage.tables, function(x) pull(x, passage_number)[1]) |> unlist()
extracted.passages.annotators <- lapply(sample.passage.tables, function(x) pull(x, annotator)[1]) |> unlist()

extracted.passages.df <- data.frame(passage.number = extracted.passage.numbers,
                                    passage = extracted.passages,
                                    annotator = extracted.passages.annotators,
                                    nondomestic = character(500),
                                    notes = character(500)) |> tibble()
write_csv(extracted.passages.df, "nondomestic_passage_extraction/2023-05-09_nondomestic-passages_all.csv")

# write a file for each annotator
for (i in seq_along(kAnnotators)){
  cur.annotator = kAnnotators[i]
  cur.filename = paste0("nondomestic_passage_extraction/2023-05-09_nondomestic-passages_",
                    cur.annotator, ".csv")
  cur.passages <- extracted.passages.df |> 
    filter(annotator == cur.annotator)
  write_csv(cur.passages, cur.filename)
}


