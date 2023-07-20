# Unfiltered passage extraction for baseline evaluation of novel passage locations


library(tidyverse)

# constants
# =========
# paths and files
kCorpusPath <- "../FullChadwyckBritishParse/" # corpus directory
kCorpusFiles <- list.files(kCorpusPath, full.names = TRUE)
# annotators names for dividing up the table
kAnnotators <- c("matt", "mark", "alex", "jessica", "svenja", "sarah", "natasha", "julia")


# passage sampling
# ================

# sample n passages at random
if (file.exists("unfiltered_passage_extraction/2023-07-06_unfiltered-passages_full.rds")){
  all.passages <- read_rds("unfiltered_passage_extraction/2023-07-06_unfiltered-passages_full.rds")
} else {
  print("Unfiltered passages not found. Generating now...")
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
    set.seed(i + 1) # + 1 so we don't sample identically to the nondomestic passages
    cur.starts <- sample(1:(cur.novel.length - 6), 20)
    # slice out just the windows around the sampled sentences
    cur.passages <- lapply(cur.starts, function(x) cur.novel.parse[which(cur.novel.parse$sentence_id >= x & 
                                                                           cur.novel.parse$sentence_id <= x + 6),])
    all.passages[[i]] <- cur.passages
  }
  # write to file
  write_rds(all.passages, "unfiltered_passage_extraction/2023-07-06_unfiltered-passages_full.rds")
}
# bind all the individual tables together
# full.passage.table <- bind_rows(all.passages)
# write_csv(full.passage.table, "")

# create a sampled subset instead for easier evaluation
# ======
all.passage.tables <- unlist(all.passages, recursive = FALSE)
# as a sentence-per-row table
set.seed(42)
# to sample another set of passages, increase the number sampled and drop the first n passages
sample.passage.tables <- sample(all.passage.tables, 40)
# create an index to track which passage is which
for (i in seq_along(sample.passage.tables)){
    sample.passage.tables[[i]] <- sample.passage.tables[[i]] |> 
    mutate(passage_number = i)
}
# bind everything into a table so we can get back to the parse data if necessary (rename if resampling)
extracted.passage.table <- bind_rows(sample.passage.tables)
write_csv(extracted.passage.table, "unfiltered_passage_extraction/2023-07-06_unfiltered-passages_parse.csv")
# drop first passages if more passages are needed
# sample.passage.tables <- sample.passage.tables[1:320]

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
                                    annotation = character(40),
                                    notes = character(40)) |> tibble()
write_csv(extracted.passages.df, "unfiltered_passage_extraction/2023-07-06_unfiltered-passages_all.csv")

# write a file for each annotator
for (i in seq_along(kAnnotators)){
  cur.annotator = kAnnotators[i]
  cur.filename = paste0("unfiltered_passage_extraction/2023-07-06_unfiltered-passages_",
                        cur.annotator, ".csv")
  cur.passages <- extracted.passages.df |> 
    mutate(annotator = rep(cur.annotator, nrow(cur.passages)), .before = annotation)
  write_csv(cur.passages, cur.filename)
}
