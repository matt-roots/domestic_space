library(tidyverse)
library(glmnet)
library(text2vec)
library(ggpubr)
library(ggsci)

kNumFolds <- 10 # for cross-validation
kJaneEyrePath <- "../FullChadwyckBritishParse/349.csv"
kAnnotationTable <- "annotations/1000-tags.csv"
kNonDomesticAnnotations <- "annotations/all_non-domestic.csv"

kRegenerateModel <- FALSE
kTrainingPassagesPath <- "domestic_regression-model/training-passages.rds"
kModelPath <- "domestic_regression-model/regression-model.rds"

# annotation data wrangling
# =========================

# load the domestic annotation data; note that table contains meaningful NAs
annotation.df <- read_csv(kAnnotationTable, na = "") |>
  distinct(UniqueID, Tagger, .keep_all = TRUE) |> 
  select(-Title, -Author, -Date, -MixedTag) |> 
  pivot_wider(names_from = Tagger, values_from = SpaceTag) |> 
  filter((!if_any(Alex:Matt, ~ .== "TRASH")) |> replace_na(TRUE)) |> 
  mutate(Filename = str_replace(Filename, pattern = "\\.txt$", ".csv")) |> 
  mutate(Filename = paste0("../", Filename))

# compute agreement for each passage
getMajority <- function(...){
  annotations <- na.omit(c(...))
  num.true <- sum(annotations == "T")
  num.false <- sum(annotations == "F")
  num.NA <- sum(annotations == "NA")
  # passage type-annotation is majority rule, or ambiguous if no majority
  return(case_when(num.true > num.false + num.NA ~ "domestic",
                   num.false > num.true + num.NA ~ "nondomestic",
                   TRUE ~ "ambiguous"))
}

# collapse to table of passage info and categorical label
annotations <- annotation.df |> 
  rowwise() |> 
  mutate(majority_tag = getMajority(c_across(Alex:Matt))) |> 
  select(-(Alex:Matt)) |> 
  ungroup()

# import nondomestic space annotations
nondomestic.annotations <- read_csv(kNonDomesticAnnotations) |> 
  filter(validated == TRUE) |> 
  pull(passage.number)
# parse out the nondomestic passages
nondomestic.passages <- read_csv("nondomestic_passage_extraction/2023-05-09_extracted-passages_parse.csv") |> 
  filter(passage_number %in% nondomestic.annotations) |> 
  select(sentence_id, sentence, filename, passage_number) |> 
  distinct(sentence, .keep_all = TRUE) |> 
  group_by(passage_number) |> 
  mutate(TextSegment = paste0(sentence, collapse = " ")) |> 
  distinct(passage_number, .keep_all = TRUE) |> 
  rename(c(SentenceNumber = sentence_id,
           Filename = filename)) |> 
  mutate(UniqueID = -1 * passage_number) |> 
  mutate(majority_tag = "nondomestic") |> 
  select(-sentence)

# augment domestic annotations with nondomestic passages
annotations <- full_join(annotations, nondomestic.passages)


# load feature tables
# ===================

# import corresponding passages from corpus-parse
# cache to rds because of slow file-reading
if (file.exists(kTrainingPassagesPath)){
  all.passages <- read_rds(kTrainingPassagesPath)
} else {
  # helper function to get passage data
  getNovelPassage <- function(novel.path, sentence.number){
    start.sentence <- sentence.number - 1
    end.sentence <- sentence.number + 4
    # the data have been mangled with unlabeled rownames
    suppressMessages(novel.parse <- read_csv(novel.path, show_col_types = FALSE, col_select = -1))
    passage.parse <- novel.parse |> 
      filter(sentence_id >= start.sentence & sentence_id <= end.sentence)
    return(passage.parse)
  }
  all.passages <- lapply(seq(nrow(annotations)),
                         function (i) getNovelPassage(annotations$Filename[i],
                                                      annotations$SentenceNumber[i]))
  names(all.passages) <- annotations$UniqueID
  write_rds(all.passages, kTrainingPassagesPath)
}


# extract features
# ----------------

# for compatibility below this should return a character vector where each element is a single feature
getFeatures <- function(passage.df){
  features <- passage.df$lemma
  return(features)
}

all.features <- lapply(all.passages, getFeatures)

# Build and train model
# ===============================

# for now, filter features and annotations to just binary classifications
binary.annotations <- annotations |> 
  filter(majority_tag != "ambiguous")
binary.features <- all.features[annotations$majority_tag != "ambiguous"]

# build DTM of features for each passage
# feature.list should be a named list
buildDTM <- function(feature.list) {
  feature.iterator <- itoken(feature.list,
                             ids = names(feature.list))
  feature.vocab <-  create_vocabulary(feature.iterator)
  feature.vectorizer <- vocab_vectorizer(feature.vocab)
  feature.dtm <- create_dtm(feature.iterator, feature.vectorizer)
  feature.dtm <- feature.dtm[,Matrix::colSums(feature.dtm) > 5]
}
feature.dtm <- buildDTM(binary.features)

# optionally, add non-count features to the DTM feature table now
# features should be numeric, columnar and exist for all documents

# divide passages in 80-20 training / test
set.seed(42)
train.length <- floor(length(binary.features) * 0.8)
train.indices <- sample(seq_along(binary.features), train.length)
# get true class labels for each element in test and train
train.classes <- as_factor(binary.annotations$majority_tag[train.indices])
test.classes <- as_factor(binary.annotations$majority_tag[-train.indices])
# divide 
train.features <- feature.dtm[train.indices,]
test.features <- feature.dtm[-train.indices,]

# TODO prune sparse terms

# by default just regenerate the model, otherwise read from file if possible
if (kRegenerateModel | !file.exists(kModelPath)) {
  cv.classifier <- cv.glmnet(x = train.features, y = train.classes,
                             family = "binomial",
                             alpha = 1, nfolds = kNumFolds)
  write_rds(cv.classifier, kModelPath)
} else {
  cv.classifier <- read_rds(kModelPath)
}


# extract the model coefficients
coef.1se <- as.data.frame(as.matrix(coef(cv.classifier, s = "lambda.1se"))) %>% filter(s1 != 0)

# plot the training data classification
train.fit <- predict(cv.classifier, newx = train.features, type = "response", s = "lambda.1se")
train.fit <- data.frame(unique_id = rownames(train.fit), 
                    classification = train.fit[,1], 
                    true_class = train.classes) |> 
              arrange(classification) |> 
              mutate(predicted_class = ifelse(test = classification > 0.5,
                                              yes = "domestic",
                                              no = "nondomestic"))
train.accuracy <- sum(train.fit$true_class == train.fit$predicted_class) / nrow(train.fit) 
train.accuracy <- sprintf("%1.1f%%", 100 * train.accuracy)

train.p <- ggplot(data = train.fit) +
  geom_bar(aes(x = true_class, fill = predicted_class)) +
  ggplot2::annotate(geom = "text", x = 1, y = 260, label = paste0("Accuracy = ", train.accuracy)) +
  scale_fill_jco() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Classification Accuracy",
       subtitle = "(Training Data)") +
  ylab("Count") +
  xlab("True Class") +
  theme_pubr()

# predict the test data
test.fit <- predict(cv.classifier, newx = test.features, type = "response", s = "lambda.1se")
test.fit <- data.frame(unique_id = rownames(test.fit), 
                        classification = test.fit[,1], 
                        true_class = test.classes) |> 
  arrange(classification) |> 
  mutate(predicted_class = ifelse(test = classification > 0.5,
                                  yes = "domestic",
                                  no = "nondomestic"))
test.accuracy <- sum(test.fit$true_class == test.fit$predicted_class) / nrow(test.fit) 
test.accuracy <- sprintf("%1.1f%%", 100 * test.accuracy)

test.p <- ggplot(data = test.fit) +
  geom_bar(aes(x = true_class, fill = predicted_class)) +
  ggplot2::annotate(geom = "text", x = 2, y = 65, label = paste0("Accuracy = ", test.accuracy)) + 
  scale_fill_jco() +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Classification Accuracy",
       subtitle = "(Test Data)") +
  ylab("Count") +
  xlab("True Class") +
  theme_pubr()
model.prediction.p <- ggarrange(train.p, test.p, 
                                common.legend = TRUE, 
                                legend = "bottom")
ggsave("domestic_regression-model/regression_plots/jane-eyre_model-classification.png", width = 12, height = 6, units = "in")


# profit? with unseen data
# ========================

# breaks the novel down into consecutive 6-sentence chunks
getNovelPassages <- function(novel.path, passage.length = 6){
  suppressMessages(novel.parse <- read_csv(novel.path, show_col_types = FALSE, col_select = -1))
  novel.sentence.length <- max(novel.parse$sentence_id)
  target.sentences <- seq(1, novel.sentence.length - passage.length, passage.length)
  novel.passages <- lapply(target.sentences, function (x) novel.parse |> 
                             filter(sentence_id >= x & sentence_id <= x + (passage.length - 1)))
  novel.features <- lapply(novel.passages, getFeatures)
  names(novel.features) <- as.character(target.sentences)
  return <- novel.features
}

# gets predictions from a novel DTM. Note that this requires imputing empty features
# and removing features not included in the model
getNovelPredictions <- function(model, novel.DTM, train.DTM){
  all.beta <- rownames(cv.classifier$glmnet.fit$beta)
  selected.features <- novel.DTM[,colnames(novel.DTM) %in% colnames(train.DTM)]
  missing.features <- colnames(train.DTM)[!colnames(train.DTM) %in% colnames(novel.DTM)]
  missing.features.matrix <- Matrix(data = 0,
                                    nrow = nrow(selected.features),
                                    ncol = length(missing.features),
                                    doDiag = TRUE,
                                    sparse = TRUE)
  colnames(missing.features.matrix) <- missing.features
  full.novel.features <- cbind(selected.features, missing.features.matrix)
  novel.predict <- predict(cv.classifier, newx = full.novel.features, 
                          type = "response", s = "lambda.1se")
  novel.predict <- data.frame(sentence_number = rownames(novel.predict),
                             probability = novel.predict[,1])
  return(novel.predict)
}

# make example predictions using short and long chunks
# ====================================================

# note this is a little slow when generating a large number of passages
jane.short.features <- getNovelPassages(kJaneEyrePath, passage.length = 6)
jane.short.DTM <- buildDTM(jane.short.features)
jane.short.predict <- getNovelPredictions(cv.classifier, jane.short.DTM, feature.dtm)

jane.long.features <- getNovelPassages(kJaneEyrePath, passage.length = 250)
jane.long.DTM <- buildDTM(jane.long.features)
jane.long.predict <- getNovelPredictions(cv.classifier, jane.long.DTM, feature.dtm)

# plotting
# ========

coef.df <- coef.1se |> 
  rownames_to_column("coef") |> 
  arrange(desc(abs(s1))) |> 
  mutate(intercept = ifelse(coef == "(Intercept)",
                            yes = TRUE, no = FALSE)) |> 
  arrange(-intercept) |> 
  mutate(coef = as_factor(coef))

# model coefficients
ggplot(coef.df) + 
  geom_col(aes(y = s1, x = coef, fill = intercept)) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_fill_jco() +
  labs(title = "Regression model features") +
  xlab("Feature") +
  ylab("Magnitude") +
  theme_pubr() + 
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        legend.position = "none",
        plot.margin = margin(10,10,10,10))
ggsave("domestic_regression-model/regression_plots/model_coefficients.png", width = 8, height = 4, units = "in")


#short chunks
ggplot(jane.short.predict, aes(x = as.numeric(sentence_number), y = probability)) +
  geom_point(color = "navyblue") +
  geom_smooth(method = "loess", span = 0.1, colour = "cornflowerblue") +
  scale_x_continuous(expand = c(0,0)) + 
  labs(title = "",
       subtitle = "(6-sentence slices)") +
  xlab("Sentence number (start of slice)") +
  ylab("Probability slice is in domestic space") +
  theme_pubr() + 
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 20))
ggsave("domestic_regression-model/regression_plots/jane-eyre_6-sentence.png", width = 10, height = 4, units = "in")

# long chunks
ggplot(jane.long.predict, aes(x = as.numeric(sentence_number), y = probability)) +
  geom_point(color = "navyblue") +
  geom_smooth(method = "loess", span = 0.1, colour = "cornflowerblue") +
  scale_x_continuous(expand = c(0,0)) + 
  labs(title = "",
       subtitle = "(250-sentence slices)") +
  xlab("Sentence number (start of slice)") +
  ylab("Probability slice is in domestic space") +
  theme_pubr() + 
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 20))
ggsave("domestic_regression-model/regression_plots/jane-eyre_250-sentence.png", width = 10, height = 4, units = "in")
