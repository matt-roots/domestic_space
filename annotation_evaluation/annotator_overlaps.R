library(tidyverse)
library(VennDiagram)
library(ggpubr)
library(ggsci)
library(ComplexHeatmap)

# Constants and helper functions
# ===============================================

kTagsPath <- "annotations/1000-tags.csv"
kTaggers <- c("Alex",
              "Svenja",
              "Matt",
              "Jessica",
              "Mark")

# calculate jaccard distance between two term lists
jaccard.term.map <- function(a, b){
  a <- na.omit(a)
  b <- na.omit(b)
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection / union)
}

# ===============================================


# load and filter any duplicates, remove trash
tags.df <- read_csv(file = kTagsPath, na = "",) |> 
  distinct(UniqueID, Tagger, .keep_all = TRUE)

getTaggerDistance <- function(tags.df){
  # create unique labels for each passage tag and pivot wide
  domestic.df <- tags.df |> 
    unite(col = SpaceTag, UniqueID, SpaceTag, remove = FALSE) |> 
    unite(col = MixedTag, UniqueID, MixedTag, remove = FALSE) |> 
    select(UniqueID, Tagger, SpaceTag, MixedTag) |> 
    pivot_wider(names_from = "Tagger", values_from = c("SpaceTag", "MixedTag"))

  # extract just the tags
  domestic.tags <- domestic.df |> 
    select(SpaceTag_Alex:SpaceTag_Matt)
  
  # make a distance matrix
  dist.matrix <- sapply(domestic.tags, function(x) sapply(domestic.tags, function(y) jaccard.term.map(x, y)))
  rownames(dist.matrix) <- kTaggers
  colnames(dist.matrix) <- kTaggers
  
  return(dist.matrix)
}

domestic.tag.distance <- getTaggerDistance(tags.df)
pheatmap(domestic.tag.distance)

domestic.tag.distance.simple <- getTaggerDistance(tags.df |> mutate(SpaceTag = ifelse(test = SpaceTag == "NA",
                                                                                      yes = "F",
                                                                                      no = SpaceTag)))
pheatmap(domestic.tag.distance.simple)


# collapse NA and F tags
domestic.df.simple <- domestic.df <- tags.df |> 
  mutate(SpaceTag = ifelse(test = SpaceTag == "NA",
                           yes = "F",
                           no = SpaceTag)) |> 
  unite(col = SpaceTag, UniqueID, SpaceTag, remove = FALSE) |> 
  unite(col = MixedTag, UniqueID, MixedTag, remove = FALSE) |> 
  select(UniqueID, Tagger, SpaceTag, MixedTag) |> 
  pivot_wider(names_from = "Tagger", values_from = c("SpaceTag", "MixedTag"))

domestic.tags.simple






# mutate values into the passage number
domestic.df <- tags.df |> 
  mutate(SpaceTag = ifelse(test = SpaceTag == "T",
                           yes = UniqueID,
                           no = NA))

domestic.wide <- domestic.df |> 
  select(UniqueID, Tagger, SpaceTag, MixedTag) |> 
  pivot_wider(names_from = "Tagger", values_from = c("SpaceTag", "MixedTag"))

categories <- list(domestic.wide$SpaceTag_Alex,
                domestic.wide$SpaceTag_Svenja,
                domestic.wide$SpaceTag_Matt,
                domestic.wide$SpaceTag_Jessica,
                domestic.wide$SpaceTag_Mark)

palette = get_palette("jco", 5)
venn.diagram(
  x = categories,
  na = "remove",
  category.names =  kTaggers,
  output=FALSE,
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = palette,
  
  # Numbers
  cex = 1.5,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 1.5,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.fontfamily = "sans",
  
  #output
  filename = "annotation_evaluation/annotation_plots/domestic_overlaps.svg",
  imagetype = "svg",
  units = "inches",
  height = 12,
  width = 12,
  margin = 0.35
)
