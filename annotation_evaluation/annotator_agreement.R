library(tidyverse)
library(irr)
library(ggpubr)
library(ggsci)

kAnnotationTable <- "annotations/1000-tags.csv"
kNonDomesticAnnotations <- "annotations/all_non-domestic.csv"

# annotation data wrangling
# =========================

# load the domestic annotation data; note that table contains meaningful NAs as strings
annotation_df <- read_csv(kAnnotationTable, na = "") |>
  distinct(UniqueID, Tagger, .keep_all = TRUE) |> 
  select(-Title, -Author, -Date, -MixedTag)

annotation_counts <- annotation_df |> 
  select(Tagger, SpaceTag) |> 
  group_by(Tagger) |> 
  count(SpaceTag) |> 
  pivot_wider(names_from = SpaceTag, values_from = n)

ggplot(annotation_df |> 
         filter(SpaceTag != "TRASH") |> 
         mutate(SpaceTag = case_when(SpaceTag == "T" ~ "Domestic",
                                     SpaceTag == "F" ~ "Nondomestic"))) +
  geom_bar(aes(x = Tagger, fill = SpaceTag), position = "dodge") + 
  scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_viridis_d(na.value = "grey") +
  labs(title = "Annotations by annotator") +
  ylab("Count") +
  theme_pubr() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank())

domestic_annotations <- annotations_wide |>
  mutate(agreement = sum(across(Alex:Matt), ~ . == "T"))
  

annotations_wide <- annotation_df |> 
  pivot_wider(names_from = Tagger, values_from = SpaceTag) |> 
  filter((!if_any(Alex:Matt, ~ .== "TRASH")) |> replace_na(TRUE)) # need to keep rows that return NA


# IRR calculations & visualization
# ================================
raw_annotations <- annotations_wide |> 
  select(Alex:Matt)

# convert to a scale that can be parsed as ordinal
# NB it's not clear the codes actually *are* ordinal
ordinal_annotations <- raw_annotations |> 
  mutate(across(everything(), ~ case_when(. == "T" ~ 1,
                                          . == "NA" ~ 2,
                                          . == "F" ~ 3)))

# overall statistics
irr::kripp.alpha(t(as.matrix(raw_annotations)))
# ordinality increases kappa relatively significantly
irr::kripp.alpha(t(as.matrix(ordinal_annotations)), method = "ordinal")

# pairwise comparison
annotators <- colnames(raw_annotations)
pairwise_annotations <- matrix(nrow = 5, ncol = 5)
colnames(pairwise_annotations) <- annotators
rownames(pairwise_annotations) <- annotators
for (i in seq_along(annotators)){
  annotator1 <- annotators[i]
  for (j in seq_along(annotators)){
    annotator2 <- annotators[j]
    if (i != j) {
      # two different annotators
      cur_annotations <- raw_annotations |> 
        select(annotator1, annotator2) |> 
        na.omit()
      cur_agreement <- irr::agree(cur_annotations)$value
      cur_kappa <- irr::kappa2(cur_annotations)$value
      cur_kripp <- irr::kripp.alpha(t(as.matrix(cur_annotations)))$value
    } else {
      # self-self comparison
      cur_kripp <- 1
    }
    pairwise_annotations[i,j] <- cur_kripp
  }
}
pheatmap(pairwise_annotations, main = "Pairwise Krippendorff's Kappa", 
         display_numbers = TRUE,
         border_color = "black", fontsize = 16,
         angle_col = 45, treeheight_row = 30, treeheight_col = 30)
