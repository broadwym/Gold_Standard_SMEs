# Read in question_assessment.csv 
# Read in libraries 
library(dplyr)
library(tidyr)
library(ggplot2)

# De-duplicate(d) [in Excel] based on question_id b/c that's the unique identifier 
# Idea: Narrow down scope to ~200 questions, not 400 per SME 

## Count number of assessments in total: 61
## With number of questions per assessment 
total_assessments <- question_assessment %>%
  group_by(KnowledgeAssessmentName) %>%
  count() 

ggplot(total_assessments, aes(x=reorder(KnowledgeAssessmentName, -n), y = n)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("Knowledge Assessment") # data viz

## Only ~4 randomly chosen questions should come from each assessment. We don't necessarily need to worry about digging deep into randomization (re: frequency/infrequency of tags) 
## First, get a random sample of 3-4 questions from each assessment
## Then, randomly assign to three groups (SMEs)
new_df <- question_assessment %>% group_by(KnowledgeAssessmentName) %>% sample_n(size = 4) # 4 questions per assessment (~200 questions total)
new_df2 <- question_assessment %>% group_by(KnowledgeAssessmentName) %>% sample_n(size = 7) # 7 questions per assessment (~400 questions total)

# Split data into as even as possible parts (82 + 81 + 81 = 244 assessments per SME):
## Generate folds as a list of indices
folds <- split(sample(nrow(new_df), nrow(new_df),replace=FALSE), as.factor(1:3)) # without replacement

# Tease out folds into individual dfs
fold_1 <- new_df[folds[[1]], ]
fold_2 <- new_df[folds[[2]], ]
fold_3 <- new_df[folds[[3]], ]

## Combine fold 1 and fold 2; fold 3 will tiebreak
## Combine fold 2 and fold 3; fold 1 will tiebreak
## Combine fold 3 and fold 1; fold 2 will tiebreak 
combined_fold_1 <- rbind(fold_1, fold_2) 
combined_fold_2 <- rbind(fold_2, fold_3)
combined_fold_3 <- rbind(fold_3, fold_1)


# Export folds as .csv
write.csv(combined_fold_1, "~/Desktop/SME_1.csv")
write.csv(combined_fold_2, "~/Desktop/SME_2.csv")
write.csv(combined_fold_3, "~/Desktop/SME_3.csv")

