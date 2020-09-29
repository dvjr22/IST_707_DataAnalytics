scorecard <- read.csv("scorecard-3.csv", stringsAsFactors = FALSE)

# There are 2 "emmanuel colleges", so I added theie states to them so that we can use name as a unique identifier
scorecard$name.id <- ifelse(scorecard$name == "Emmanuel College" & scorecard$state=="GA", "Emmanuel College (GA)", 
                         ifelse(scorecard$name == "Emmanuel College" & scorecard$state == "MA", "Emmanuel College (MA)", scorecard$name))

# read in cluster assignments
clusters <- read.csv("during.clusters.csv")

# join to scorecard dataset
library(dplyr)
scorecard <- scorecard %>%
  left_join(clusters, by = c("name.id"="name"))
str(scorecard)
scorecard$during.cluster <- factor(scorecard$during.cluster)


# if you want to remove the name.id column:
scorecard <- scorecard %>% select(-name.id)
str(scorecard)
