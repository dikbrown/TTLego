library(dplyr)

#Designate node as central (not a child of any node) or not
themes$center <- F
themes$center[which(is.na(themes$parent_id))] <- T

head(themes)

node_sizes <- themes %>%
                group_by(parent_id) %>%
                summarize(size = n())

