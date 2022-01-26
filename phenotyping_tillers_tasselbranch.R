#Fall 2021 greenhouse phenotype analysis script, modified from 21UM_area_tillers_tbn_script.R
#Input: csv file with phenotype measurements
#Output: ggplot graphs
#12/13/2021
#written by Amber

library(tidyverse)
library(ggbeeswarm)
library(ggpubr)

setwd("")

all_data <- read_csv("", col_names=TRUE)

### ~ Normalize Tillers Function ~ ###
normalize_tillers <- function(h, tils) {
  #convert tillers (tils) from string to array of nums
  tils_vector <- str_split(tils, ";")
  tils_vector <- as.numeric(unlist(tils_vector))
  
  #divide each tiller by plant height (h)
  all_tils <- c()
  for (x in tils_vector){all_tils <- append(all_tils, x / h)}
  
  #add all tillers
  tiller_len_normalized <- sum(all_tils)
  
  return(tiller_len_normalized)
}

#normalize all tillers in csv
for(x in 1:nrow(all_data)){
  all_data$normalized_tiller_len[x] <- normalize_tillers(all_data$plant_height[x], all_data$all_tillers[x])
}

all_data$normalized_tiller_len[is.na(all_data$normalized_tiller_len)] <- 0

### ~ Make Graphs ~ ###

#graph normalized tiller length by genotype
all_data %>% filter(! is.na(normalized_tiller_len)) %>% ggplot(aes(x=genotype, y=normalized_tiller_len, color=genotype)) + 
  geom_boxplot() + geom_point(aes(color=genotype)) + stat_compare_means(method="t.test", cex=2.3)


#graph tassel branch number
all_data %>% ggplot(aes(x=genotype, y=tassel_branch_number, color=genotype)) + geom_boxplot() + 
  geom_point(aes(color=genotype),position=position_jitterdodge()) + stat_compare_means(method="t.test", cex=2.3)
