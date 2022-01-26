# Carpel area import handtracing script. For comparison with machine learning data.

# Inputs: label data from via in csv format, ML csv data from carpel_area_analysis.R 
# Outputs: handtraced carpel area csv file, comparison ggplot graphs
# written by Amber, Sept 2021

####


# set up
library(tidyverse)
library(ggbeeswarm)
library(ggpubr)
library(Momocs)

setwd("") #set to directory with via csv files

# initalize an empty df to store prepped data
prepped_trace_data = data.frame()

# list each genotype handtraced validation set, csv is stored in a folder with the same name
genotype_list <- c("gt1-P", "gt1-P_zoom", "tb1gt1ra3", "gt1ra3", "mixed")

# ~~~ Loop over genotypes ~~~
for (index in genotype_list){
  print(paste0("Processing ", index))
  
  # import genotype specific validation handtracing from csv
  all_trace_data <- read_csv(paste0(index, "/via_region_data.csv"))
  all_trace_data <- rename(all_trace_data, filename = `#filename`)
  all_trace_data$genotype <- index
  
  # extract verticies from df
  vertex_data <- all_trace_data %>% select(region_shape_attributes, filename, genotype)
  vertex_data$region_shape_attributes <- as.character(vertex_data$region_shape_attributes)
  
  # get x verticies
  vertex_data$x_start <- str_locate(vertex_data$region_shape_attributes, "\\[") + 1
  vertex_data$x_end <- str_locate(vertex_data$region_shape_attributes, "\\]") - 1
  vertex_data$x_coords <- str_sub(vertex_data$region_shape_attributes, as.numeric(vertex_data$x_start[ , 1]), as.numeric(vertex_data$x_end[ , 1]))
  vertex_data$x_coords_new <- sapply(strsplit((vertex_data$x_coords), split=","), function(x) as.numeric(x))
  
  # get y verticies
  vertex_data$y_coords <- str_sub(vertex_data$region_shape_attributes, as.numeric(vertex_data$x_end[ , 1]) + 2, -1)
  vertex_data$y_start <- str_locate(vertex_data$y_coords, "\\[") + 1
  vertex_data$y_end <- str_locate(vertex_data$y_coords, "\\]") - 1
  vertex_data$y_coords <- str_sub(vertex_data$y_coords, as.numeric(vertex_data$y_start[ , 1]), as.numeric(vertex_data$y_end[ , 1]))
  vertex_data$y_coords_new <- sapply(strsplit((vertex_data$y_coords), split=","), function(x) as.numeric(x))
  
  #drop rows with NAs
  vertex_data <- vertex_data %>% drop_na(x_coords)
  
  # calculate area for vertex points using Momocs
  vertex_data$carpel_area <- NA
  for (x in 1:nrow(vertex_data)){
    #print(x) if there is a coo error check where by uncommenting this
    temp <- cbind(matrix(vertex_data$x_coords_new[[x]]), matrix(vertex_data$y_coords_new[[x]]))
    vertex_data$carpel_area[x] <- coo_area(temp)
  }
  
  # store calculated area in the prepped_trace_data df
  prepped_trace_data <- rbind(prepped_trace_data, vertex_data %>% select(filename, genotype, carpel_area))
  
}

#save the final handtraced validation df to a csv

write_csv(prepped_trace_data, "all_handtraced_area.csv")

#plot handtraced area by genotype

prepped_trace_data$genotype <- as.factor(prepped_trace_data$genotype)
prepped_trace_data %>% ggplot(aes(x=genotype,y=area, fill=genotype)) + geom_boxplot(outlier.shape = NA) + geom_quasirandom(size = .01)  + 
  labs(y = "handtraced carpel area (pixels squared, 1 pixel = 30.3 um)", title ="handtraced carpel area")


# ~~~ Comparing handtraced and ML area data ~~~

setwd("") #ML data directory
all_data <- read_csv("") #ML data


ML_data <- all_data %>% filter(genotype == c("B73", "gt1-P", "gt1ra3", "tb1gt1ra3"), treatment == "no_treatment") %>% select(genotype, carpel_area)
ML_data$type <- "ML"

prepped_trace_data$type <- 'handtraced'
prepped_trace_data <- rename(prepped_trace_data, "carpel_area" = area)
prepped_trace_data <- all_trace_data %>% filter(genotype == c("B73", "gt1-P", "gt1ra3", "tb1gt1ra3")) %>% select(genotype, "carpel_area")

all_data %>% select(genotype, planting, treatment, average_carpels_per_image) %>% ggplot(aes(x=genotype, y=average_carpels_per_image)) + geom_boxplot()
all_data %>% select(genotype, treatment, carpel_area) %>% ggplot(aes(x=genotype, y=carpel_area, fill=treatment)) + geom_boxplot(outlier.shape = NA) + geom_quasirandom(size = .01)
all_data %>% filter(genotype == "gt1ra3") %>% select(planting, treatment, carpel_area) %>% ggplot(aes(x=treatment, y=carpel_area, fill=treatment)) + geom_boxplot(outlier.shape = NA) + geom_quasirandom(size = .01) + facet_wrap(~planting)
all_data %>% filter(genotype == c("gt1-P", "gt1ra3", "tb1gt1ra3")) %>% select(genotype, treatment, carpel_area) %>% ggplot(aes(x=genotype, y=carpel_area, fill=genotype)) + geom_boxplot(outlier.shape = NA) + geom_quasirandom(size = .01)
all_data %>% filter(genotype == c("gt1-P", "gt1ra3", "tb1gt1ra3")) %>% 
  
subset1 <- filter(all_data, genotype == "gt1ra3") %>% select(carpel_area)
subset2 <- filter(all_data, genotype == "gt1-P") %>% select(carpel_area)
subset3 <- filter(all_data, genotype == "tb1gt1ra3") %>% select(carpel_area)

ReplicateAverages <- all_data_x %>% group_by(treatment, planting) %>% summarise(mean(carpel_area))
all_data_x %>% filter(genotype == "gt1ra3") %>% select(treatment, carpel_area, row_number,planting) %>% ggplot(aes(x=treatment, y=carpel_area)) + geom_boxplot(outlier.shape = NA) + geom_quasirandom(aes(color=planting),size = .25) + geom_beeswarm(data=ReplicateAverages, aes(color=planting), size=4) + stat_compare_means(data=ReplicateAverages, comparisons = list(c("defoliated", "no_treatment")), method="t.test", paired=TRUE)
t.test(all_data %>% filter(treatment == 'defoliated') %>% select(carpel_area), all_data%>% filter(treatment == "no_treatment") %>% select(carpel_area))
#p-value < 2.2e-16
all_data_x$treatment <- levels(all_data_x$treatment, c("no_treatment", "defoliated"))

t.test(subset2, subset1) #p-value < 2.2e-16
t.test(subset2, subset3) # p-value < 2.2e-16
t.test(subset1, subset2) # p-value < 2.2e-16

vertices %>% select(treatment, carpel_area) %>% ggplot(aes(x=treatment, y=carpel_area, fill=treatment)) + geom_boxplot() + geom_quasirandom(alpha=.2)
vertices %>% select(treatment, carpel_area, genotype) %>% ggplot(aes(x=treatment,y=carpel_area/30.14, fill=treatment)) + geom_boxplot(outlier.shape = NA) + geom_quasirandom(size = .05) + ylab("carpel area (um squared)")
t.test(late_gt1ra3_nt$carpel_area,late_gt1ra3_def$carpel_area) #gt1ra3 p-value early = 0.003408, p-value late = 0.07152
subset <- all_data %>% filter(genotype == "gt1ra3", planting == "late") %>% select(treatment, carpel_area)