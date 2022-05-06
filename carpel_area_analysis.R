# Carpel Area Calculations. Input from Mask-RCNN .csv files, output ggplot graphs.
# Amber 9/9/21, comments added 10/19/21, 5/6/22

# set up libraries

library(tidyverse)
library(ggbeeswarm)
library(ggpubr)

setwd("")

# ~~~~~~~~~~~~~~~~~ Import data from Mask-RCNN generated carpel area .csv files while adding columns with genotype/treatment information ~~~~~~~~~~~~~~~~~~~~~~~~



# Step 1: Create empty data frame to store all data. *Only do this at the beginning.*
all_data <- data.frame()
x <- data.frame()
y <- data.frame()

# Step 2: Start importing from csv files. Each csv file should have measurements from only one genotype, either control or treatment, and planting time.
#   Combinations of genotypes, treatments, and timings in one csv file will not be imported correctly.
#   However, if you have multiple csvs for the same condition, for example- measurements for one genotype/one treatment/one planting split over 3 csvs, that is fine. Just import them one by one.
#   If you are just comparing genotypes, leave the extra code below commented out.
#   If you are comparing genotype by treatment by planting time, uncomment the extra lines. Check that your csv imports are correct. (2 genotypes * 2 treatment conditions * 2 planting times = minimum 6 separate csv files)

subset_genotype <- "ra3_N" #set genotype name, for example A10 or gt1

x <- read_csv("carpel_area_results_ra3_N.csv", col_names=TRUE) # set genotype, control (no treatment) csv import file name. Example: carpel_area_late_gt1ra3_no_treatment.csv
#y <- read_csv("", col_names=TRUE) # set genotype, treatment csv import file name. Example: carpel_area_late_gt1ra3_defoliated.csv

x$genotype <- subset_genotype #make a new column with genotype
#y$genotype <- subset_genotype

x$treatment <- "no_treatment" #make a new column for treatment
#y$treatment <- "defoliated"


# Step 3: early or late planting? skip if only 1 planting

#~~~~ if early run this chunk ~~~~
#subset_genotype_early_nt <- str_c("early_", subset_genotype, "_nt")
#subset_genotype_early_def <- str_c("early_", subset_genotype, "_def")

#x$planting <- "early"
#y$planting <- "early"

#assign(subset_genotype_early_nt, x) #make fresh df to store
#assign(subset_genotype_early_def, y) #make fresh df to store

#~~~~ if late run this chunk ~~~~
#subset_genotype_late_nt <- str_c("late_", subset_genotype, "_nt")
#subset_genotype_late_def <- str_c("late_", subset_genotype, "_def")

#x$planting <- "late"
#y$planting <- "late"

#assign(subset_genotype_late_nt, x) #make fresh df to store
#assign(subset_genotype_late_def, y) #make fresh df to store

#Step 3.5, optional: Calculate the average number of carpels detected per image for each set and add it to the dataframe

#x$image_total <- 178 #no treatment image total - change this number depending on your dataset
#y$image_total <- 220 #treatment image total - change this number depending on your dataset
#x$total_carpels <- nrow(x)
#y$total_carpels <- nrow(y)
#x$average_carpels_per_image <- x$total_carpels / x$image_total
#y$average_carpels_per_image <- y$total_carpels / y$image_total

# Step 4: Combine new data frames with old one

all_data <- rbind(all_data, x, y)

# Step 5: Repeat from *Step 2* with the next genotype or next csv

# Step 6: Once all csvs are imported, save your completed data frame as a new csv

write_csv(all_data, "all_carpel_area_data.csv")




# ~~~~~~~~~~~~~~~~~ Generating ggplot graphs from ML carpel area data ~~~~~~~~~~~~~~~~~~~~~~~~


x <- read_csv("all_carpel_area_data.csv") #read in the previously generated csv from Step 6

# ~~~ Generating scan y-Position number, angle position, and plant number for each carpel ~~~

# Step 1: cleaning image_ids so all formats match. There should be 5 characters before the "~".
#x$image_id <- str_replace(x$image_id, "p_test", "") #for maize 2021 dataset
#x$image_id <- str_replace(x$image_id, "_tassel", "") #for maize 2021 dataset
x$image_id <- str_replace(x$image_id, "ra3_ds", "r3D") #for setaria 2022 dataset
x$image_id <- str_replace(x$image_id, "ra3_N", "r3N") #for setaria 2022 dataset
x$image_id <- str_replace(x$image_id, "ra3_normal", "r3N") #for setaria 2022 dataset
x$image_id <- str_replace(x$image_id, "ra3_n", "r3N") #for setaria 2022 dataset
x$image_id <- str_replace(x$image_id, "ra3", "r3N") #for setaria 2022 dataset

# Step 2: find string positions in image_id that correspond to scan Y-position, scan angle position, and plant number. Save values in new columns 'y-pos', 'angle', and 'plant_num'.
x$y_pos_start <- str_locate(x$image_id, "~") # set position 0 at "~"
x$y_pos_start <- x$y_pos_start[ , 1] # only keep start position id
x$y_pos <- str_sub(x$image_id, x$y_pos_start + 1, x$y_pos_start + 2) # grab the 2 numbers to the right of the "~"
print(as.factor(x$y_pos)) # do levels look correct? should be 00 01 02 etc
x$y_pos <- as.numeric(x$y_pos) # convert to numeric for graphing

x$angle <- as.factor((str_sub(x$image_id, x$y_pos_start + 9 , x$y_pos_start + 10)))
print(x$angle) # do levels look correct? For my data I have images from angles 08 and 10 (2 levels).

x$plant_num <- str_sub(x$image_id, 1, 6) #plant number should be extracted in the format 'XXX-XX', row number dash plant number
x$plant_num <- as.factor(str_replace(x$plant_num, "~", "")) #remove ~ for single digit plant numbers and convert to factor for graphing
print(x$plant_num) #do plant numbers look correct? both single and double digits
print(levels(x$plant_num))

#x$rownumb <- as.factor(str_sub(x$plant_num, 0, 3)) #this only works if the row number is included in csv name
#print(x$rownumb)

# Step 3: add ggplot functions (using ggpubr for this now)

#function to add counts and mean above boxplot from https://gscheithauer.medium.com/how-to-add-number-of-observations-to-a-ggplot2-boxplot-b22710f7ef80
#stat_box_data <- function(y, upper_limit = max(x$carpel_area) * 1.15) {
#  return( 
#    data.frame(
#      y = 0.95 * upper_limit,
#      label = paste('count =', length(y), '\n',
#                    'mean =', round(mean(y), 1), '\n')
#    )
#  )
#}

# Step 4: ggplot graphs

#guide:
# 1. view all genotype data points
# 2. all genotype data points split by treatment
# 3. single genotype split by treatment and faceted by planting
# 4. single genotype split by treatment and faceted by row number
# 5. single genotype single rows split by treatment and faceted by image angle
# 6. all genotypes no treatment
# 7. all genotypes no treatment faceted by angle
# 8. single genotype filtered for only quality image data, comparing treatments
# 9. single genotype filtered for only quality image data, comparing treatments by row number
# 10. single genotype filtered for only quality image data, single rows split by treatment and faceted by image angle
# 11. graphing handtraced vs all ML data


# 1. view all genotype data points
x %>% ggplot(aes(x = genotype, y = carpel_area, color = genotype)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = 1, alpha=.35, width = .7) + 
  #stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  labs(y = "carpel area (pixels squared, 1 pixel = x um)") # for maize images, 1 pixel = 30.3 um

ggsave("all_ML_carpel_area_data.jpg", width = 17, height = 10, units = "in")
ggsave("all_ML_carpel_area_data.pdf", width = 17, height = 10, units = "in")


# 2. all genotype data points split by treatment (gt1ra3 and gt1 are the only genotypes I have both kinds of data for)

x$treatment <- factor(x$treatment, levels = c("no_treatment", "defoliated")) #no_treatment on the left

x %>% filter(genotype == "gt1ra3" | genotype == "gt1") %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~genotype) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

ggsave("all_treatments_compared_ML_carpel_area_data.jpg", width = 8, height = 7, units = "in")
ggsave("all_treatments_compared_ML_carpel_area_data.pdf", width = 8, height = 7, units = "in")

# 3. just gt1ra3 split by treatment and faceted by planting

x %>% filter(genotype == "gt1ra3") %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~planting) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

ggsave("gt1ra3_treatments_planting_compared_ML_carpel_area_data.jpg", width = 8, height = 7, units = "in")
ggsave("gt1ra3_treatments_planting_compared_ML_carpel_area_data.pdf", width = 8, height = 7, units = "in")

# 4. just gt1ra3 split by treatment and faceted by row number

x %>% filter(genotype == "gt1ra3") %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~rownumb) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

ggsave("gt1ra3_treatments_rownumber_ML_carpel_area_data.jpg", height = 12, width = 10, units = "in")
ggsave("gt1ra3_treatments_rownumber_ML_carpel_area_data.pdf", height = 12, width = 10, units = "in")

# 5. gt1ra3 single rows split by treatment and faceted by image angle

gg154 <- x %>% filter(rownumb == "154") %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~angle) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

gg158 <- x %>% filter(rownumb == "158") %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~angle) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

gg405 <- x %>% filter(rownumb == "405") %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~angle) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

gg409 <- x %>% filter(rownumb == "409") %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~angle) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

#combine all plots into one
ggarrange(gg154, gg158, gg405, gg409, labels = c("154", "158", "405", "409"), ncol = 2, nrow = 2)

ggsave("gt1ra3_treatments_angle_rownumber_ML_carpel_area_data.jpg", height = 12, width = 16, units = "in")
ggsave("gt1ra3_treatments_angle_rownumber_ML_carpel_area_data.pdf", height = 12, width = 16, units = "in")

# 6. just gt1-P, gt1ra3, and tb1gt1ra3 no treatment

x %>% filter(genotype == "gt1-P" | genotype == "gt1ra3" | genotype == "tb1gt1ra3") %>% filter(treatment == "no_treatment") %>% ggplot(aes(x = genotype, y = carpel_area, color = genotype)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

#all possible paired t-tests are significant
#subset1 <- filter(x, genotype == "gt1ra3") %>% select(carpel_area)
#subset2 <- filter(x, genotype == "gt1-P") %>% select(carpel_area)
#subset3 <- filter(x, genotype == "tb1gt1ra3") %>% select(carpel_area)
#t.test(subset2, subset1) #p-value < 2.2e-16
#t.test(subset2, subset3) # p-value < 2.2e-16
#t.test(subset1, subset2) # p-value < 2.2e-16

ggsave("gt1P_gt1ra3_triple_no_treatment_ML_carpel_area_data.jpg", height = 7, width = 6, units = "in")
ggsave("gt1P_gt1ra3_triple_no_treatment_ML_carpel_area_data.pdf", height = 7, width = 6, units = "in")

# 7. just gt1-P, gt1ra3, and tb1gt1ra3 no treatment faceted by angle

x %>% filter(treatment == "no_treatment") %>% ggplot(aes(x = angle, y = carpel_area, color = angle)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = 1, alpha=.35, width = .4) + 
  #stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  #stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)") +
  facet_wrap(~genotype)

# I think angle 10 is a little bit closer to the branch compared to angle 8
ggsave("gt1P_gt1ra3_triple_angle_no_treatment_ML_carpel_area_data.jpg", height = 7, width = 9, units = "in")
ggsave("gt1P_gt1ra3_triple_angle_no_treatment_ML_carpel_area_data.pdf", height = 7, width = 9, units = "in")

# 8. gt1ra3 filtered plant rows for only quality image data, comparing treatments

good_gt1ra3_images <- c("154-2", "154-4", "154-6", "154-7", "154-8", "158-7", "158-10", 
                        "405-2", "405-8", "409-10", "409-2", "409-4", "409-6", "409-8",
                        "405-1", "405-7", "405-9", "409-1", "409-3", "409-5")
                        
x %>% filter(plant_num %in% good_gt1ra3_images) %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~planting) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")


ggsave("good_gt1ra3_treatments_ML_carpel_area_data.jpg", height = 8, width = 7, units = "in")
ggsave("good_gt1ra3_treatments_ML_carpel_area_data.pdf", height = 8, width = 7, units = "in")

# 9. gt1ra3 filtered plant rows for only quality image data, comparing treatments by row number

x %>% filter(plant_num %in% good_gt1ra3_images) %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~rownumb) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

ggsave("good_gt1ra3_treatments_rownumber_ML_carpel_area_data.jpg", height = 7, width = 7, units = "in")
ggsave("good_gt1ra3_treatments_rownumber_ML_carpel_area_data.pdf", height = 7, width = 7, units = "in")


# 10. gt1ra3 filtered plant rows for only quality image data, gt1ra3 single rows split by treatment and faceted by image angle

gg154 <- x %>% filter(plant_num %in% good_gt1ra3_images) %>% filter(rownumb == "154") %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~angle) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

gg158 <- x %>% filter(plant_num %in% good_gt1ra3_images) %>% filter(rownumb == "158") %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~angle) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

gg405 <- x %>% filter(plant_num %in% good_gt1ra3_images) %>% filter(rownumb == "405") %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~angle) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

gg409 <- x %>% filter(plant_num %in% good_gt1ra3_images) %>% filter(rownumb == "409") %>% ggplot(aes(x = treatment, y = carpel_area, color = treatment)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  facet_wrap(~angle) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)")

#combine all plots into one
ggarrange(gg154, gg158, gg405, gg409, labels = c("154", "158", "405", "409"), ncol = 2, nrow = 2)

ggsave("good_gt1ra3_treatments_angle_rownumber_ML_carpel_area_data.jpg", height = 12, width = 16, units = "in")
ggsave("good_gt1ra3_treatments_angle_rownumber_ML_carpel_area_data.pdf", height = 12, width = 16, units = "in")


# 11. graphing handtraced vs all ML data
setwd("D:/Maize_carpel_area/carpel_training_9_22_21_v2/balloon")
handtraced_data <- read_csv("all_handtraced_area.csv")

handtraced_data$type <- "handtraced"
x$type <- "ML"

temp1 <- handtraced_data %>% filter(genotype == "gt1-P" | genotype == "gt1ra3" | genotype == "tb1gt1ra3") %>% select(genotype, carpel_area, type)
temp2 <- x %>% filter(genotype == "gt1-P" | genotype == "gt1ra3" | genotype == "tb1gt1ra3") %>% filter(treatment == "no_treatment", angle == "10") %>% select(genotype, carpel_area, type) #only using angle 10, handtracing was done at angle 9

HT_ML_combined <- rbind(temp1, temp2)

HT_ML_combined %>% ggplot(aes(x = type, y = carpel_area, color = type)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_quasirandom( size = .01, alpha=.35, width = .4) + 
  stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5, vjust = 0.9) + 
  stat_compare_means(method="t.test", cex=2.3) + 
  labs(y = "carpel area (pixels squared, 1 pixel = 30.3 um)") +
  facet_wrap(~genotype)


ggsave("ML_HT_compare_gt1P_gt1ra3_triple_no_treatment_carpel_area_data.jpg", height = 6, width = 9, units = "in")
ggsave("ML_HT_compare_gt1P_gt1ra3_triple_no_treatment_carpel_area_data.pdf", height = 6, width = 9, units = "in")