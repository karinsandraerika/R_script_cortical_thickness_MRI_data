####### Motor Skills #######
####### Compare Classic vs. MTX8 ####### 

rm(list = ls())

library(ggplot2)
library(psych)
library(dplyr)

# set working directory
#setwd("~/Documents/projects/MotorSkills/Scripts")

## load data
#mean.thickness.lh.file <- "~/Documents/projects/MotorSkills/Data/lh.meanthickness.allsubj.table"
mean.thickness.lh.table <- read.table(mean.thickness.lh.file, header = TRUE)
#mean.thickness.rh.file <- "~/Documents/Jobb/KI-Jobb/Data_analysis/projects/MotorSkills/Data/rh.meanthickness.allsubj.table"
mean.thickness.rh.table <- read.table(mean.thickness.rh.file, header = TRUE)

#View(mean.thickness.lh.table)
#View(mean.thickness.rh.table)

## Add variables for classic/mtx8, subject, control/experimental and session.
# subject
for (i in 1:length(mean.thickness.lh.table$lh.aparc.thickness)) {
  mean.thickness.lh.table$subject[i] <- substr(mean.thickness.lh.table$lh.aparc.thickness[i], 5, 11)
}
for (i in 1:length(mean.thickness.rh.table$rh.aparc.thickness)) {
  mean.thickness.rh.table$subject[i] <- substr(mean.thickness.rh.table$rh.aparc.thickness[i], 5, 11)
}
# session
for (i in 1:length(mean.thickness.lh.table$lh.aparc.thickness)) {
  mean.thickness.lh.table$session[i] <- substr(mean.thickness.lh.table$lh.aparc.thickness[i], 13, 13)
}
for (i in 1:length(mean.thickness.rh.table$rh.aparc.thickness)) {
  mean.thickness.rh.table$session[i] <- substr(mean.thickness.rh.table$rh.aparc.thickness[i], 13, 13)
}
# control/experimental
for (i in 1:length(mean.thickness.lh.table$lh.aparc.thickness)){
  if (substr(mean.thickness.lh.table$subject[i], 5, 5) == "1"){
  mean.thickness.lh.table$group[i] <- "experimental"
  }
  if (substr(mean.thickness.lh.table$subject[i], 5, 5) == "2"){
    mean.thickness.lh.table$group[i] <- "control"
  }
}
for (i in 1:length(mean.thickness.rh.table$rh.aparc.thickness)){
  if (substr(mean.thickness.rh.table$subject[i], 5, 5) == "1"){
    mean.thickness.rh.table$group[i] <- "experimental"
  }
  if (substr(mean.thickness.rh.table$subject[i], 5, 5) == "2"){
    mean.thickness.rh.table$group[i] <- "control"
  }
}
# classic/mtx8.
for (i in 1:length(mean.thickness.lh.table$lh.aparc.thickness)) {
  if (startsWith(mean.thickness.lh.table$subject[i], "lue2") && (mean.thickness.lh.table$session[i] == 7 || mean.thickness.lh.table$session[i] == 6)){
    mean.thickness.lh.table$system[i] <- "MTX8"
  } else if (mean.thickness.lh.table$subject[i] %in% c("lue4104", "lue4105") && mean.thickness.lh.table$session[i] == 5){
    mean.thickness.lh.table$system[i] <- "MTX8"
  } else {
    mean.thickness.lh.table$system[i] <- "Classic"
  }
}
for (i in 1:length(mean.thickness.rh.table$rh.aparc.thickness)) {
  if (startsWith(mean.thickness.rh.table$subject[i], "lue2") && (mean.thickness.rh.table$session[i] == 7 || mean.thickness.rh.table$session[i] == 6)){
    mean.thickness.rh.table$system[i] <- "MTX8"
  } else if (mean.thickness.rh.table$subject[i] %in% c("lue4104", "lue4105") && mean.thickness.rh.table$session[i] == 5){
    mean.thickness.rh.table$system[i] <- "MTX8"
  } else {
    mean.thickness.rh.table$system[i] <- "Classic"
  }
}
rm(i)

## Check data
# Descriptive statistics
summary(mean.thickness.lh.table)
summary(mean.thickness.rh.table)
#describe by Classic/MTX8. Total mean for all subjects
describeBy(mean.thickness.lh.table$lh_MeanThickness_thickness, group = mean.thickness.lh.table$system)
describeBy(mean.thickness.rh.table$rh_MeanThickness_thickness, group = mean.thickness.rh.table$system)
# Check mean thickness aggregated by session and system
aggregate(formula = lh_MeanThickness_thickness ~ session + system, data = mean.thickness.lh.table, FUN = mean)
aggregate(formula = rh_MeanThickness_thickness ~ session + system, data = mean.thickness.rh.table, FUN = mean)

## Look for outliers in MeanThickness_thickness.
#lh
out.mean.lh <- boxplot(mean.thickness.lh.table$lh_MeanThickness_thickness)$out
out.mean.lh <- out.mean.lh[order(out.mean.lh, decreasing = TRUE)]
# find index of outliers (ordered by table not out.mean)
which(mean.thickness.lh.table$lh_MeanThickness_thickness %in% out.mean.lh)
#rh
out.mean.rh <- boxplot(mean.thickness.rh.table$rh_MeanThickness_thickness)$out
out.mean.rh <- out.mean.rh[order(out.mean.rh, decreasing = TRUE)]
which(mean.thickness.rh.table$rh_MeanThickness_thickness %in% out.mean.rh) 

## Create subset containing only subjects that were scanned using MTX8
MTX8.subjects.lh <- subset(mean.thickness.lh.table, system == "MTX8")
MTX8.subset.lh <- subset(mean.thickness.lh.table, subject %in% unique(MTX8.subjects.lh$subject))
MTX8.subjects.rh <- subset(mean.thickness.rh.table, system == "MTX8")
MTX8.subset.rh <- subset(mean.thickness.rh.table, subject %in% unique(MTX8.subjects.rh$subject))
rm(MTX8.subjects.lh, MTX8.subjects.rh)
# Descriptive
describeBy(MTX8.subset.lh$lh_MeanThickness_thickness, group = MTX8.subset.lh$system)
describeBy(MTX8.subset.rh$rh_MeanThickness_thickness, group = MTX8.subset.rh$system)

################################################################
####### Comparing mean thickness of Classic/MTX8 for each region #######
# compute mean thickness for each region in the subset that were scanned with MTX8
# plot thickness based on the magnitude of the difference between systems.

## Compute mean of each region grouped by subject and Classic/MTX8 and place in new data frame
mean.per.system.lh <- MTX8.subset.lh %>%
  group_by(subject, system) %>%
  summarise_at(vars(lh_bankssts_thickness:eTIV), mean)
mean.per.system.rh <- MTX8.subset.rh %>%
  group_by(subject, system) %>%
  summarise_at(vars(rh_bankssts_thickness:eTIV), mean)

## Calculate the difference between Classic/MTX8 (MTX8 minus Classic) for each subject and region, place in new data frame.
diff.lh <- mean.per.system.lh %>%
  group_by(subject) %>%
  transmute_at(vars(lh_bankssts_thickness:lh_MeanThickness_thickness), diff)
diff.lh <- diff.lh %>% 
  group_by(subject) %>%
  summarise_all(mean)
diff.rh <- mean.per.system.rh %>%
  group_by(subject) %>%
  transmute_at(vars(rh_bankssts_thickness:rh_MeanThickness_thickness), diff)
diff.rh <- diff.rh %>% 
  group_by(subject) %>%
  summarise_all(mean)

## Calculate mean difference for each region in descending order.
mean.diff.lh <- data.frame(Region = names(subset(diff.lh[2:35])), mean_diff = colMeans(subset(diff.lh[2:35])))
mean.diff.lh <- mean.diff.lh %>%
  arrange(desc(abs(mean_diff)))
mean.diff.rh <- data.frame(Region = names(subset(diff.rh[2:35])), mean_diff = colMeans(subset(diff.rh[2:35])))
mean.diff.rh <- mean.diff.rh %>%
  arrange(desc(abs(mean_diff)))
# Add into one list for both hemispheres
mean.diff.lhrh <- rbind(mean.diff.lh, mean.diff.rh) %>%
  arrange(desc(abs(mean_diff)))

## Boxplots of mean thickness for Classic/MTX8 for each region. Set y axis to the same scale.
# Save all boxplots in a list.
# min(mean.per.system.lh[3:37]) min(mean.per.system.rh[3:37]) max(mean.per.system.lh[3:37]) max(mean.per.system.rh[3:37])
gg.list.lh <- list()
for (i in 1:length(mean.diff.lh$Region)) {
  gg.list.lh[[i]] <- ggplot(MTX8.subset.lh, aes_string(x = "system", y = colnames(MTX8.subset.lh[i+1]), fill = "system")) + geom_boxplot() + labs(title  = colnames(MTX8.subset.lh[i+1])) + coord_cartesian(ylim = c(1.5, 3.8))
}
gg.list.rh <- list()
for (i in 1:length(mean.diff.rh$Region)) {
  gg.list.rh[[i]] <- ggplot(MTX8.subset.rh, aes_string(x = "system", y = colnames(MTX8.subset.rh[i+1]), fill = "system")) + geom_boxplot() + labs(title  = colnames(MTX8.subset.rh[i+1])) + coord_cartesian(ylim = c(1.5, 3.8))
}
# Add into one list for both hemispheres and order the list according to magnitude of difference
gg.list.temp <- rbind(gg.list.lh, gg.list.rh)
gg.list.lhrh <- list()
for (i in 1:length(mean.diff.lhrh$Region)) {
  for (j in 1:length(gg.list.temp)){
    if (gg.list.temp[[j]]$labels$y == mean.diff.lhrh$Region[i]) {
      gg.list.lhrh[[i]] <- gg.list.temp[[j]]
      break
    }
  }
}
rm(gg.list.temp, i, j)

## Show the plots in list one by one
#k = 1
# Run this once for each plot
#gg.list.lhrh[[k]]
#k = k + 1

## Alternatives to add to plots
# add dots for subjects
# + geom_point()
# add mean
# + stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4)
# set y axis to same limit
# + coord_cartesian(ylim = c(1.5, 3.8))

##############################################################
####### Compare total mean (MeanThickness_thickness) per session for Classic/MTX8 ######
## Plot data with boxplot
# Number of observations, MTX8 subset
xtabs(~ session + system, MTX8.subset.lh)
# Number of observations, all subjects
xtabs(~ session + system, mean.thickness.lh.table)

# Lh boxplot: only the MTX8 subset
ggplot(MTX8.subset.lh, aes(x = session, y = lh_MeanThickness_thickness, fill = system)) + geom_boxplot() + labs(title  = "Subset: only subjects that were scanned with MTX8, LH")
# Lh boxplot: all subjects
ggplot(mean.thickness.lh.table, aes(x = session, y = lh_MeanThickness_thickness, fill = system)) + geom_boxplot() + labs(title  = "All subjects, LH")
# Rh boxplot: only the MTX8 subset
ggplot(MTX8.subset.rh, aes(x = session, y = rh_MeanThickness_thickness, fill = system)) + geom_boxplot() + labs(title  = "Subset: only subjects that were scanned with MTX8, RH")
# Rh boxplot: all subjects
ggplot(mean.thickness.rh.table, aes(x = session, y = rh_MeanThickness_thickness, fill = system)) + geom_boxplot() + labs(title  = "All subjects, RH")

## Compare mean cortical thickness between Classic and MTX8 with paired t-test/ Wilcoxon
# Combine mean of both lh and rh into one data frame
mean.per.system.session.lh <- MTX8.subset.lh %>%
  group_by(system, subject) %>%
  summarise(mean_thickness_system_lh = mean(lh_MeanThickness_thickness)) 
mean.per.system.session.rh <- MTX8.subset.rh %>%
  group_by(system, subject) %>%
  summarise(mean_thickness_system_rh = mean(rh_MeanThickness_thickness))
mean.per.system.session.both <- mean.per.system.session.lh
mean.per.system.session.both$mean_thickness_system_rh <- mean.per.system.session.rh$mean_thickness_system_rh
mean.per.system.session.both$mean_thickness_lhrh <- rowMeans(mean.per.system.session.both[ ,c("mean_thickness_system_lh", "mean_thickness_system_rh")])
# Mean and sd
group_by(mean.per.system.session.both, system) %>%
  summarise(
    count = n(),
    mean = mean(mean_thickness_lhrh),
    sd = sd(mean_thickness_lhrh)
  )
# check normality of the difference
difference <- with(mean.per.system.session.both, mean_thickness_lhrh[system == "Classic"] - mean_thickness_lhrh[system == "MTX8"])
shapiro.test(difference)
#t.test(mean_thickness_lhrh ~ system, data = mean.per.system.both, paired = TRUE)
wilcox.test(mean_thickness_lhrh ~ system, data = mean.per.system.session.both, paired = TRUE)

