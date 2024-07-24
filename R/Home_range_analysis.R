library(ggplot2)
library(dplyr)

# Read the files
hom_monthly <- read.csv("C:/Users/Jado/Documents/DFGF/RE_HR_analysis_2020-2022/Final_dataset/Ready_for_analysis/Home ranges_KRC_RDB_monthly_elevation_2020-2022.csv", header=T)
hom_annual <- read.csv("C:/Users/Jado/Documents/DFGF/RE_HR_analysis_2020-2022/Final_dataset/Ready_for_analysis/Home ranges_KRC_RDB_elevation_annual_2020-2022.csv", header=T)

# First objective: Compare the monthly and annual home range sizes for groups ##
# ranging in Eastern versus those in the Western part of the VNP and determine##
# the effect of vegetation zone on the home range size #########################
# ##############################################################################

# Assigning a new column to the dataset which shows eastern and western groups
hom_monthly$Gorilla_group_part <- ifelse(hom_monthly$Gorilla_group %in% c("KWI", "KSA", "MUH", "SAB"), "Eastern groups", "Western groups")
hom_annual$Gorilla_group_part <- ifelse(hom_annual$Gorilla_group %in% c("KWI", "KSA", "MUH", "SAB"), "Eastern groups", "Western groups")

# Monthly HR size with error bars and points
# ###########################################

# Calculate summary statistics for each Gorilla group and percentage kernel
summary_stats <- hom_monthly %>%
  group_by(Gorilla_group, Gorilla_group_part, Percentage_kernel) %>%
  summarise(
    min_area = min(Area_km2),
    mean_area = mean(Area_km2),
    max_area = max(Area_km2)
  )

# Plot monthly HR size with error bars and points
ggplot(data = summary_stats) +
  geom_point(aes(x = Gorilla_group, y = mean_area, color = as.factor(Percentage_kernel)), size = 2) +
  geom_errorbar(aes(x = Gorilla_group, ymin = min_area, ymax = max_area, color = as.factor(Percentage_kernel)),
                width = 0.2) +
  facet_wrap(~ Gorilla_group_part, scales = "free") +
  scale_color_manual(
    values = c("90" = "red", "50" = "black"),
    labels = c("90" = "90% KDE", "50" = "50% KDE"),
    name = ""
  ) +
  xlab("Groups") +
  ylab("Area (km²)") +
  theme(legend.position = "bottom")

# Annual HR size with error bars and points
# ###########################################

# Calculate summary statistics for each Gorilla group and percentage kernel
summary_stats <- hom_annual %>%
  group_by(Gorilla_group, Gorilla_group_part, Percentage) %>%
  summarise(
    min_area = min(Area_km2),
    mean_area = mean(Area_km2),
    max_area = max(Area_km2)
  )

# Plot Annual HR size with error bars and points
ggplot(data = summary_stats) +
  geom_point(aes(x = Gorilla_group, y = mean_area, color = as.factor(Percentage)), size = 2) +
  geom_errorbar(aes(x = Gorilla_group, ymin = min_area, ymax = max_area, color = as.factor(Percentage)),
                width = 0.2) +
  facet_wrap(~ Gorilla_group_part, scales = "free") +
  scale_color_manual(
    values = c("90" = "red", "50" = "black"),
    labels = c("90" = "90% KDE", "50" = "50% KDE"),
    name = ""
  ) +
  xlab("Groups") +
  ylab("Area (km²)") +
  theme(legend.position = "bottom")

# Find the effect of vegetation zone on the home range size ###################
# ############################################################################

