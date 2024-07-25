library(ggplot2)
library(patchwork)
library(dplyr)

# Read the files
hom_monthly <- read.csv("C:/Users/Jado/Documents/DFGF/RE_HR_analysis_2020-2022/Final_dataset/Ready_for_analysis/Home ranges_KRC_RDB_monthly_elevation_2020-2022.csv", header=T)
hom_annual <- read.csv("C:/Users/Jado/Documents/DFGF/RE_HR_analysis_2020-2022/Final_dataset/Ready_for_analysis/Home ranges_KRC_RDB_elevation_annual_2020-2022.csv", header=T)
hom_veg <- read.csv("C:/Users/Jado/Documents/DFGF/RE_HR_analysis_2020-2022/Final_dataset/Ready_for_analysis/Home ranges_KRC_RDB_veg_zones_monthly_elevation_2020-2022.csv", header=T)

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

# Correlation between home range size and various factors
# #######################################################

# Correlation between home range size and elevation
# #################################################
cor.test(as.numeric(hom_monthly$Average_Elevation_DEM), as.numeric(hom_monthly$Area_km2), method = "spearman",
         conf.level = 0.90)

qplot(x = Average_Elevation_DEM, y = Area_km2, data = hom_monthly) +
  geom_smooth(method = "lm") + xlab("Average elevation [m]") +
  ylab("Area [km2]") + xlim(min(hom_monthly$Average_Elevation_DEM), max(hom_monthly$Average_Elevation_DEM)) +
  ylim(min(hom_monthly$Area_km2), max(hom_monthly$Area_km2))

# Correlation between home range size and N of GPS locations
# ##########################################################
cor.test(as.numeric(hom_monthly$Nb_of_GPS), as.numeric(hom_monthly$Area_km2), method = "spearman",
         conf.level = 0.90)

qplot(x = Nb_of_GPS, y = Area_km2, data = hom_monthly) +
  geom_smooth(method = "lm") + xlab("Number of GPS points") +
  ylab("Area [km2]") + xlim(min(hom_monthly$Nb_of_GPS), max(hom_monthly$Nb_of_GPS)) +
  ylim(min(hom_monthly$Area_km2), max(hom_monthly$Area_km2))

# Correlation between home range size and N interactions
# ######################################################
cor.test(as.numeric(hom_monthly$N_of_interactions), as.numeric(hom_monthly$Area_km2), method = "spearman",
         conf.level = 0.90)

qplot(x = N_of_interactions, y = Area_km2, data = hom_monthly) +
  geom_smooth(method = "lm") + xlab("Number of interactions") +
  ylab("Area [km2]") + xlim(min(hom_monthly$N_of_interactions), max(hom_monthly$N_of_interactions)) +
  ylim(min(hom_monthly$Area_km2), max(hom_monthly$Area_km2))

# Correlation between home range size and N of individuals
# ########################################################
cor.test(as.numeric(hom_monthly$N_of_individuals), as.numeric(hom_monthly$Area_km2), method = "spearman",
         conf.level = 0.90)

qplot(x = N_of_individuals, y = Area_km2, data = hom_monthly) +
  geom_smooth(method = "lm") + xlab("Number of individuals") +
  ylab("Area [km2]") + xlim(min(hom_monthly$N_of_individuals), max(hom_monthly$N_of_individuals)) +
  ylim(min(hom_monthly$Area_km2), max(hom_monthly$Area_km2))

# Effect of vegetation zone on home range size
# ############################################

# Let's start with percentage kernel:90
# #####################################

veg_hom_90 <- hom_veg %>%
  filter(Percentage == 90)
nrow(veg_hom_90)

veg_hom_50 <- hom_veg %>%
  filter(Percentage == 50)
nrow(veg_hom_50)

# Add new columns for each unique zone, indicating presence (1) or absence (0)
veg_hom_90_trans <- for (zone in unique(veg_hom_90$Zones)) {
  veg_hom_90[[zone]] <- ifelse(veg_hom_90$Zones == zone, 1, 0)
}

veg_hom_50_trans <- for (zone in unique(veg_hom_50$Zones)) {
  veg_hom_50[[zone]] <- ifelse(veg_hom_50$Zones == zone, 1, 0)
}

# Then use glm and GAM to model the effect of vegetation zone on home range size
# ##############################################################################

# Fit a GLM model

