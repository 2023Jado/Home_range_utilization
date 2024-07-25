library(ggplot2)
library(patchwork)
library(dplyr)
library(mgcv)

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
veg_hom_90 <- subset(hom_veg, Percentage == 90)
nrow(veg_hom_90)

veg_hom_50 <- subset(hom_veg, Percentage == 50)
nrow(veg_hom_50)

# Add new columns for each unique zone, indicating presence (1) or absence (0)
for (zone in unique(veg_hom_90$Zones)) {
  veg_hom_90[[zone]] <- ifelse(veg_hom_90$Zones == zone, veg_hom_90$Area_veg_km2, 0)
}

for (zone in unique(veg_hom_50$Zones)) {
  veg_hom_50[[zone]] <- ifelse(veg_hom_50$Zones == zone, veg_hom_50$Area_veg_km2, 0)
}

# Then use glm and GAM to model the effect of vegetation zone on home range size
# ##############################################################################

# remove the spaces in the column names
names(veg_hom_90) <- gsub(" ", "_", names(veg_hom_90))
names(veg_hom_50) <- gsub(" ", "_", names(veg_hom_50))

# Subset the data
veg_hom_90 <- veg_hom_90 %>%
  select(-Zones, -Area_veg_km2)

veg_hom_50 <- veg_hom_50 %>%
  select(-Zones, -Area_veg_km2)

# remove the entire duplicate rows
veg_hom_90 <- veg_hom_90[!duplicated(veg_hm_90), ]
veg_hom_50 <- veg_hom_50[!duplicated(veg_hm_50), ]

# ############## GLM ######################
# ########################################

# Fit a GLM model: 90% KDE
glm_model_90 <- glm(Area_km2 ~ Herbaceous + Bamboo + Hagenia_forest +
                   Outside_of_the_park + Sub_alpine + Alpine +
                   Meadow + Mimulopsis + Mixed_forest + Brush_ridges,
                 data = veg_hom_90,
                 family = gaussian(),
                 model = TRUE, method = "glm.fit")

# Summarize GLM results: 50% KDE
summary(glm_model_90)

# Fit a GLM model: 50% KDE
glm_model_50 <- glm(Area_km2 ~ Herbaceous + Bamboo + Hagenia_forest +
                      Outside_of_the_park + Sub_alpine + Alpine +
                      Meadow + Mimulopsis + Mixed_forest + Brush_ridges,
                    data = veg_hom_50,
                    family = gaussian(),
                    model = TRUE, method = "glm.fit")

# Summarize GLM results: 50% KDE
summary(glm_model_50)

##################### GAM ######################
# #############################################

# Check the number of unique values in each predictor: 90% KDE
unique_values <- sapply(veg_hom_90, function(x) length(unique(x)))
print(unique_values)

# Fit a GAM model: 90% KDE
gam_model_90 <- gam(Area_km2 ~
                      s(Herbaceous, k=30) +
                      s(Bamboo, k=33) +
                      s(Hagenia_forest, k=29) +
                      s(Outside_of_the_park, k=33) +
                      s(Sub_alpine, k=27) +
                      s(Alpine, k=20) +
                      s(Meadow, k=33) +
                      s(Mimulopsis, k=13) +
                      s(Mixed_forest, k=6) +
                      s(Brush_ridges, k=14),
                    data = veg_hom_90,
                    family = gaussian(),
                    method = "GCV.Cp")

# Summarize GAM results: 90% KDE
summary(gam_model_90)

# Visualize the smooth effects of the GAM model
plot(gam_model_90, pages = 1, all.terms = TRUE)

# Check the number of unique values in each predictor: 50% KDE
unique_values <- sapply(veg_hom_50, function(x) length(unique(x)))
print(unique_values)

# Fit a GAM model: 50% KDE
gam_model_50 <- gam(Area_km2 ~
                      s(Herbaceous, k=28) +
                      s(Bamboo, k=29) +
                      s(Hagenia_forest, k=29) +
                      s(Outside_of_the_park, k=27) +
                      s(Sub_alpine, k=22) +
                      s(Alpine, k=15) +
                      s(Meadow, k=19) +
                      s(Mimulopsis, k=8) +
                      s(Mixed_forest, k=6) +
                      s(Brush_ridges, k=9),
                    data = veg_hom_50,
                    family = gaussian(),
                    method = "GCV.Cp")

# Summarize GAM results: 50% KDE
summary(gam_model_50)

# Visualize the smooth effects of the GAM model
plot(gam_model_50, pages = 1, all.terms = TRUE)
