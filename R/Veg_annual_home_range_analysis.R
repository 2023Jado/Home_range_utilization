library(ggplot2)
library(patchwork)
library(dplyr)
library(mgcv)

hom_ann_veg <- read.csv("C:/Users/Jado/Documents/DFGF/RE_HR_analysis_2020-2022/Final_dataset/Ready_for_analysis/Home ranges_KRC_RDB_veg_zones_elevation_annual_2020-2022.csv", header=T)

# Effect of vegetation zone on home range size
# ############################################

# Let's start with percentage kernel:90
# #####################################
ann_veg_hom_90 <- subset(hom_ann_veg, Percentage == 90)
nrow(ann_veg_hom_90)

ann_veg_hom_50 <- subset(hom_ann_veg, Percentage == 50)
nrow(ann_veg_hom_50)

# Add new columns for each unique zone, indicating the area of the home range in that zone
for (zone in unique(ann_veg_hom_90$Zones)) {
  ann_veg_hom_90[[zone]] <- ifelse(ann_veg_hom_90$Zones == zone, ann_veg_hom_90$Area_veg_km2, 0)
}

for (zone in unique(ann_veg_hom_50$Zones)) {
  ann_veg_hom_50[[zone]] <- ifelse(ann_veg_hom_50$Zones == zone, ann_veg_hom_50$Area_veg_km2, 0)
}

# Then use glm and GAM to model the effect of vegetation zone on home range size
# ##############################################################################

# remove the spaces in the column names
names(ann_veg_hom_90) <- gsub(" ", "_", names(ann_veg_hom_90))
names(ann_veg_hom_50) <- gsub(" ", "_", names(ann_veg_hom_50))

# Subset the data
ann_veg_hom_90 <- ann_veg_hom_50 %>%
  select(-Zones, -Area_veg_km2)

ann_veg_hom_50 <- ann_veg_hom_50 %>%
  select(-Zones, -Area_veg_km2)


# remove the entire duplicate rows
ann_veg_hom_90 <- ann_veg_hom_90[!duplicated(ann_veg_hom_90), ]
ann_veg_hom_50 <- ann_veg_hom_50[!duplicated(ann_veg_hom_50), ]

# Fit a GLM model: 90% KDE
ann_glm_model_90 <- glm(Area_km2 ~ Herbaceous + Bamboo + Hagenia_forest +
                      Outside_of_the_park + Sub_alpine + Alpine +
                      Meadow + Mimulopsis + Mixed_forest + Brush_ridges,
                    data = ann_veg_hom_90,
                    family = gaussian(),
                    model = TRUE, method = "glm.fit")

# Summarize GLM results: 90% KDE
summary(ann_glm_model_90)

# Fit a GLM model: 50% KDE
ann_glm_model_50 <- glm(Area_km2 ~ Herbaceous + Bamboo + Hagenia_forest +
                      Outside_of_the_park + Sub_alpine + Alpine +
                      Meadow + Mimulopsis + Mixed_forest + Brush_ridges,
                    data = ann_veg_hom_50,
                    family = gaussian(),
                    model = TRUE, method = "glm.fit")

# Summarize GLM results: 50% KDE
summary(ann_glm_model_50)

# Fit a GAM model: 90% KDE

# Check the number of unique values in each predictor: 90% KDE
unique_values <- sapply(ann_veg_hom_90, function(x) length(unique(x)))
print(unique_values)

# Adjust the number of basis functions for predictors with fewer unique values
ann_gam_model_90 <- gam(
  Area_km2 ~
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
  data = ann_veg_hom_90,
  family = gaussian(),
  method = "GCV.Cp"
)

# Summary of the GAM model
summary(ann_gam_model_90)

# Plot the smooth terms
plot(ann_gam_model_90, pages = 1, all.terms = TRUE)

# Check residuals
par(mfrow = c(2, 2))
gam.check(ann_gam_model_90)

# Check the number of unique values in each predictor: 50% KDE
unique_values <- sapply(ann_veg_hom_50, function(x) length(unique(x)))
print(unique_values)

# Fit a GAM model: 50% KDE
ann_gam_model_50 <- gam(
  Area_km2 ~
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
  data = ann_veg_hom_50,
  family = gaussian(),
  method = "GCV.Cp")

# Summary of the GAM model
summary(ann_gam_model_50)

# Plot the smooth terms
plot(ann_gam_model_50, pages = 1, all.terms = TRUE)

# Check residuals
par(mfrow = c(2, 2))
gam.check(ann_gam_model_50)


