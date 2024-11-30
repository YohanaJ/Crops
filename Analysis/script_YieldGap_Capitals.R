setwd("C:/Users/yohan/Desktop/crops")
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(corrplot)
library(readxl)
library(vegan)
library(dplyr)
library(broom)




############### ECORREGIONS
#load ecoregions file
ecor <- read.csv("ecoregion.csv")
colnames(ecor)
str(ecor)
#estimation of diversity of ecoregions per country
df <-  ecor %>%  dplyr::mutate(shannon_index = vegan::diversity(area_ha), .by = ISO_A3)
colnames(df)
df1 <- df [!duplicated(df[c(6,10)]),]
df1 <- df1[,c(6,10)]




############################################################################### CROPS 
crop_all <- read.csv("crops_all.csv")
crop_main <- read.csv("crops_main.csv")
colnames(crop_all)
crop_all <- crop_all[,c(3,6,7,8)]
colnames(crop_all)<-c("Area.Code..M49.","shannon_item_all", "shannon_itemgroup_all","Total_Harvested_Area")
# merge the database with the response variables and predictor with the shannon index and harveste area of the main crops
crop <- merge(crop_main,crop_all, by="Area.Code..M49.")
#merge dataset with responde variable and predictors with diversity of ecoregions
crop <- merge(crop, df1, by="Alpha.3code", by.y="ISO_A3")

# estimate the dominance of the major crops 
crop$proportion_main_harv <- crop$main_Harvested_Area/crop$Total_Harvested_Area

### rename the variables for clarity
crop<- crop %>% 
  rename(
    GII = `Gender.Inequality.Index..GII...equality...0..inequality...1.`,
    HDI = `Human.Development.Index..HDI...highest...1.`,
    percent_cultivated_area_equipped_for_irrigation = `X..of.the.cultivated.area.equipped.for.irrigation`,
    percent_total_country_area_cultivated = `X..of.total.country.area.cultivated`,
    percent_Agriculture_value_added_to_GDP = `Agriculture..value.added....GDP.`,
    percent_rural = `X.rural`,
    Area_km2 = `Area.km2.`,
    Services_value_added_to_GDP = `Services..value.added.to.GDP`,
    Agriculture_value_added_to_GDP=`Agriculture..value.added.to.GDP`,
    Industry_value_added_to_GDP=`Industry..value.added.to.GDP`
  )

# estimate the percentaje of service and indutry to GDP usign agriculture information
crop$percent_service_value_added_to_GDP <- (crop$Services_value_added_to_GDP* crop$percent_Agriculture_value_added_to_GDP)/crop$Agriculture_value_added_to_GDP
crop$percent_industry_value_added_to_GDP <- (crop$Industry_value_added_to_GDP*  crop$percent_Agriculture_value_added_to_GDP)/crop$Agriculture_value_added_to_GDP


# Define the columns I am using for the analysis
columns_to_use <- c("Alpha.3code", "Relative_yield_gap_trend",  "AverageWeightedYieldGap", #response
                    "shannon_avg_item", "shannon_avg_itemgroup", "shannon_item_all", "shannon_itemgroup_all", "percent_total_country_area_cultivated", #production capital
                     "Total_Land_Cover",   "proportion_main_harv",  "shannon_index", #Natural capital
                    "percent_cultivated_area_equipped_for_irrigation", "roadsm_km2","Avg_pest", "fert_avg", # physical capital
                    "Agriculture_value_added_to_GDP", "GDP.per.capita", "Industry_value_added_to_GDP", "Services_value_added_to_GDP","percent_Agriculture_value_added_to_GDP", "percent_industry_value_added_to_GDP", "percent_service_value_added_to_GDP",#financial capital
                    "Population.density", "percent_rural","HDI", #human capital
                    "CV_NRI", "cv_temp", # weather variables 
                    "Accountability", "law", "Regulation", "PoliticalStability", "Effectiveness", "Corruption", "Avg_democracy", "GII", #socio-political capital
                    "Area_km2"
                    )
# Select only the relevant columns from crops
crops_subset <- crop %>%
  select(all_of(columns_to_use))
# Replace Inf values with NA and drop rows with NA values
crops_cleaned<- crops_subset%>%
  mutate(across(everything(), ~replace(.x, is.infinite(.x), NA))) %>%
  drop_na()


   

predictors <- c("shannon_avg_item", "shannon_avg_itemgroup", "shannon_item_all", "shannon_itemgroup_all", "percent_total_country_area_cultivated", #production capital
"Total_Land_Cover",   "proportion_main_harv",  "shannon_index", #Natural capital
"percent_cultivated_area_equipped_for_irrigation", "roadsm_km2","Avg_pest", "fert_avg", # physical capital
"GDP.per.capita","percent_Agriculture_value_added_to_GDP", "percent_industry_value_added_to_GDP", "percent_service_value_added_to_GDP",#financial capital
"Population.density", "percent_rural","HDI", #human capital
"CV_NRI", "cv_temp", # weather variables 
"Accountability", "law", "Regulation", "PoliticalStability", "Effectiveness", "Corruption", "Avg_democracy", "GII") #socio-political capital


######################## use to create a shapefile with the data
# library(sf)
# countries <- read_sf("countries.shp")
# colnames(crops_cleaned)
# shp_count <- merge(countries,crops_cleaned, by.x="Alpha-3cod", by.y="Alpha.3code" )
# colnames(shp_count)
# # Creating a mapping of old to new column names to avoid errors
# shp_count <- shp_count[, c(1:8, 12:25,27, 30:47 )]
# colnames(shp_count) <- c(
#   "Alpha3code", "fid", "FORMAL_EN", "ECONOMY", "INCOME_GRP", "WB_A3", "CONTINENT",
#   "SUBREGION","ryg_trend","avg_yg", "shann_avg", "shann_avg_grp", "shann_all", "shann_grp_all",
#   "pct_cult_area",   "Tot_land_cv", "prp_main_hrv", "ecor","pct_irrig", "roads_km2","Avg_pest",
#    "fert_avg", "GDP_percap","Agr_GDP","Ind_GDP", "Srv_GDP",
#   "Pop_dens","pct_rural", "HDI", "CV_NRI","cv_temp","Acctblty", "law", "rgltn", "pltcl_stab",
#   "effect","corrpt", "GII", "dmcrcy", "areakm", "geometry")
# 
# 
# st_write(shp_count, "cropsdata_per_country.shp")



# Exclude non-numeric columns from scaling
numeric_features <- crops_cleaned %>%
  select(where(is.numeric)) %>%
  names()

# Columns not to scale (response variables and country code)
columns_not_to_scale <- c( 'Relative_yield_gap_trend', "Alpha.3code", "AverageWeightedYieldGap" )

# Remove the target variable and other specified columns from scaling, scale the data so we can compare the importance
numeric_features_to_scale <- setdiff(numeric_features, columns_not_to_scale)

# Scale only the numeric features that are not in the exclusion list
rescale_minus1_1 <- function(x) {
  2 * ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) - 1
}

# Apply the rescaling to the selected numeric features
crops_cleaned <- crops_cleaned %>%
  mutate(across(all_of(numeric_features_to_scale), ~ rescale_minus1_1(.x)))


#########################  model one-to-one variables 
# Define the target
target <- "Relative_yield_gap_trend"

# Initialize an empty data frame to store results
results <- data.frame(Predictor = character(), Slope = numeric(), P_Value = numeric(), SE = numeric(), Significance = character(), stringsAsFactors = FALSE)

# Loop through each predictor
for (predictor in predictors) {
  formula <- as.formula(paste(target, "~", predictor))
  model <- glm(formula, data = crops_cleaned)
  
  slope <- coef(summary(model))[2, "Estimate"]
  p_value <- coef(summary(model))[2, "Pr(>|t|)"]
  se <- coef(summary(model))[2, "Std. Error"]
  
  
  results <- rbind(results, data.frame(Predictor = predictor, Slope = slope, P_Value = p_value, SE = se, Significance = significance))
}

# Filter significant variables
sub_signif <- subset(results, results$P_Value < 0.05)
sub_signif$Predictor <- factor(sub_signif$Predictor, levels = sub_signif$Predictor[order(sub_signif$Slope)])


# Add error bars and significance to the plot
ggplot(sub_signif, aes(x = reorder(Predictor, Slope), y = Slope, fill = Slope > 0)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Slope - SE, ymax = Slope + SE), width = 0.2, color = "black") +  # Add error bars
  scale_x_discrete(labels = new_labels_lineal)+
  coord_flip() +
  labs(
    title = "",
    x = "Predictor",
    y = "Estimate"
  ) +
  scale_fill_manual(values = c("TRUE" = "#53868B", "FALSE" = "#CD5B45")) +  # Positive in blue, negative in red
  theme_bw() +
  theme(legend.position = "none")



############################## stepwise selection, multivariable models

target <- 'Relative_yield_gap_trend'
formula <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
null_model <- glm(formula = as.formula(paste(target, "~ 1")), data = crops_cleaned , family = gaussian)

# Define the full model (all predictors)
full_model <- glm(formula = formula, data = crops_cleaned , family = gaussian)


# Perform stepwise selection
stepwise_model <- step(full_model, direction = "both")
summary(stepwise_model)

model_summary <- broom::tidy(stepwise_model)

# Define significance codes based on p-value
model_summary <- model_summary %>%
  filter(term != "(Intercept)") %>% # Exclude the intercept
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    estimate_sign = ifelse(estimate > 0, "Positive", "Negative") # Define positive/negative estimates
  )
new_labels <- c(
  "shannon_avg_item" = "Diversity of main crops",    
  "shannon_item_all" = "Diversity of all crops",                        
  "shannon_itemgroup_all" = "Diversity of functional groups in main crops",                
  "shannon_index" = "Diversity of ecoregions",                
  "percent_cultivated_area_equipped_for_irrigation" = "Percentage cultivated equipped for irrigation",
  "Avg_pest" = "Pesticide use",                     
  "GDP.per.capita" = "GDP per capita",                
  "percent_industry_value_added_to_GDP" = "Percent of industry value added to GDP",  
  "Population.density" = "Population density",
  "percent_rural" = "Percentage of rural population", 
  "CV_NRI" = "Variation of NRI",     
  "Regulation" = "Regulatory quality",                                
  "PoliticalStability" = "Political Stability and Absence of Violence/Terrorism"
)

# Modify the plot to include the new labels
plot <- ggplot(model_summary, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(aes(color = estimate > 0), size = 3) + # Use logical color (positive or negative) without showing legend
  geom_errorbar(aes(ymin = estimate - std.error * 1.96, ymax = estimate + std.error * 1.96), width = 0.2) + # 95% CI
  geom_text(aes(label = significance, shape = significance), hjust = -0.5, vjust = 0.5, size = 6, color = "black") + # Add larger significance codes
  coord_flip() + # Flip coordinates for better readability
  labs(
    x = "Predictors",
    y = "Estimates",
    title = "",
    subtitle = ""
  ) +
  scale_color_manual(values = c("TRUE" = "#00CDCD", "FALSE" = "#CD5B45"), guide = "none") + # Custom colors without showing legend
  scale_shape_manual(
    values = c("***" = 16, "**" = 16, "*" = 16, "." = 16), # Shapes for asterisks
    labels = c("***" = "p < 0.001", "**" = "p < 0.01", "*" = "p < 0.05", "." = "p < 0.1"),
    name = "Significance Codes"
  ) +
  scale_x_discrete(labels = new_labels) + # Apply the new labels to the x-axis (flipped, so actually the y-axis)
  theme_minimal()

# Display the plot
print(plot)











############################### yield gap average

target2 <- 'AverageWeightedYieldGap'
formula2 <- as.formula(paste(target2, "~", paste(predictors, collapse = " + ")))
null_model2 <- glm(formula = as.formula(paste(target2, "~ 1")), data = crops_cleaned , family = gaussian)

# Define the full model (all predictors)
full_model2 <- glm(formula = formula2, data = crops_cleaned , family = gaussian)
summary(full_model)

# Perform stepwise selection
stepwise_model2 <- step(full_model2, direction = "both")
summary(stepwise_model2)


model_summary2 <- broom::tidy(stepwise_model2)
model_summary2 <- model_summary2 %>%
  filter(term != "(Intercept)") %>% # Exclude the intercept
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    estimate_sign = ifelse(estimate > 0, "Positive", "Negative") # Define positive/negative estimates
  )

new_labels2 <- c(
  "shannon_avg_item" = "Diversity of main crops",    
  "shannon_item_all" = "Diversity of all crops",                        
  "shannon_itemgroup_all" = "Diversity of functional groups in main crops",                
  "Total_Land_Cover" = "Natural land cover",                
  "fert_avg" = "Fertilizer use",
  "Avg_pest" = "Pesticide use",                     
  "GDP.per.capita" = "GDP per capita",                
  "percent_industry_value_added_to_GDP" = "Percent of industry value added to GDP",  
  "percent_Agriculture_value_added_to_GDP"= "Percent of agriculture value added to GDP", 
  "percent_service_value_added_to_GDP"= "Percent of service value added to GDP", 
  "Population.density" = "Population density",
  "percent_rural" = "Percentage of rural population", 
  "HDI" = "HDI",     
  "Regulation" = "Regulatory quality",                                
  "GII" = "Gini index"
)


plot2 <- ggplot(model_summary2, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(aes(color = estimate > 0), size = 3) + # Use logical color (positive or negative) without showing legend
  geom_errorbar(aes(ymin = estimate - std.error * 1.96, ymax = estimate + std.error * 1.96), width = 0.2) + # 95% CI
  geom_text(aes(label = significance, shape = significance), hjust = -0.5, vjust = 0.5, size = 6, color = "black") + # Add larger significance codes
  coord_flip() + # Flip coordinates for better readability
  labs(
    x = "Predictors",
    y = "Estimates",
    title = "",
    subtitle = ""
  ) +
  scale_color_manual(values = c("TRUE" = "#00CDCD", "FALSE" = "#CD5B45"), guide = "none") + # Custom colors without showing legend
  scale_shape_manual(
    values = c("***" = 16, "**" = 16, "*" = 16, "." = 16), # Shapes for asterisks
    labels = c("***" = "p < 0.001", "**" = "p < 0.01", "*" = "p < 0.05", "." = "p < 0.1"),
    name = "Significance Codes"
  ) +
  scale_x_discrete(labels = new_labels2) + # Apply the new labels to the x-axis (flipped, so actually the y-axis)
  theme_minimal()

# Display the plot
print(plot2)





  
  
  ##################################### Correlation predictors vs response
  
target_variable <- "Relative_yield_gap_trend"  
columns_to_select <- c(predictors, target_variable)
  
# Select the predictors and the target variable from the dataset
data_selected <- crops_cleaned[, columns_to_select]
  
# Calculate the correlation matrix
cor_matrix <- cor(data_selected, use = "complete.obs")
  
# Extract the correlations with the target variable
target_correlations <- as.data.frame(cor_matrix[, target_variable, drop = FALSE])
  
  