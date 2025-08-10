# Set the working directory to the specified folder on the desktop
setwd("Your_directory")

# Load required libraries for data manipulation, visualization, and analysis
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
# Load the ecoregions data file
ecor <- read.csv("ecoregion.csv")

# Estimate the diversity of ecoregions per country using the Shannon index
df <-  ecor %>%  dplyr::mutate(shannon_index = vegan::diversity(area_ha), .by = ISO_A3)
colnames(df)

# Remove duplicate rows based on selected columns and keep only necessary columns
df1 <- df [!duplicated(df[c(6,10)]),]
df1 <- df1[,c(6,10)]



############################################################################### CROPS 
# Load the crop data files (all crops and main crops)
crop_all <- read.csv("crops_all.csv") #all crops
crop_main <- read.csv("crops_main.csv") #major crops and capital data

# Select relevant columns from the crop_all dataset
crop_all <- crop_all[,c(3,6,7,8)]
colnames(crop_all)<-c("Area.Code..M49.","shannon_item_all", "shannon_itemgroup_all","Total_Harvested_Area")


# Merge crop data with response variables (Shannon index and harvested area of main crops)
crop <- merge(crop_main,crop_all, by="Area.Code..M49.")

# Merge with ecoregion diversity data
crop <- merge(crop, df1, by="Alpha.3code", by.y="ISO_A3")

# Invert the trend so negative values (reduction of yield gap) have a positive interpretation
crop$Relative_yield_gap_trend <- crop$Relative_yield_gap_trend*(-1)

# Estimate the proportion of harvested area dominated by the main crops ("dominance of major crops"
crop$proportion_main_harv <- crop$main_Harvested_Area/crop$Total_Harvested_Area
summary(crop$proportion_main_harv)

# Rename variables for clarity and better interpretation
crop<- crop %>% 
  dplyr::rename(
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

# Load and process GDP-related data (services and industry value added to GDP)
services <- read.csv("C:/Users/yohan/Desktop/crops/revision/servicesgdp.csv")
services$percent_service_value_added_to_GDP <- as.numeric(as.character(services$percent_service_value_added_to_GDP))

industry <- read.csv("C:/Users/yohan/Desktop/crops/revision/industrygdp.csv")
industry$percent_industry_value_added_to_GDP <- as.numeric(as.character(industry$percent_industry_value_added_to_GDP))

# Merge the services and industry data with the main crop data
crop <- merge(crop, services, by.x="Alpha.3code", by.y="Country.Code")
crop <- merge(crop, industry, by.x="Alpha.3code", by.y="Country.Code")
colnames(crop)



# Define the columns that will be used for analysis (response and predictors)
columns_to_use <- c("Alpha.3code", "Relative_yield_gap_trend",  "AverageWeightedYieldGap", # Response variables
                    "shannon_avg_item", "shannon_avg_itemgroup", "shannon_item_all", "shannon_itemgroup_all", "percent_total_country_area_cultivated", 
                    "Total_Land_Cover",   "proportion_main_harv",  "shannon_index", # Natural capital
                    "percent_cultivated_area_equipped_for_irrigation", "roadsm_km2","Avg_pest", "fert_avg", # Physical capital
                    "GDP.per.capita","percent_Agriculture_value_added_to_GDP", "percent_industry_value_added_to_GDP", "percent_service_value_added_to_GDP",# Financial capital
                    "Population.density", "percent_rural","HDI", # Human capital
                    "CV_NRI", "cv_temp", # Weather variables 
                    "Accountability", "law", "Regulation", "PoliticalStability", "Effectiveness", "Corruption", "Avg_democracy", "GII") # Socio-political capital
                    
# Select only relevant columns from the crop dataset
crops_subset <- crop %>%
  dplyr:: select(all_of(columns_to_use))

# Replace infinite values with NA and drop rows with missing values (NA)
crops_cleaned <- crops_subset %>%
  mutate(across(everything(), ~replace(.x, is.infinite(.x), NA))) %>%
  drop_na()

# Define the list of predictors used for further analysis
predictors <- c("shannon_avg_item", "shannon_avg_itemgroup", "shannon_item_all", "shannon_itemgroup_all", "percent_total_country_area_cultivated", 
"Total_Land_Cover",  
"proportion_main_harv",  "shannon_index", #natural cpaital
"percent_cultivated_area_equipped_for_irrigation", "roadsm_km2","Avg_pest", "fert_avg", # Physical capital
"GDP.per.capita","percent_Agriculture_value_added_to_GDP", "percent_industry_value_added_to_GDP", "percent_service_value_added_to_GDP",# Financial capital
"Population.density", "percent_rural","HDI", # Human capital
"CV_NRI", "cv_temp", # Weather variables 
"Accountability", "law", "Regulation", "PoliticalStability", "Effectiveness", "Corruption", "Avg_democracy", "GII") # Socio-political capital


######################## CREATE A SHAPEFILE WITH THE DATA (commented out)
#library(sf)
# countries <- read_sf("countries.shp")
# colnames(crops_cleaned)
# shp_count <- merge(countries,crops_cleaned, by.x="Alpha-3cod", by.y="Alpha.3code" )
# colnames(shp_count)
# # Create a mapping of old to new column names to avoid errors
# shp_count <- shp_count[, c(1:8, 12:25,27, 30:47 )]
# colnames(shp_count) <- c(
#   "Alpha3code", "fid", "FORMAL_EN", "ECONOMY", "INCOME_GRP", "WB_A3", "CONTINENT",
#   "SUBREGION","ryg_trend","avg_yg", "shann_avg", "shann_avg_grp", "shann_all", "shann_grp_all",
#   "pct_cult_area",   "Tot_land_cv", "prp_main_hrv", "ecor","pct_irrig", "roads_km2","Avg_pest",
#    "fert_avg", "GDP_percap","Agr_GDP","Ind_GDP", "Srv_GDP",
#   "Pop_dens","pct_rural", "HDI", "CV_NRI","cv_temp","Acctblty", "law", "rgltn", "pltcl_stab",
#   "effect","corrpt", "GII", "dmcrcy", "areakm", "geometry")
# 
# st_write(shp_count, "cropsdata_per_country.shp")


#------------------------------- SCALING VARIABLES FOR ANALYSIS
# Select only the numeric columns for scaling
numeric_features <- crops_cleaned %>%
  dplyr::select(where(is.numeric)) %>%
  names()

# Columns that should not be scaled (response variable and country code)
columns_not_to_scale <- c( 'Relative_yield_gap_trend', "Alpha.3code", "AverageWeightedYieldGap" )

# Remove the target variable and columns from the exclusion list, scale the remaining numeric features
numeric_features_to_scale <- setdiff(numeric_features, columns_not_to_scale)

# Define a custom function to scale features to the range [0, 1]
rescale_0_1 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply scaling to the numeric features
crops_cleaned <- crops_cleaned %>%
  mutate(across(all_of(numeric_features_to_scale), ~ rescale_0_1(.x)))

############################################# LOG CHECK
# Check for relationships between variables and perform log transformations on selected variables
plot(crops_cleaned$Relative_yield_gap_trend,crops_cleaned$Population.density)
summary(crops_cleaned$cv_temp)

# Variables to log-transform to handle zero values
vars_to_log <- c(
  "Population.density", 
  "CV_NRI", 
  "cv_temp"
)

# Apply log transformation (log(1 + x)) to the selected variables
crops_cleaned[vars_to_log] <- lapply(crops_cleaned[vars_to_log], function(x) log1p(x))
colnames(crops_cleaned)
plot(crops_cleaned$Relative_yield_gap_trend,crops_cleaned$cv_temp)

#########################   
# Define the target variable
target <- "Relative_yield_gap_trend"

# Group predictor variables by categories (e.g., strategies, capital, weather, etc.)
predictor_groups <- list(
  strategies=c("shannon_avg_item", "shannon_avg_itemgroup", "shannon_item_all", "shannon_itemgroup_all", "proportion_main_harv"), 
  natural_capital = c( "Total_Land_Cover",  "shannon_index"),
  physical_capital = c("percent_cultivated_area_equipped_for_irrigation", "roadsm_km2", "Avg_pest", "fert_avg"),
  financial_capital = c("GDP.per.capita", "percent_Agriculture_value_added_to_GDP", "percent_industry_value_added_to_GDP", "percent_service_value_added_to_GDP"),
  human_capital = c("Population.density",  "HDI"), #, "percent_rural"),
  weather_variables = c("CV_NRI", "cv_temp"),
  socio_political_capital = c(   "Avg_democracy", "GII") #,"PoliticalStability" ,"Accountability" ,"law", "Regulation", "Effectiveness", "Corruption")
)

#------------------------------- CORRELATION ANALYSIS
# Flatten the predictor groups into a single list for correlation analysis
capital_vars <- unlist(predictor_groups, use.names = FALSE)

# Subset the dataset with these variables for correlation calculation
capital_data <- crops_cleaned %>%
  dplyr::select(all_of(capital_vars))

# Calculate the correlation matrix for the selected variables
cor_matrix <- cor(capital_data, use = "pairwise.complete.obs")

# Plot the correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.7, order = "hclust")


#----------------------------- CORRELATION WITH TARGET
# Initialize an empty list to store correlations with the target variable
correlations_with_target <- list()

# Calculate the correlation for each predictor group with the target variable
for (group in names(predictor_groups)) {
  vars <- predictor_groups[[group]]
  
  # Subset the dataframe with the predictors and target variable
  df_group <- crops_cleaned[, c(vars, target)]
  
  # Calculate correlation of each predictor with the target variable
  cors <- sapply(vars, function(v) cor(crops_cleaned[[v]], crops_cleaned[[target]], use = "complete.obs"))
  
  correlations_with_target[[group]] <- round(cors, 3)
}

# Display results for one group (e.g., strategies)
correlations_with_target$strategies


#----------------------------- BEST MODEL SELECTION
# Set up the environment for modeling and fitting different combinations of predictors

library(dplyr)
library(purrr)
library(tidyr)

# Define the target variable
target <- "Relative_yield_gap_trend"

# Define strategy and capital variables for model fitting
strategy_pool <- c("shannon_avg_item", "shannon_avg_itemgroup", "shannon_itemgroup_all","proportion_main_harv")
climate_vars  <- c("cv_temp", "CV_NRI")

# List capital groups (natural, physical, financial, human, socio-political)
capital_groups <- list(
  natural = predictor_groups$natural_capital,
  physical = predictor_groups$physical_capital,
  financial = predictor_groups$financial_capital,
  human = predictor_groups$human_capital,
  socio_political = predictor_groups$socio_political_capital
)

# Generate combinations of 1 to 4 capital groups for modeling
capital_combinations <- unlist(
  lapply(1:4, function(i) combn(names(capital_groups), i, simplify = FALSE)),
  recursive = FALSE
)

# Build a grid of combinations for modeling (each row is a different combination)
search_grid <- tibble(capitals = capital_combinations) %>%
  mutate(
    capital_vars = purrr::map(capitals, ~ unlist(capital_groups[.x])),
    all_vars = purrr::map(capital_vars, ~ c(climate_vars, strategy_pool, .x))
  )

# Fit models for each combination and gather results
model_results <- pmap_dfr(search_grid, function(capitals, capital_vars, all_vars) {
  fmla <- reformulate(all_vars, response = target)
  model <- glm(fmla, data = crops_cleaned)
  pseudo_r2 <- 1 - (model$deviance / model$null.deviance)
  
  tibble(
    capital_combo = paste(capitals, collapse = " + "),
    num_vars = length(all_vars),
    AIC = AIC(model),
    pseudo_R2 = round(pseudo_r2, 4),
    formula = list(fmla),
    model_obj = list(model)
  )
})

# Sort the models by AIC (Akaike Information Criterion) for model selection
top_models <- model_results %>% arrange(AIC)

# Display top models sorted by AIC
print(top_models)

# Display a summary of the best model
summary(top_models$model_obj[[19]])
top_models$model_obj[[1]]$formula


library(broom)

# Expand top models with coefficients for inspection
top_models_expanded <- top_models %>%
  slice_head(n = 10) %>%
  mutate(
    model_summary = purrr::map(model_obj, ~ broom::tidy(.x))
  ) %>%
  unnest(model_summary)

# Export the top models to a CSV file
top_models_export <- top_models_expanded %>%
  select(where(~ !is.list(.)))  # Remove list columns

write.csv(top_models_export, "top_10_models_ok.csv", row.names = FALSE)


library(sjPlot)
length(crops_cleaned$shannon_avg_item)

# Fit a GLM (Generalized Linear Model) with the best predictors
best <- glm(Relative_yield_gap_trend ~  cv_temp + CV_NRI + shannon_avg_item + 
              shannon_avg_itemgroup + shannon_itemgroup_all + proportion_main_harv + Population.density + HDI, data=crops_cleaned)

# Show a summary of the best model
summary(best)

# Plot the estimated effects of the best model's predictors
p <- plot_model(model = best, 
                type = "est",
                 show.p = TRUE,        # Show p-values
                show.values = TRUE, digits = 3,value.size = 3, value.offset=0.25)

p1 <- p + scale_y_continuous(limits = c(-0.1, 0.1)) +theme_bw(base_size = 12)
p1

#--------------------------------- RANDOM FOREST

# Define the groups of predictors used in the analysis based on different types of capital:
# - Strategies: Variables related to crop diversity and dominance
# - Natural capital: Land cover and ecoregion diversity
# - Physical capital: Variables related to irrigation, road density, pest control, and fertilizer use
# - Financial capital: GDP per capita and agriculture, industry, and services share of GDP
# - Human capital: Population density and Human Development Index (HDI)
# - Socio-political capital: Democracy index and gender inequality index
# - Climate variables: Temperature and rainfall variability
predictor_groups <- list(
  strategies= c("shannon_avg_item", "shannon_avg_itemgroup",  "shannon_itemgroup_all","proportion_main_harv"), 
  natural_capital = c( "Total_Land_Cover",  "shannon_index"), 
  physical_capital = c("percent_cultivated_area_equipped_for_irrigation", "roadsm_km2", "Avg_pest", "fert_avg"),
  financial_capital = c("GDP.per.capita", "percent_Agriculture_value_added_to_GDP", 
                        "percent_industry_value_added_to_GDP", "percent_service_value_added_to_GDP"),
  human_capital = c("Population.density", "HDI"),
  socio_political_capital = c("Avg_democracy", "GII"),
  climate_vars  = c("cv_temp", "CV_NRI")
)

# 1. Merge all the predictor variables into a single vector and prepare the target variable
all_predictors <- unlist(predictor_groups)
target <- "Relative_yield_gap_trend"

# Subset the data to include only the predictors and the target variable
rf_data <- crops_cleaned[, c(all_predictors, target)]

# Remove rows with missing values to ensure clean data for analysis
rf_data <- rf_data[complete.cases(rf_data), ]
colnames(rf_data)  # Display the column names to check the data

#--------------------------------------------------------------------

# Random Forest Model Setup
library(randomForest)
library(caret)

# Set a random seed for reproducibility
set.seed(123)

# Define a grid of possible 'mtry' values (number of variables considered at each split)
tune_grid <- expand.grid(mtry = c(3, 5, 7, 10))

# Set up cross-validation control for training
control <- trainControl(
  method = "cv",         # Use cross-validation
  number = 5,            # Number of folds in cross-validation
  verboseIter = TRUE     # Show iteration details
)

# Prepare the dataset with predictors and target variable for training
rf_data1 <- rf_data[, c("Relative_yield_gap_trend", all_predictors)]

# Split the data into training and test sets
y <- rf_data1$Relative_yield_gap_trend
set.seed(123)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)

# Create training and testing datasets
train_data <- rf_data1[train_index, ]
test_data <- rf_data1[-train_index, ]
colnames(train_data)

# Extract predictors and target variable for model fitting
x <- train_data[, c(2:19)]  # Select predictors
y <- train_data$Relative_yield_gap_trend  # Select target variable

# Train the random forest model using the caret package
rf_tuned <- caret::train(
  x = x,
  y = y,
  method = "rf",           # Use Random Forest method
  trControl = control,     # Pass the cross-validation control
  tuneGrid = tune_grid,    # Tune the mtry parameter
  ntree = 500              # Number of trees in the forest
)

# Evaluate the model's performance on the testing set
preds <- predict(rf_tuned, newdata = test_data)

# Compute and display the model's performance metrics (RMSE, MAE, R²)
postResample(preds, test_data$Relative_yield_gap_trend)

########################### SHAPLEY RANDOM FOREST

# Prepare data for SHAP value interpretation
X_rf <- rf_data1[,-1]  # Exclude the target variable (first column)

# Set seed for reproducibility and sample 100 rows for background data
set.seed(1)
bg_X <- X_rf[sample(nrow(X_rf), 100), ]

# Calculate SHAP values using the kernelshap package
library(kernelshap)
shap_rf <- kernelshap(rf_tuned, X = X_rf, bg_X = bg_X)

# Visualize SHAP values using the shapviz package
library(shapviz)

# Create a beeswarm plot to show feature importance
sv2 <- shapviz(shap_rf)
sv_importance(sv2, kind = "beeswarm", show_numbers = TRUE, max_display = Inf, viridis_args = list(begin = 0.85, end = 0.1, option = "inferno"))


############## INTERACTION PLOTS

# Import necessary libraries for interaction plots
library(iml)
library(viridis)

# Set the response variable
y_rf <- crops_cleaned$Relative_yield_gap_trend

var_labels <- list(
  GDP.per.capita = "GDP per capita",
  percent_industry_value_added_to_GDP = "Industry (% of GDP)",
  percent_service_value_added_to_GDP = "Services (% of GDP)",
  percent_Agriculture_value_added_to_GDP = "Agriculture (% of GDP)",
  HDI = "Human Development Index",
  Population.density = "Population density",
  Avg_democracy = "Democracy index",
  GII = "Gender inequality Index",
  roadsm_km2 = "Road density",
  fert_avg = "Fertilizer use",
  Avg_pest = "Pesticide use",
  percent_cultivated_area_equipped_for_irrigation = "Irrigated area (%)",
  shannon_index = "Ecoregion diversity",
  Total_Land_Cover = "Natural land cover",
  cv_temp = "Temperature variability",
  CV_NRI = "Rainfall variability",
  proportion_main_harv = "Dominance of major crops",
  shannon_avg_item = "Taxonomic diversity of major crops",
  shannon_avg_itemgroup = "Functional diversity of major crops",
  shannon_itemgroup_all = "Functional diversity of all crops"
)

library(patchwork)
############################# diversity of major crops
interaction_targets <- setdiff(unlist(predictor_groups), "shannon_avg_item")

plot_list <- list()
set.seed(10)
for (var in interaction_targets) {
  pdp_2d <- FeatureEffect$new(predictor_rf,
                              feature = c(var, "shannon_avg_item"),
                              method = "pdp")
  
  # Use descriptive label for title
  var_label <- var_labels[[var]]
  p <- pdp_2d$plot() +
    ggtitle("") +
    scale_fill_viridis(option = "plasma", direction = -1) +
    labs(x = var_label, y = "Taxonomic Diversity major crops")
  
  plot_list[[var]] <- p
}


# Combine all plots in a grid: 3 columns (you can adjust)
combined_plot <- wrap_plots(plot_list, ncol = 3)

# Show or save the combined plot
print(combined_plot)

#1100x1300


################## functional diversity main groups

interaction_targets <- setdiff(unlist(predictor_groups), "shannon_avg_itemgroup")

plot_list <- list()

set.seed(10)
for (var in interaction_targets) {
  pdp_2d <- FeatureEffect$new(predictor_rf,
                              feature = c(var, "shannon_avg_itemgroup"),
                              method = "pdp")
  
  # Use descriptive label for title
  var_label <- var_labels[[var]]
  p <- pdp_2d$plot() +
    ggtitle("") +
    scale_fill_viridis(option = "plasma", direction = -1) +
    labs(x = var_label, y = "Functional diversity major crops")
  
  plot_list[[var]] <- p
}

# Combine all plots in a grid: 3 columns (you can adjust)
combined_plot <- wrap_plots(plot_list, ncol = 3)

# Show or save the combined plot
print(combined_plot)




################## functional diversity all crops

interaction_targets <- setdiff(unlist(predictor_groups), "shannon_itemgroup_all")

plot_list <- list()

set.seed(10)
for (var in interaction_targets) {
  pdp_2d <- FeatureEffect$new(predictor_rf,
                              feature = c(var, "shannon_itemgroup_all"),
                              method = "pdp")
  
  # Use descriptive label for title
  var_label <- var_labels[[var]]
  p <- pdp_2d$plot() +
    ggtitle("") +
    scale_fill_viridis(option = "plasma", direction = -1) +
    labs(x = var_label, y = "Functional diversity of all crops")
  
  plot_list[[var]] <- p
}


# Combine all plots in a grid: 3 columns (you can adjust)
combined_plot <- wrap_plots(plot_list, ncol = 3)

# Show or save the combined plot
print(combined_plot)






################## dominance of major crops

interaction_targets <- setdiff(unlist(predictor_groups), "proportion_main_harv")

plot_list <- list()
set.seed(10)

for (var in interaction_targets) {
  pdp_2d <- FeatureEffect$new(predictor_rf,
                              feature = c(var, "proportion_main_harv"),
                              method = "pdp")
  
  # Use a descriptive label for the title
  var_label <- var_labels[[var]]
  p <- pdp_2d$plot() +
    ggtitle("") +
    scale_fill_viridis(option = "plasma", direction = -1) +
    labs(x = var_label, y = "Dominance major crops")
  
  plot_list[[var]] <- p
}

# Combine all plots in a grid: 3 columns (you can adjust)
combined_plot <- wrap_plots(plot_list, ncol = 3)

# Show or save the combined plot
print(combined_plot)
