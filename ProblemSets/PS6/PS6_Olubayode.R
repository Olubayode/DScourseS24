library(dplyr)        # For data manipulation
library(data.table)   # For fast data manipulation
library(readr)        # To read/write data
library(caret)        # For data splitting and model training
library(glmnet)       # For linear models
library(Metrics)      # For model evaluation
library(ggplot2)      # For plotting
library(knitr)        # For tabulating results in R Markdown

library(readr)
library(dplyr)
getwd()

# Reading the CSV file into a dataframe
df <- read_csv('Chile_real_estate_listings.csv')

# Viewing the top rows of the dataframe, equivalent to df.head() in Python
head(df)

# removing duplicates
library(dplyr)

# Remove duplicates based on the 'id' column
df <- df %>% distinct(id, .keep_all = TRUE)

# Check for duplicates again (should return an empty data frame if no duplicates)
df[duplicated(df$id), ]

# Inspect the dataframe structure and summary
glimpse(df)

# Quality Report Function

quality_report <- function(df) {
  total <- sapply(df, function(x) sum(is.na(x)))
  percent <- sapply(df, function(x) mean(is.na(x)) * 100)
  nuniq <- sapply(df, function(x) length(unique(x)))
  dtype <- sapply(df, class)
  quality_df <- data.frame("Total NaN" = total, "Percent of NaN" = percent, 
                           "Nunique" = nuniq, "Dtype" = dtype)
  
  print(quality_df)
}

# Execute the quality report function on your dataframe
quality_report(df)

# Extracting uniques Values

# Display unique values in the 'operation' column
unique(df$operation)

# Display unique values in the 'property_type' column
unique(df$property_type)

library(tidyr)
library(ggplot2)
library(dplyr)

# Assuming df is your dataframe
#df_long <- df %>%
#  mutate(id = row_number()) %>%
  #pivot_longer(-id, names_to = "variable", values_to = "value") %>%
 # mutate(is_missing = is.na(value))

df_long <- df %>%
  mutate(id = row_number()) %>%
  # Convert all columns (except 'id') to character
  mutate(across(-id, as.character)) %>%
  pivot_longer(-id, names_to = "variable", values_to = "value") %>%
  mutate(is_missing = is.na(value))


# Create a heatmap of missing values and assign it to a variable
heatmap_plot <- ggplot(df_long, aes(x = variable, y = id, fill = is_missing)) +
  geom_tile() +
  scale_fill_manual(values = c("TRUE" = "gold", "FALSE" = "darkgreen")) +
  theme_light() +  # Using theme_light for a white background
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(fill = "Missing", x = "Variable", y = "Index") +
  guides(fill = guide_legend(title = "Is Missing?"))

# Save the heatmap plot to a PNG file
ggsave("heatmap_missing_data.png", plot = heatmap_plot, width = 10, height = 6, dpi = 300)


#BoxPlot
library(ggplot2)

# Assuming 'df' is your dataframe and 'expenses' is the column you're interested in
library(ggplot2)

# Assuming 'df' is your dataframe and 'expenses' is the column for the boxplot
boxplot <- ggplot(df, aes(y = expenses)) +
  geom_boxplot() +
  theme_light() +  # Using theme_light for a white background
  labs(title = "Boxplot of Expenses", y = "Expenses", x = "")

# Save the boxplot to a PNG file
ggsave("boxplot_expenses.png", plot = boxplot, width = 8, height = 6, dpi = 300)


library(dplyr)

# Assuming 'df' is your dataframe
df <- df %>%
  select(-expenses, -price_per_m2, -place_with_parent_names, -price_usd_per_m2)

library(dplyr)

df <- df %>%
  mutate(property_type = case_when(
    property_type == "apartment" ~ "1",
    property_type == "house" ~ "2",
    property_type == "store" ~ "3",
    property_type == "PH" ~ "4",
    TRUE ~ as.character(property_type)  # This line ensures that other values remain unchanged
  ))
df$property_type <- as.numeric(df$property_type)

library(dplyr)

# Assuming 'df' is your dataframe
df <- df %>%
  mutate(operation = case_when(
    operation == "rent" ~ "1",
    operation == "sell" ~ "2",
    TRUE ~ as.character(operation)  # Ensures that other values remain unchanged
  ))

# Convert the 'operation' column to numeric, if required
df$operation <- as.numeric(df$operation)

# Display the structure of the dataframe
glimpse(df)

#Calculating Mean Rooms by Property Type
library(dplyr)
df %>%
  group_by(property_type) %>%
  summarise(mean_rooms = mean(rooms, na.rm = TRUE))
#Filling Missing Room Values
df <- df %>%
  mutate(rooms = ifelse(is.na(rooms) & property_type == 1, 3,
                        ifelse(is.na(rooms) & property_type == 2, 5,
                               ifelse(is.na(rooms) & property_type == 3, 6,
                                      ifelse(is.na(rooms) & property_type == 4, 1, rooms)))))


# Converting Currency Values
df <- df %>%
  mutate(currency = case_when(
    currency == "CLP" ~ "1",
    currency == "CLF" ~ "2",
    currency == "np.NaN" ~ "0", # Check if this is correct; you might need is.na(currency)
    currency == "COP" ~ "3",
    currency == "USD" ~ "4",
    TRUE ~ as.character(currency)))

# Dropping a Column
df <- df %>%
  select(-price_aprox_local_currency)
#Calculating Mean Price by Rooms
df %>%
  group_by(rooms) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))


#Descriptive Statistics
summary(df)

# Filling Missing Values
df <- df %>%
  mutate(price = ifelse(is.na(price), 5.602594e+07, price),
         price_aprox_usd = ifelse(is.na(price_aprox_usd), 2.547893e+05, price_aprox_usd),
         surface_total_in_m2 = ifelse(is.na(surface_total_in_m2), 622.420789, surface_total_in_m2))


if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)
#Create a Logical Matrix for Missing Data
#missing_data <- is.na(df)
#: Visualize Missing Data with ggplot2
#library(tidyr)
#missing_data_long <- missing_data %>%
 # mutate(Row = row_number()) %>%
  #pivot_longer(cols = -Row, names_to = "Variable", values_to = "is_missing") %>%
  #mutate(Variable = as.factor(Variable))
  
library(tidyr)
library(dplyr)

# Assuming df is your original dataframe and missing_data is a logical matrix derived from it
missing_data <- is.na(df)

# Convert the logical matrix into a dataframe first
missing_data_df <- as.data.frame(missing_data)

# Now proceed with your original code, applied to the dataframe
missing_data_long <- missing_data_df %>%
  mutate(Row = row_number()) %>%
  pivot_longer(cols = -Row, names_to = "Variable", values_to = "is_missing") %>%
  mutate(Variable = as.factor(Variable))


ggplot(missing_data_long, aes(x = Variable, y = Row)) +
  geom_tile(aes(fill = is_missing), color = "purple") +
  scale_fill_manual(values = c("TRUE" = "gold", "FALSE" = "darkgreen")) +
  theme_light() +  # Using theme_light for a white background
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        legend.position = "none") +
  labs(y = "Row number")
#Save the Heatmap to a PNG File
ggsave("clean_heatmap_missing_data.png", width = 10, height = 6, dpi = 300)


library(ggplot2)

# Create the lmplot
lm_plot <- ggplot(df, aes(x = rooms, y = price)) +
  geom_point(aes(color = property_type), alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  # Linear model fit line
  theme_light() +  # Using theme_light for a white background
  labs(title = "Linear Model Plot: Price vs. Rooms",
       x = "Rooms",
       y = "Price")

# Display the plot
print(lm_plot)

# Save the plot as PNG
ggsave("lmplot_price_vs_rooms.png", plot = lm_plot, width = 8, height = 6, dpi = 300)

library(ggplot2)

# Ensure 'property_type' is treated as a factor for proper ordering and labeling in the plot
df$property_type <- factor(df$property_type)

# Create the bar plot
# Create the bar plot with different colors for each property_type
bar_plot <- ggplot(df, aes(x = property_type, y = rooms, fill = property_type)) +
  geom_bar(stat = "summary", fun = "mean", color = "black") +
  theme_light() +  # Using theme_light for a white background
  labs(title = "Average Rooms by Property Type",
       x = "Property Type",
       y = "Average Number of Rooms") +
  scale_fill_brewer(palette = "Set3")  # Optional: Use a specific color palette


# Display the plot
print(bar_plot)

# Save the plot as PNG
ggsave("barplot_rooms_by_property_type.png", plot = bar_plot, width = 8, height = 6, dpi = 300)



library(ggplot2)
library(dplyr)

# Assuming df_summary is already properly summarized and contains the 'factor_property_type' and 'rooms'

# Remove 0% categories
df_summary <- df_summary %>%
  filter(rooms > 0) %>%
  mutate(label = paste0(round((rooms / sum(rooms)) * 100, 1), "%"))

# Plot the pie chart
#pie_chart <- ggplot(df_summary, aes(x = "", y = rooms, fill = factor_property_type)) +
  #geom_bar(stat = "identity", width = 1) +
 # coord_polar("y", start = 0) +
 # geom_label(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
 # scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
#  #theme_light() +  # Using theme_light for a white background
 # theme_void() +  # Removes background elements including radial axis text
  #labs(title = "Percentage of Rooms in Each Property Type")
  

#print(pie_chart)

# Plot the pie chart
pie_chart <- ggplot(df_summary, aes(x = "", y = rooms, fill = factor_property_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_label(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
  #theme_minimal(base_size = 14) +  # Using theme_minimal for a clean white background
  theme_light() +
  theme(axis.text = element_blank(),  # Hide axis text
        axis.title = element_blank(),  # Hide axis titles
        axis.ticks = element_blank(),  # Hide axis ticks
        panel.grid = element_blank(),  # Hide grid
        panel.border = element_blank()) +  # Hide panel border
  labs(title = "Percentage of Rooms in Each Property Type")

# Display the plot
print(pie_chart)



# Save the plot
ggsave("pie_chart_rooms_by_property_type.png", plot = pie_chart, width = 8, height = 6, dpi = 300)

