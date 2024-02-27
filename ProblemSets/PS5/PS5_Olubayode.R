library(rvest)
library(knitr)

# Fetching the page
url <- 'https://en.wikipedia.org/wiki/List_of_capitals_of_states_of_Nigeria'
page <- read_html(url)

# Extract the first wikitable sortable table
table <- html_node(page, "table.wikitable.sortable")

# Parse the table into a dataframe
df <- html_table(table, fill = TRUE)

# Display the dataframe using kable for better formatting
kable(df)

# Load required libraries
library(rvest)
library(xml2)
library(dplyr)
library(officer)
library(magrittr)

# Fetching the page
url <- 'https://en.wikipedia.org/wiki/List_of_capitals_of_states_of_Nigeria'
page <- read_html(url)

# Extract the correct table
table <- html_node(page, "table.wikitable")

# Parse the table into a dataframe
df <- html_table(table, fill = TRUE)

# Since df should be a dataframe, let's directly use it
# First, inspect the structure of df to understand its columns
str(df)

# Assuming the dataframe structure is correctly identified, and you have columns for states and capitals,
# replace 'State' and 'Capital' with the actual column names if they differ
# Here, I'm assuming the first column is the state and the second is the capital based on common table structures
# But you should adjust these based on the actual column names you see in the structure output
states_nig <- df[[1]]  # Access the first column, presumably states
capitals_nig <- df[[2]]  # Access the second column, presumably capitals


# Generate random questions
set.seed(123) # For reproducibility
for (i in 1:20) {
  index <- sample(nrow(df), 1)
  state <- df$State[index]
  capital <- df$Capital[index]
  cat(sprintf('What is the capital of %s?\n', state))
}

library(officer)

# Create a new Word document
doc <- read_docx()

set.seed(123) # For reproducibility

for (i in 1:20) {
  # Sample a row for the current question
  index <- sample(nrow(df), 1)
  state <- df$State[index]
  capital <- df$Capital[index]
  
  # Generate three random options different from the correct answer
  options <- sample(df$Capital[df$Capital != capital], 3)
  
  # Add the correct answer to the options and shuffle them
  options <- sample(c(capital, options))
  
  # Add the question to the document
  question_text <- sprintf("Question %d: What is the capital of %s?", i, state)
  doc <- body_add_par(doc, question_text, style = "Normal")
  
  # Add the options to the document
  options_text <- sprintf("A. %s    B. %s    C. %s    D. %s", options[1], options[2], options[3], options[4])
  doc <- body_add_par(doc, options_text, style = "Normal")
  
  # Add an empty line for spacing (correct usage without specifying argument names)
  doc <- body_add_par(doc, "", style = "Normal")
}

# Specify the path and filename for your Word document
file_path <- "Quiz_Capitals_of_Nigeria.docx"

# Save the document
print(doc, target = file_path)

cat(sprintf("The quiz has been saved to '%s'.", file_path))


# SECOND TASK


library(httr)
library(jsonlite)

# Function to fetch weather data
fetch_weather <- function(city) {
  # Access API key from environment variable
  api_key <- Sys.getenv("OPENWEATHER_API_KEY")
  url <- sprintf("http://api.openweathermap.org/data/2.5/weather?q=%s&appid=%s&units=metric", city, api_key)
  response <- GET(url)
  content(response, "parsed", type = "application/json")
}

# Function to display weather data
display_weather_table <- function(weather_data_list) {
  cat(sprintf("Weather data for: %s, %s\n", weather_data_list[[1]]$name, weather_data_list[[1]]$sys$country))
  cat(sprintf("%-20s%-20s%-30s%-20s%-15s%-15s\n", "Time", "Condition", "Description", "Temperature (°C)", "Humidity (%)", "Wind Speed (m/s)"))
  for (data in weather_data_list) {
    cat(sprintf("%-20s%-20s%-30s%-20.2f%-15d%-15.2f\n", 
                data$time, 
                data$weather[[1]]$main, 
                data$weather[[1]]$description, 
                data$main$temp, 
                data$main$humidity, 
                data$wind$speed))
  }
}

city <- "Norman,US"

# Initialize an empty list to store weather data
weather_data_list <- list()

repeat {
  weather_data <- fetch_weather(city)
  if (!is.null(weather_data)) {
    # Add a timestamp to the weather data
    weather_data$time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    weather_data_list <- append(weather_data_list, list(weather_data))
    
    # Limit the list size to the last 5 entries
    if (length(weather_data_list) > 5) {
      weather_data_list <- tail(weather_data_list, 5)
    }
    
    display_weather_table(weather_data_list)
  } else {
    cat("Failed to retrieve weather data\n")
  }
  
  # Wait for 1 minute (60 seconds) before fetching the data again
  Sys.sleep(60)
}




