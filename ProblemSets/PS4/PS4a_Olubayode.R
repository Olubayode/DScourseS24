# R command to download the file
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')
# Print the content of dates.json
system('cat dates.json')
library(jsonlite)
library(dplyr)


# Load the JSON data into a list and then to a data frame
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# Check the types
print(class(mydf))
print(class(mydf$date))
# List the first n rows
n <- 5
print(head(mydf, n))
