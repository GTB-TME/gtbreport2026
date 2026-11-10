
### Load last est_inc dataset available for mortality script


library(data.table)
library(stringr)
library(here)

# Define the directory where your files are located
data_dir <- here('inc_mort/analysis/csv/')

# List all files in the directory that match the pattern "est_inc_YYYY-MM-DD.csv"
file_list <- list.files(
  path = data_dir,
  pattern = "est_inc_\\d{4}-\\d{2}-\\d{2}\\.csv$",
  full.names = TRUE # Get the full path to the files
)

# Extract dates from the filenames
# We'll use a regular expression to find the YYYY-MM-DD part
dates_in_filenames <- str_extract(file_list, "\\d{4}-\\d{2}-\\d{2}")

# Convert extracted dates to Date objects for proper comparison
dates_converted <- as.Date(dates_in_filenames)

# Find the index of the latest date
latest_file_index <- which.max(dates_converted)

# Get the full path of the latest file
latest_file <- file_list[latest_file_index]

# Load the latest file
est <- fread(latest_file)

# Now 'est' contains the data from the latest file
print(paste("Successfully loaded:", latest_file))
