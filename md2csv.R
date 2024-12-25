# Load necessary library
library(readr)

# Define the path to the Markdown file
md_file <- "/Users/guhl/Documents/GitHub/pinghook/pinghook.md"

# Read the Markdown file
md_content <- readLines(md_file)

# Extract links and descriptions using a regular expression
links <- regmatches(md_content, gregexpr("\\[([^\\]]+)\\]\\(([^\\)]+)\\)", md_content))

# Flatten the list and remove empty elements
links <- unlist(links)
links <- links[links != ""]

# Create a data frame from the extracted links
link_data <- data.frame(
  Description = gsub("\\[([^\\]]+)\\]\\(([^\\)]+)\\)", "\\1", links),
  URL = gsub("\\[([^\\]]+)\\]\\(([^\\)]+)\\)", "\\2", links),
  stringsAsFactors = FALSE
)

# Define the path to the CSV file
csv_file <- "/Users/guhl/Documents/GitHub/pinghook/links.csv"

# Write the data frame to a CSV file
write_csv(link_data, csv_file)

# Print the data frame
print(link_data)