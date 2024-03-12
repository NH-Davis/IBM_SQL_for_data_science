# Load Data Sets

census_df <- read.csv("C:\\Users\\nhart\\OneDrive\\Desktop\\IBM\\SQL for Data Science\\data\\ChicagoCensusData.csv")
crime_df <- read.csv("C:\\Users\\nhart\\OneDrive\\Desktop\\IBM\\SQL for Data Science\\data\\ChicagoCrimeData.csv")
school_df <- read.csv("C:\\Users\\nhart\\OneDrive\\Desktop\\IBM\\SQL for Data Science\\data\\ChicagoPublicSchools.csv")

# 1: Find the total number of crimes recorded in the Crime Table

# Load necessary library
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Dataframe: crime_df
# Column of interest: PRIMARY_TYPE

# Extract and print all unique categories within the PRIMARY_TYPE column
unique_categories <- unique(crime_df$PRIMARY_TYPE)
print(unique_categories)

# Method 1: Calculate the count of all instances within each category using table()
category_counts_table <- table(crime_df$PRIMARY_TYPE)
print(category_counts_table)

# Method 2: Calculate the count of all instances within each category using dplyr
category_counts_dplyr <- crime_df %>%
  group_by(PRIMARY_TYPE) %>%
  summarise(count = n())

print(category_counts_dplyr)

# Calculate and print the total sum across all categories
# This can be done directly by summing the 'count' column from the dplyr result
# Or by using nrow() to count all rows in the dataframe
total_count_dplyr <- sum(category_counts_dplyr$count)
total_count_nrow <- nrow(crime_df)

print(paste("Total count using dplyr: ", total_count_dplyr))
print(paste("Total count using nrow(): ", total_count_nrow))

# 2: List community area names and numbers with per capita income less than 11000

# Load necessary library
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Dataframe: census_df
# Columns of interest: COMMUNITY_AREA_NAME and PER_CAPITA_INCOME

# Filter communities with PER_CAPITA_INCOME of 11000 or less
low_income_communities <- census_df %>%
  filter(PER_CAPITA_INCOME <= 11000) %>%
  select(COMMUNITY_AREA_NAME)

# Print the list of community names
print(low_income_communities)

#3: : List all case numbers for crimes involving minors?(children are not considered minors for the purposes of crime analysis)

# Load necessary library
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Dataframe: crime_df
# Columns of interest: CASE_NUMBER and PRIMARY_TYPE

# Filter for minor crimes and select the case numbers
minorcrime_case_numbers <- crime_df %>%
  filter(PRIMARY_TYPE == "OFFENSE INVOLVING CHILDREN") %>%
  select(CASE_NUMBER)

# Print the case numbers for kidnapping crimes
print(minorcrime_case_numbers)



#4: List all kidnapping crimes involving a child?

# Load necessary library
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Dataframe: crime_df
# Columns of interest: CASE_NUMBER and PRIMARY_TYPE

# Filter for kidnapping crimes and select the case numbers
kidnapping_case_numbers <- crime_df %>%
  filter(PRIMARY_TYPE == "KIDNAPPING") %>%
  select(CASE_NUMBER)

# Print the case numbers for kidnapping crimes
print(kidnapping_case_numbers)

# 5: List the kind of crimes that were recorded at schools. (No repetitions)

# Load necessary library
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Dataframe: crime_df
# Columns of interest: PRIMARY_TYPE and LOCATION_DESCRIPTION

# Filter rows where LOCATION_DESCRIPTION contains "school" (case-insensitive)
# and select unique instances of PRIMARY_TYPE
unique_primary_types_school <- crime_df %>%
  filter(grepl("school", LOCATION_DESCRIPTION, ignore.case = TRUE)) %>%
  select(PRIMARY_TYPE) %>%
  distinct()

# Print the unique PRIMARY_TYPE instances related to "school" locations
print(unique_primary_types_school)

#6: List the type of schools along with the average safety score for each type

# Load necessary library
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Dataframe: school_df
# Columns of interest: Elementary..Middle..or.High.School and SAFETY_SCORE

# Calculate the average safety score by school category
average_safety_scores <- school_df %>%
  group_by(Elementary..Middle..or.High.School) %>%
  summarise(Average_Safety_Score = mean(SAFETY_SCORE, na.rm = TRUE))

# Print the average safety scores by school category
print(average_safety_scores)

#7: List 5 community areas with highest % of households below poverty line

# Load necessary library
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Dataframe: census_df
# Columns of interest: COMMUNITY_AREA_NAME and PERCENT_HOUSEHOLDS_BELOW_POVERTY

# List the 5 community areas with the highest % of households below poverty line
top_5_poverty_areas <- census_df %>%
  arrange(desc(PERCENT_HOUSEHOLDS_BELOW_POVERTY)) %>%
  select(COMMUNITY_AREA_NAME, PERCENT_HOUSEHOLDS_BELOW_POVERTY) %>%
  slice_head(n = 5)

# Print the top 5 community areas
print(top_5_poverty_areas)

#8: Which community area is most crime prone? Display the community area number only.

# Load necessary library
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Dataframe: crime_df
# Columns of interest: COMMUNITY_AREA_NUMBER and PRIMARY_TYPE

# Determine the most crime-prone community area by total count of crimes
most_crime_prone_area <- crime_df %>%
  group_by(COMMUNITY_AREA_NUMBER) %>%
  summarise(Total_Crimes = n()) %>%
  arrange(desc(Total_Crimes)) %>%
  slice_head(n = 1)

# Print the most crime-prone community area
print(most_crime_prone_area)


#9: Use a sub-query to find the name of the community area with highest hardship index

# Load necessary library
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Dataframe: census_df
# Columns of interest: COMMUNITY_AREA_NAME and HARDSHIP_INDEX

# Identify the community area with the highest hardship index
community_highest_hardship <- census_df %>%
  arrange(desc(HARDSHIP_INDEX)) %>%
  select(COMMUNITY_AREA_NAME, HARDSHIP_INDEX) %>%
  slice_head(n = 1)

# Print the community area with the highest hardship index
print(community_highest_hardship)


#10: Use a sub-query to determine the Community Area Name with most number of crimes?

# Load necessary library
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Step 1: Join crime_df with census_df to map community area names to numbers
crime_with_names_df <- crime_df %>%
  inner_join(census_df, by = "COMMUNITY_AREA_NUMBER")

# Step 2 & 3: Group by COMMUNITY_AREA_NAME and summarise to get total crimes
total_crimes_by_area <- crime_with_names_df %>%
  group_by(COMMUNITY_AREA_NAME) %>%
  summarise(Total_Crimes = n())

# Step 4: Arrange in descending order of Total_Crimes and get the top area
most_crime_prone_area <- total_crimes_by_area %>%
  arrange(desc(Total_Crimes)) %>%
  slice_head(n = 1)

# Print the community area name with the highest number of crimes
print(most_crime_prone_area)
