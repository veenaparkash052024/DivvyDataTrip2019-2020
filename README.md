---
title: "Divvy Trip Data_2019_2020"
author: "Veena_Capstone Project_DivvyTrip"
date: "2024-11-22"
output: html_document
---

Load the necessary Libraries:
```{r}
library(readr)
library(tidyverse)
library(writexl)
```

Load the data sets:
```{r}
Divvy_Trips_2020_Q1 <- read_csv("C:/Users/VP/Desktop/Divvy Trip_2019_2020/Divvy_Trips_2020_Q1.csv")
Divvy_Trips_2019_Q1 <- read_csv("C:/Users/VP/Desktop/Divvy Trip_2019_2020/Divvy_Trips_2019_Q1.csv")

head(Divvy_Trips_2020_Q1)
head(Divvy_Trips_2019_Q1)

colnames(Divvy_Trips_2020_Q1)
colnames(Divvy_Trips_2019_Q1)


colnames(Divvy_Trips_2020_Q1)
colnames(Divvy_Trips_2019_Q1)
```

Step 2: Make columns consistent and merge them into a single dataframe.

```{r}
# Rename columns in Divvy_Trips_2019_Q1 to match Divvy_Trips_2020_Q1
Divvy_Trips_2019_Q1 <- Divvy_Trips_2019_Q1 %>%
  rename(
    ride_id = trip_id,
    started_at = start_time,
    ended_at = end_time,
    rideable_type = bikeid,
    start_station_id = from_station_id,
    start_station_name = from_station_name,
    end_station_id = to_station_id,
    end_station_name = to_station_name,
    member_casual = usertype
  ) %>%
  mutate(
    # Add missing columns with NA to match Divvy_Trips_2020_Q1
    start_lat = NA,
    start_lng = NA,
    end_lat = NA,
    end_lng = NA,
    # Ensure rideable_type is a character, assigning "unknown" as a placeholder
    rideable_type = as.character("unknown"),
    # Convert ride_id to character to ensure compatibility
    ride_id = as.character(ride_id)
  )

# Ensure consistent data types for Divvy_Trips_2020_Q1 columns
Divvy_Trips_2020_Q1 <- Divvy_Trips_2020_Q1 %>%
  mutate(
    rideable_type = as.character(rideable_type),
    ride_id = as.character(ride_id)  # Ensure ride_id is character
  )

# Combine the two datasets
Divvy_Trips_Combined <- bind_rows(Divvy_Trips_2019_Q1, Divvy_Trips_2020_Q1)

# Preview combined data
head(Divvy_Trips_Combined)
```

Interpretation: In this step, column names and data types are standardized to ensure consistency between the 2019 and 2020 datasets. Missing columns in the 2019 dataset are filled with NA, and mismatched data types (e.g., rideable_type and ride_id) are resolved. The datasets are then merged into a single, unified dataframe.


Step 3: Clean up and add data to prepare for analysis.
```{r}
# I. Remove unnecessary columns (if applicable)
Divvy_Trips_Cleaned <- Divvy_Trips_Combined %>%
  select(
    ride_id, rideable_type, started_at, ended_at, start_station_name,
    end_station_name, member_casual, start_lat, start_lng, end_lat, end_lng
  )

# II. Handle missing values
# Remove rows with missing start or end station names (critical for analysis)
Divvy_Trips_Cleaned <- Divvy_Trips_Cleaned %>%
  filter(!is.na(start_station_name) & !is.na(end_station_name))

# III. Standardize and clean categorical variables
# Ensure consistent labels for `member_casual`
Divvy_Trips_Cleaned <- Divvy_Trips_Cleaned %>%
  mutate(
    member_casual = case_when(
      member_casual %in% c("Subscriber", "member") ~ "member",
      member_casual %in% c("Customer", "casual") ~ "casual",
      TRUE ~ member_casual
    )
  )

# IV. Add derived columns
Divvy_Trips_Cleaned <- Divvy_Trips_Cleaned %>%
  mutate(
    trip_duration = as.numeric(difftime(ended_at, started_at, units = "mins")), # Calculate trip duration in minutes
    day_of_week = weekdays(started_at),  # Extract day of the week
    month = format(started_at, "%B")  # Extract month
  )

# V. Filter invalid or outlier data
Divvy_Trips_Cleaned <- Divvy_Trips_Cleaned %>%
  filter(
    trip_duration > 0 & trip_duration <= 1440  # Keep trips between 1 minute and 24 hours
  )

# Preview the cleaned and prepared dataset
head(Divvy_Trips_Cleaned)

# Summary of the cleaned dataset
summary(Divvy_Trips_Cleaned)

```

Interpretation:
In Step 3, the data is cleaned and prepared for analysis by addressing several aspects. First, unnecessary columns are removed to retain only relevant variables. Missing values in critical columns, such as station names, are handled by filtering out rows with missing data. Categorical variables, like member_casual, are standardized to ensure consistency in labeling. Derived columns are added, including trip_duration (calculated in minutes), day_of_week, and month to enhance the dataset’s usefulness. Outliers or invalid entries, such as trips with durations outside a reasonable range, are filtered out. The cleaned dataset is now ready for analysis.

Step 4: Conduct descriptive analysis.
```{r}
# I. Summary Statistics
# Summary for numeric columns (e.g., trip_duration)
summary(Divvy_Trips_Cleaned$trip_duration)

# Count the number of trips for each member type
table(Divvy_Trips_Cleaned$member_casual)

# Count the number of trips by rideable type
table(Divvy_Trips_Cleaned$rideable_type)

# II. Trip Duration Analysis
# Mean and median trip duration
mean_trip_duration <- mean(Divvy_Trips_Cleaned$trip_duration, na.rm = TRUE)
median_trip_duration <- median(Divvy_Trips_Cleaned$trip_duration, na.rm = TRUE)

cat("Mean Trip Duration:", mean_trip_duration, "minutes\n")
cat("Median Trip Duration:", median_trip_duration, "minutes\n")

# III. Visualizations
# a. Distribution of Trip Duration
ggplot(Divvy_Trips_Cleaned, aes(x = trip_duration)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Trip Duration",
    x = "Trip Duration (minutes)",
    y = "Frequency"
  ) +
  xlim(0, 100) +  # Focus on trips up to 100 minutes for clarity
  theme_minimal()

# b. Trips by Day of the Week
ggplot(Divvy_Trips_Cleaned, aes(x = day_of_week, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Number of Trips by Day of the Week",
    x = "Day of the Week",
    y = "Number of Trips",
    fill = "Member Type"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "orange")) +
  scale_x_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# c. Trips by Month
ggplot(Divvy_Trips_Cleaned, aes(x = month, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Number of Trips by Month",
    x = "Month",
    y = "Number of Trips",
    fill = "Member Type"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "orange")) +
  scale_x_discrete(limits = month.name)

# d. Rideable Type Usage
ggplot(Divvy_Trips_Cleaned, aes(x = rideable_type, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Usage of Rideable Types",
    x = "Rideable Type",
    y = "Number of Trips",
    fill = "Member Type"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "orange"))
```

Interpretation:
In Step 4, descriptive analysis is conducted on the cleaned dataset to understand key trends and patterns. Summary statistics for trip durations reveal a wide range, with a mean of 13.66 minutes and a median of 8.95 minutes. The distribution of trip duration is visualized using a histogram, focusing on trips up to 100 minutes. The analysis also explores trip patterns by day of the week and month, revealing the distribution of trips across these variables and comparing member types. Additionally, the usage of different rideable types is visualized to understand how trip patterns vary across ride options.

Step 5: Export a summary file for further analysis.

```{r}
# Create a summary dataframe for export
summary_data <- data.frame(
  Metric = c(
    "Total Trips",
    "Mean Trip Duration (minutes)",
    "Median Trip Duration (minutes)",
    "Most Common Day of the Week",
    "Most Common Rideable Type",
    "Member Trips",
    "Casual Trips"
  ),
  Value = c(
    nrow(Divvy_Trips_Cleaned),
    mean(Divvy_Trips_Cleaned$trip_duration, na.rm = TRUE),
    median(Divvy_Trips_Cleaned$trip_duration, na.rm = TRUE),
    names(sort(table(Divvy_Trips_Cleaned$day_of_week), decreasing = TRUE)[1]),
    names(sort(table(Divvy_Trips_Cleaned$rideable_type), decreasing = TRUE)[1]),
    sum(Divvy_Trips_Cleaned$member_casual == "member", na.rm = TRUE),
    sum(Divvy_Trips_Cleaned$member_casual == "casual", na.rm = TRUE)
  )
)

# Write the summary to a CSV file
write.csv(summary_data, "Divvy_Trips_Summary.csv", row.names = FALSE)

# Alternatively, write the summary to an Excel file
write_xlsx(summary_data, "Divvy_Trips_Summary.xlsx")

# Confirm the export
cat("Summary files exported: Divvy_Trips_Summary.csv and Divvy_Trips_Summary.xlsx\n")
```
