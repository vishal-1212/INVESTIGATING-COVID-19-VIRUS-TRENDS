# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(httr)

# Step 1: Download COVID-19 data from a reliable source
# Example: Johns Hopkins University COVID-19 data repository
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
temp_file <- tempfile()
download.file(url, temp_file)

# Step 2: Load the data
covid_data <- read.csv(temp_file)
unlink(temp_file)  # Clean up temporary file

# Step 3: Data preprocessing
covid_long <- covid_data %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Date",
               values_to = "ConfirmedCases") %>%
  mutate(Date = mdy(gsub("X", "", Date))) %>%
  group_by(Country.Region, Date) %>%
  summarize(TotalCases = sum(ConfirmedCases, na.rm = TRUE), .groups = "drop")

# Step 4: Analyze trends for a specific country (e.g., India)
country_trends <- covid_long %>%
  filter(Country.Region == "India") %>%
  arrange(Date)

# Step 5: Visualize the trends
ggplot(country_trends, aes(x = Date, y = TotalCases)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "COVID-19 Trends in India",
       x = "Date",
       y = "Total Confirmed Cases",
       caption = "Source: Johns Hopkins University") +
  theme_minimal()

# Step 6: (Optional) Compare trends across countries
countries_to_compare <- c("India", "United States", "Brazil")
comparison_trends <- covid_long %>%
  filter(Country.Region %in% countries_to_compare) %>%
  arrange(Date)

ggplot(comparison_trends, aes(x = Date, y = TotalCases, color = Country.Region)) +
  geom_line(size = 1) +
  labs(title = "COVID-19 Trends Across Countries",
       x = "Date",
       y = "Total Confirmed Cases",
       caption = "Source: Johns Hopkins University") +
  theme_minimal() +
  scale_color_manual(values = c("India" = "blue", "United States" = "red", "Brazil" = "green"))
