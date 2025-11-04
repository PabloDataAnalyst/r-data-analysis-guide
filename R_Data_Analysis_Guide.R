# ================================================================
# 📘 GUIDE TO ANALYZE DATA WITH R + FUNCTIONS
# Author: Pablo
# Last updated: 04/11/2025
# Purpose: Practic guide for data analysis in R (tidyverse)
# ================================================================

## ================================================================
## 1. USING R PACKAGES
## ================================================================
# install.packages() → Install packages in R
install.packages("tidyverse")

# library() → Load packages
library(tidyverse)


## ================================================================
## 2. VIEWING DATA
## ================================================================
# read_...() → Import data files
hotel <- read_csv(".../datasets/csv/hotel_bookings.csv")

# skim_without_charts() → Detailed data summary without plots
skim_without_charts(penguins)

# head() → Show first rows of a dataset
head(diamonds)

# View() → Open dataset in a new tab
View(diamonds)

# str() or glimpse() → Display dataset structure
str(diamonds)
glimpse(diamonds)

# colnames() → Return column names
colnames(diamonds)

# slice_sample() → Extract random sample with n rows
diamonds_sample <- diamonds |>
  slice_sample(n = 100)


## ================================================================
## 3. ORGANIZING DATA
## ================================================================
# arrange() → Sort data (ascending / descending)
arrange(penguins, bill_length_mm)
arrange(penguins, -bill_length_mm)
penguins |> arrange(bill_length_mm)

# filter() → Filter rows based on conditions
filter(penguins, species == "Adelie")

# pivot_wider() → Convert from long to wide format
wide_table <- employee |>
  pivot_wider(
    names_from = job_title,
    values_from = first_name
  )

# pivot_longer() → Convert from wide to long format
long_table <- wide_table |>
  pivot_longer(
    cols = c(Professional, Programmer, Management, Clerical, Developer),
    names_to = "job_title",
    values_to = "first_name",
    values_drop_na = TRUE
  )


## ================================================================
## 4. CLEANING DATA
## ================================================================
# select() → Select specific columns
select(penguins, species)
select(penguins, -species)
penguins |> select(species)

# drop_na() → Remove rows with missing values
drop_na(penguins)

# trimws() → Remove whitespace from text
light_power_cars$model <- trimws(light_power_cars$model)

# rename() → Rename columns
rename(diamonds, carat_new = carat)
rename(diamonds, carat_new = carat, cut_new = cut)

# rename_with() → Change column name format
rename_with(diamonds, toupper)
rename_with(diamonds, tolower)

# clean_names() → Ensure valid column names
clean_names(penguins)

# bias() → Check bias between actual and predicted data
actual_sales <- c(150, 203, 137, 247, 116, 287)
predicted_sales <- c(200, 300, 150, 250, 150, 300)
bias(actual_sales, predicted_sales) # -35 → clear bias


## ================================================================
## 5. MANIPULATING DATA
## ================================================================
# unite() → Combine multiple columns into one
hotel |> unite(arrival_month_year, c("arrival_date_month", "arrival_date_year"), sep = " ")
employee |> unite(name, first_name, last_name, sep = " ")

# separate() → Split one column into multiple ones
employee |> separate(name, into = c("first_name", "last_name"), sep = " ")

# mutate() → Create or modify columns
diamonds |> mutate(carat_2 = carat * 100)

# group_by() + summarise() → Calculate summaries by group
penguins |> 
  group_by(island) |> 
  drop_na() |> 
  summarise(mean_bill_length_mm = mean(bill_length_mm))

# Multiple metrics + filtering
penguins |> 
  group_by(island, species) |> 
  drop_na() |> 
  summarise(
    min_bl = min(bill_length_mm),
    max_bl = max(bill_length_mm),
    mean_bl = mean(bill_length_mm)
  ) |> 
  filter(species == "Adelie")

# sum(), sd(), cor(), mean()
quartet |> summarise(sd(x))
quartet |> summarise(cor(x, y))
mean(hotel$lead_time)
hotel |> summarise(mean(lead_time))

# Basic summary example
hotel_summary <- hotel |>
  group_by(hotel) |>
  summarise(
    average_lead_time = mean(lead_time),
    min_lead_time = min(lead_time),
    max_lead_time = max(lead_time)
  )


## ================================================================
## 6. VISUALIZING DATA
## ================================================================
# ggplot() → Create data visualizations
ggplot(diamonds, aes(carat, price, color = cut)) + geom_point()

# facet_wrap() and facet_grid() → Multiple plots by variable(s)
ggplot(diamonds, aes(carat, price, color = cut)) + facet_wrap(~cut)
ggplot(penguins, aes(x = flipper_len, y = body_mass, color = species)) +
  geom_point() +
  facet_grid(sex ~ species)

# reorder(), labs(), annotate(), paste0(), theme()
light_power_cars |>
  ggplot() +
  geom_col(aes(x = reorder(Model, -HorsePower), HorsePower)) +
  labs(
    title = "Cars under 1200kg",
    x = "Car Model",
    y = "Horsepower (HP)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 15, face = "bold")
  )

# combined_plot() → Combine several plots
combined_plot <- (plot1 + plot2) / (plot3 + plot4)

# geom_bar() → Automatic count per category
diamonds |> ggplot(aes(cut)) + geom_bar()

# scale_fill_manual() → Custom color scales
scale_fill_manual(values = c("#1F77B4", "#2CA02C", "#FF7F0E", "#9467BD", "#7F7F7F"))

# ggsave() → Save last plot
setwd(".../R/plots")
ggsave("Cars_dashboard.png")


## ================================================================
## 7. DOCUMENTATION
## ================================================================
# Document your analysis process in Markdown or R Notebooks
# Combine text + code + plots for reproducibility


## ================================================================
## APPENDIX
## ================================================================
# Save intermediate results as new objects
# Example: new_dataframe <- original_dataframe |> filter(...) |> arrange(...)
