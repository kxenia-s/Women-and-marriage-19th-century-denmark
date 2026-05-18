# Load libraries
library(tidyverse)

# Set working directory
setwd("~/Desktop/DAM")

### Kvinder

## 1801

data_1801_young <- read_csv("census-1801-normalized.csv")

# Check columns
colnames(data_1801_young)

# Clean dataset 
clean_data_1801_young <- data_1801_young %>%
  filter(
    !is.na(alder),
    alder > 19,
    !is.na(alder),
    alder < 31,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != "mand"
  )

# Histogram of age by marital status
ggplot(clean_data_1801_young, aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

# Piechart of marital stats
clean_data_1801_young %>%
  count(civilstand) %>%
  ggplot(aes(x = "", y = n, fill = civilstand)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(n / sum(n) * 100, 1), "%")),
    position = position_stack(vjust = 0.5)
  ) +
  theme_void() +
  labs(
    title = "Marital Status Distribution",
    fill = "Marital Status"
  )

## 1834

data_1834_young <- read_csv("census-1834-normalized.csv")

# Check columns
colnames(data_1834_young)

# Clean dataset 
clean_data_1834_young <- data_1834_young %>%
  filter(
    !is.na(alder),
    alder > 19,
    !is.na(alder),
    alder < 31,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != "mand"
  )

# Histogram of age by marital status
ggplot(clean_data_1834_young, aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

# Piechart of marital stats
clean_data_1834_young %>%
  count(civilstand) %>%
  ggplot(aes(x = "", y = n, fill = civilstand)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(n / sum(n) * 100, 1), "%")),
    position = position_stack(vjust = 0.5)
  ) +
  theme_void() +
  labs(
    title = "Marital Status Distribution",
    fill = "Marital Status"
  )

## 1845

data_1845_young <- read_csv("census-1845-normalized.csv")

# Check columns
colnames(data_1845_young)

# Clean dataset
clean_data_1845_young <- data_1845_young %>%
  filter(
    !is.na(age),
    age > 19,
    !is.na(age),
    age < 31,
    !is.na(marital_status),
    marital_status != "",
    !is.na(gender),
    gender != "mand"
  )

# Histogram of age by marital status
ggplot(clean_data_1845_young, aes(x = age, fill = marital_status)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

# Piechart of marital stats
clean_data_1845_young %>%
  count(marital_status) %>%
  ggplot(aes(x = "", y = n, fill = marital_status)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(n / sum(n) * 100, 1), "%")),
    position = position_stack(vjust = 0.5)
  ) +
  theme_void() +
  labs(
    title = "Marital Status Distribution",
    fill = "Marital Status"
  )

## 1860

data_1860_young <- read_csv("census-1860-normalized.csv")

# Check columns
colnames(data_1860_young)

# Clean dataset 
clean_data_1860_young <- data_1860_young %>%
  filter(
    !is.na(alder),
    alder > 19,
    !is.na(alder),
    alder < 31,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != "mand"
  )

# Histogram of age by marital status
ggplot(clean_data_1860_young, aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

# Piechart of marital stats
clean_data_1860_young %>%
  count(civilstand) %>%
  ggplot(aes(x = "", y = n, fill = civilstand)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(n / sum(n) * 100, 1), "%")),
    position = position_stack(vjust = 0.5)
  ) +
  theme_void() +
  labs(
    title = "Marital Status Distribution",
    fill = "Marital Status"
  )

# ___________________________________

## 1801
  
data_1801_ext <- read_csv("census-1801-normalized.csv")

# Check columns
colnames(data_1801_ext)

# Clean dataset
clean_data_1801_ext <- data_1801_ext %>%
  filter(
    !is.na(alder),
    alder > 29,
    !is.na(alder),
    alder < 41,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != "mand"
  )

# Histogram of age by marital status
ggplot(clean_data_1801_ext, aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

# Piechart of marital stats
clean_data_1801_ext %>%
  count(civilstand) %>%
  ggplot(aes(x = "", y = n, fill = civilstand)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(n / sum(n) * 100, 1), "%")),
    position = position_stack(vjust = 0.5)
  ) +
  theme_void() +
  labs(
    title = "Marital Status Distribution",
    fill = "Marital Status"
  )

## 1834

data_1834_ext <- read_csv("census-1834-normalized.csv")

# Check columns
colnames(data_1834_ext)

# Clean dataset
clean_data_1834_ext <- data_1834_ext %>%
  filter(
    !is.na(alder),
    alder > 29,
    !is.na(alder),
    alder < 41,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != "mand"
  )

# Histogram of age by marital status
ggplot(clean_data_1834_ext, aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

# Piechart of marital stats
clean_data_1834_ext %>%
  count(civilstand) %>%
  ggplot(aes(x = "", y = n, fill = civilstand)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(n / sum(n) * 100, 1), "%")),
    position = position_stack(vjust = 0.5)
  ) +
  theme_void() +
  labs(
    title = "Marital Status Distribution",
    fill = "Marital Status"
  )

## 1845

data_1845_ext <- read_csv("census-1845-normalized.csv")

# Check columns
colnames(data_1845_ext)

# Clean dataset 
clean_data_1845_ext <- data_1845_ext %>%
  filter(
    !is.na(age),
    age > 29,
    !is.na(age),
    age < 41,
    !is.na(marital_status),
    marital_status != "",
    !is.na(gender),
    gender != "mand"
  )

# Histogram of age by marital status
ggplot(clean_data_1845_ext, aes(x = age, fill = marital_status)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

# Piechart of marital stats
clean_data_1845_ext %>%
  count(marital_status) %>%
  ggplot(aes(x = "", y = n, fill = marital_status)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(n / sum(n) * 100, 1), "%")),
    position = position_stack(vjust = 0.5)
  ) +
  theme_void() +
  labs(
    title = "Marital Status Distribution",
    fill = "Marital Status"
  )

## 1860

data_1860_ext <- read_csv("census-1860-normalized.csv")

# Check columns
colnames(data_1860_ext)

# Clean dataset
clean_data_1860_ext <- data_1860_ext %>%
  filter(
    !is.na(alder),
    alder > 29,
    !is.na(alder),
    alder < 41,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != "mand"
  )

# Histogram of age by marital status
ggplot(clean_data_1860_ext, aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

# Piechart of marital stats
clean_data_1860_ext %>%
  count(civilstand) %>%
  ggplot(aes(x = "", y = n, fill = civilstand)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(n / sum(n) * 100, 1), "%")),
    position = position_stack(vjust = 0.5)
  ) +
  theme_void() +
  labs(
    title = "Marital Status Distribution",
    fill = "Marital Status"
  )

# ___________________________________

## 1801

# Clean dataset 
clean_data_1801_kplus <- data_1801_ext %>%
  filter(
    !is.na(alder),
    alder > 19,
    !is.na(alder),
    alder < 41,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != "mand"
  )

# Histogram of age by marital status
ggplot(clean_data_1801_kplus, aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

## 1834

# Clean dataset 
clean_data_1834_kplus <- data_1834_ext %>%
  filter(
    !is.na(alder),
    alder > 19,
    !is.na(alder),
    alder < 41,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != "mand"
  )

# Histogram of age by marital status
ggplot(clean_data_1834_kplus, aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

## 1845

# Clean dataset 
clean_data_1845_kplus <- data_1845_ext %>%
  filter(
    !is.na(age),
    age > 19,
    !is.na(age),
    age < 41,
    !is.na(marital_status),
    marital_status != "",
    !is.na(gender),
    gender != "mand"
  )

# Histogram of age by marital status
ggplot(clean_data_1845_kplus, aes(x = age, fill = marital_status)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

## 1860

# Clean dataset 
clean_data_1860_kplus <- data_1860_ext %>%
  filter(
    !is.na(alder),
    alder > 19,
    !is.na(alder),
    alder < 41,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != "mand"
  )

# Histogram of age by marital status
ggplot(clean_data_1860_kplus, aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

# ___________________________________

### Mænd

## 1801

data_1801_ext <- read_csv("census-1801-normalized.csv")

# Check columns
colnames(data_1801_ext)

# Clean dataset 
clean_data_1801_mplus <- data_1801_ext %>%
  filter(
    !is.na(alder),
    alder > 19,
    !is.na(alder),
    alder < 41,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != "kvinde"
  )

# Histogram of age by marital status
ggplot(clean_data_1801_mplus, aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

## 1834

data_1834_ext <- read_csv("census-1834-normalized.csv")

# Check columns
colnames(data_1834_ext)

# Clean dataset
clean_data_1834_mplus <- data_1834_ext %>%
  filter(
    !is.na(alder),
    alder > 19,
    !is.na(alder),
    alder < 41,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != "kvinde"
  )

# Histogram of age by marital status
ggplot(clean_data_1834_mplus, aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

## 1845

data_1845_ext <- read_csv("census-1845-normalized.csv")

# Check columns
colnames(data_1845_ext)

# Clean dataset 
clean_data_1845_mplus <- data_1845_ext %>%
  filter(
    !is.na(age),
    age > 19,
    !is.na(age),
    age < 41,
    !is.na(marital_status),
    marital_status != "",
    !is.na(gender),
    gender != "kvinde"
  )

# Histogram of age by marital status
ggplot(clean_data_1845_mplus, aes(x = age, fill = marital_status)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

## 1860

data_1860_ext <- read_csv("census-1860-normalized.csv")

# Check columns
colnames(data_1860_ext)

# Clean dataset 
clean_data_1860_mplus <- data_1860_ext %>%
  filter(
    !is.na(alder),
    alder > 19,
    !is.na(alder),
    alder < 41,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != "kvinde"
  )

# Histogram of age by marital status
ggplot(clean_data_1860_mplus, aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  labs(
    title = "Age and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

# ___________________________________

### Mænd & kvinder

## 1801

# Clean dataset with BOTH genders
clean_data_1801_extplus <- data_1801_ext %>%
  filter(
    !is.na(alder),
    alder > 19,
    alder < 41,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != ""
  )

# Graph
ggplot(clean_data_1801_extplus,
       aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "stack") +
  facet_wrap(~ koen) +
  labs(
    title = "Age, Gender and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

## 1834

# Clean dataset with BOTH genders
clean_data_1834_extplus <- data_1834_ext %>%
  filter(
    !is.na(alder),
    alder > 19,
    alder < 41,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != ""
  )

# Graph
ggplot(clean_data_1834_extplus,
       aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "stack") +
  facet_wrap(~ koen) +
  labs(
    title = "Age, Gender and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

## 1845

# Clean dataset with BOTH genders
clean_data_1845_extplus <- data_1845_ext %>%
  filter(
    !is.na(age),
    age > 19,
    age < 41,
    !is.na(marital_status),
    marital_status != "",
    !is.na(gender),
    gender != ""
  )

# Graph
ggplot(clean_data_1845_extplus,
       aes(x = age, fill = marital_status)) +
  geom_histogram(binwidth = 2, position = "stack") +
  facet_wrap(~ gender) +
  labs(
    title = "Age, Gender and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()

## 1860

# Clean dataset with BOTH genders
clean_data_1860_extplus <- data_1860_ext %>%
  filter(
    !is.na(alder),
    alder > 19,
    alder < 41,
    !is.na(civilstand),
    civilstand != "",
    !is.na(koen),
    koen != ""
  )

# Graph
ggplot(clean_data_1860_extplus,
       aes(x = alder, fill = civilstand)) +
  geom_histogram(binwidth = 2, position = "stack") +
  facet_wrap(~ koen) +
  labs(
    title = "Age, Gender and Marital Status",
    x = "Age",
    y = "Count",
    fill = "Marital Status"
  ) +
  theme_minimal()
