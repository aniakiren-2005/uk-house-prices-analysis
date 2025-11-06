install.packages("data.table")
install.packages("data.table")
install.packages("arrow")
library(data.table)
library(arrow)
house_data <- fread("uk_house_prices.csv")
house_data <- fread("pp-complete.csv")
sample_data <- house_data[sample(.N, 10000)]  # 10,000 random rows
dim(sample_data)     # rows, columns
names(sample_data)   # column names
head(sample_data)    # first 6 rows
# take 1% of random sample because its huge
set.seed(123)
sample_data2 <- house_data[sample(.N, .N * 0.01)]
# simple regression: price ~ year
model <- lm(Price ~ as.numeric(format(Date_of_Transfer, "%Y")), data = sample_data2)
# price = v2 in data
model <- lm(v2 ~ as.numeric(format(Date_of_Transfer, "%Y")), data = sample_data2)
names(sample_data2)
setnames(house_data, old = c("v2", "v3"), new = c("price", "date"))
names(sample_data2)
setnames(
house_data,
old = names(house_data),
new = c("id", "price", "date", "postcode", "property_type", "old_new",
"duration", "paon", "saon", "street", "locality", "town_city",
"district", "county", "ppd_category_type", "record_status")
)
setnames(
sample_data2,
old = names(sample_data2),
new = c("id", "price", "date", "postcode", "property_type", "old_new",
"duration", "paon", "saon", "street", "locality", "town_city",
"district", "county", "ppd_category_type", "record_status")
)
names(sample_data2)
head(sample_data2)
sample_data2[, date := as.Date(date)]
sample_data2[, price := as.numeric(price)]
model <- lm(price ~ as.numeric(format(date, "%Y")), data = sample_data2)
summary(model)
install.packages("ggplot2")
library(ggplot2)
# Remove any missing or invalid price/date rows
sample_data2 <- sample_data2[!is.na(price) & !is.na(date)]
# Extract year for analysis
sample_data2[, year := as.numeric(format(date, "%Y"))]
# To visualise the results
# Compute yearly averages for visualisation
avg_by_year <- sample_data2[, .(mean_price = mean(price, na.rm = TRUE)), by = year]
ggplot(avg_by_year, aes(x = year, y = mean_price)) +
geom_point(alpha = 0.5) +
geom_smooth(method = "lm", se = TRUE, color = "blue") +
labs(
title = "Average UK House Prices Over Time",
x = "Year of Sale",
y = "Average Price (£)"
) +
theme_minimal(base_size = 14)
# Save the model summary to a text file
sink("model_summary.txt")
summary(model)
sink()
# Save linear model
saveRDS(model, "linear_model.rds")
savehistory("C:/Projects/house_prices/history.r")
