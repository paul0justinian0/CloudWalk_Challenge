# Importing the data ----

payment_data <- read.csv("Operations_analyst_data.csv")

# First look at the data ----

head(payment_data)
summary(payment_data)
str(payment_data)

# Necessary adjustments:
# 'day' from chr to date
# 'entity' from chr to factor
# 'product' from chr to factor
# 'price_tier" from chr to factor
# 'anticipation_method' from chr to factor
# 'payment_method' from chr to factor
# 'installments' from chr to factor

# Making the necessary adjustments ----
library(tidyverse)
library(lubridate) # You'll need this library for week() function

payment_data <- payment_data %>%
    mutate(
        day = as_date(day),
        entity = as_factor(entity),
        product = as_factor(product),
        price_tier = as_factor(price_tier),
        anticipation_method = as_factor(anticipation_method),
        payment_method = as_factor(payment_method),
        installments = as_factor(installments)
    )

## Verifying the database ----
str(payment_data)
summary(payment_data)

# Making the first visualizations ----

## TPV per entity ----

payment_data %>%
    group_by(entity) %>%
    summarise(tpv = sum(amount_transacted) / 1000000000) %>%
    ggplot(aes(x = entity, y = tpv)) +
    geom_bar(stat = "identity") +
    labs(title = "Total TPV per Entity") +
    xlab("Entity") +
    ylab("TPV (Billions)") +
    theme_bw()

## TPV per product ----

payment_data %>%
    group_by(product) %>%
    summarise(tpv = sum(amount_transacted) / 1000000000) %>%
    ggplot(aes(x = reorder(product, -tpv), y = tpv)) +
    geom_bar(stat = "identity") +
    labs(title = "Total TPV per Product") +
    xlab("Product") +
    ylab("TPV (Billions)") +
    theme_bw()

## TPV per payment_method ----

payment_data %>%
    group_by(payment_method) %>%
    summarise(tpv = sum(amount_transacted) / 1000000000) %>%
    ggplot(aes(x = reorder(payment_method, -tpv), y = tpv)) +
    geom_bar(stat = "identity") +
    labs(title = "Total TPV per Payment Method") +
    xlab("Payment Method") +
    ylab("TPV (Billions)") +
    theme_bw()

## AVG_Ticket per entity ----

payment_data %>%
    group_by(entity) %>%
    summarise(avg_ticket = sum(amount_transacted) / sum(quantity_transactions)) %>%
    ggplot(aes(x = reorder(entity, -avg_ticket), y = avg_ticket)) +
    geom_bar(stat = "identity") +
    labs(title = "Average Ticket size per Entity") +
    xlab("Entity") +
    ylab("Average Ticket Size") +
    theme_bw()

## AVG_Ticket per product ----

payment_data %>%
    group_by(product) %>%
    summarise(avg_ticket = sum(amount_transacted) / sum(quantity_transactions)) %>%
    ggplot(aes(x = reorder(product, -avg_ticket), y = avg_ticket)) +
    geom_bar(stat = "identity") +
    labs(title = "Average Ticket size per Product") +
    xlab("Product") +
    ylab("Average Ticket Size") +
    theme_bw()

## AVG_Ticket per payment_method ----

payment_data %>%
    group_by(payment_method) %>%
    summarise(avg_ticket = sum(amount_transacted) / sum(quantity_transactions)) %>%
    ggplot(aes(x = reorder(payment_method, -avg_ticket), y = avg_ticket)) +
    geom_bar(stat = "identity") +
    labs(title = "Average Ticket size per Payment Method") +
    xlab("Payment Method") +
    ylab("Average Ticket Size") +
    theme_bw()

## Transaction Volume per installments ----

payment_data %>%
    group_by(installments) %>%
    summarise(total_transactions = sum(quantity_transactions)) %>%
    ggplot(aes(x = sort(installments), y = total_transactions)) +
    geom_bar(stat = "identity") +
    labs(title = "Transaction Volume by Number of Installments") +
    xlab("Number of Installments") +
    ylab("Total Transaction Volume") +
    theme_bw()

## Transaction Volume per price_tier ----

payment_data %>%
    group_by(price_tier) %>%
    summarise(total_transactions = sum(quantity_transactions)) %>%
    ggplot(aes(x = reorder(price_tier, -total_transactions), y = total_transactions)) +
    geom_bar(stat = "identity") +
    labs(title = "Transaction Volume by Price Tier") +
    xlab("Price Tier") +
    ylab("Total Transaction Volume") +
    theme_bw()

## TPV per month ----

payment_data %>%
    mutate(m = month(day, label = TRUE)) %>%
    group_by(m) %>%
    summarise(tpv = sum(amount_transacted) / 1000000) %>%
    ggplot(aes(x = m, y = tpv)) +
    geom_bar(stat = "identity") +
    labs(title = "Total Payment Volume (TPV) by Month") +
    xlab("Month") +
    ylab("TPV (Millions)") +
    theme_bw()

## TPV per weekday ----

payment_data %>%
    mutate(weekday = wday(day, label = TRUE)) %>%
    group_by(weekday) %>%
    summarise(tpv = sum(amount_transacted) / 1000000) %>%
    ggplot(aes(x = weekday, y = tpv)) +
    geom_bar(stat = "identity") +
    labs(title = "Total Payment Volume (TPV) by Day of the Week") +
    xlab("Day of the Week") +
    ylab("TPV (Millions)") +
    theme_bw()

## Anticipation Method per Entity ----

payment_data %>%
    ggplot(aes(x = entity, fill = anticipation_method)) +
    geom_bar(position = "fill") +
    labs(title = "Anticipation Method Usage by Entity") +
    xlab("Entity") +
    ylab("Proportion") +
    theme_bw() +
    scale_fill_brewer(palette = "Paired")


# New visualizations for evolution over time ----

## TPV evolution by month (Line Plot) ----

payment_data %>%
    mutate(month_year = floor_date(day, "month")) %>%
    group_by(month_year) %>%
    summarise(tpv = sum(amount_transacted) / 1000000) %>%
    ggplot(aes(x = month_year, y = tpv)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "steelblue") +
    labs(title = "TPV Evolution by Month",
         subtitle = "Trends over time",
         x = "Date",
         y = "TPV (Millions)") +
    theme_bw() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y")

## TPV evolution by week (Line Plot) ----

payment_data %>%
    mutate(week_date = floor_date(day, "week")) %>%
    group_by(week_date) %>%
    summarise(tpv = sum(amount_transacted) / 1000000) %>%
    ggplot(aes(x = week_date, y = tpv)) +
    geom_line(color = "darkred", size = 1) +
    geom_point(color = "darkred") +
    labs(title = "TPV Evolution by Week",
         subtitle = "Trends over time",
         x = "Date",
         y = "TPV (Millions)") +
    theme_bw() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y")

## Average Ticket evolution by month (Line Plot) ----

payment_data %>%
    mutate(month_year = floor_date(day, "month")) %>%
    group_by(month_year) %>%
    summarise(avg_ticket = sum(amount_transacted) / sum(quantity_transactions)) %>%
    ggplot(aes(x = month_year, y = avg_ticket)) +
    geom_line(color = "seagreen", size = 1) +
    geom_point(color = "seagreen") +
    labs(title = "Average Ticket Evolution by Month",
         subtitle = "Trends over time",
         x = "Date",
         y = "Average Ticket Size") +
    theme_bw() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y")

## Average Ticket evolution by week (Line Plot) ----

payment_data %>%
    mutate(week_date = floor_date(day, "week")) %>%
    group_by(week_date) %>%
    summarise(avg_ticket = sum(amount_transacted) / sum(quantity_transactions)) %>%
    ggplot(aes(x = week_date, y = avg_ticket)) +
    geom_line(color = "purple", size = 1) +
    geom_point(color = "purple") +
    labs(title = "Average Ticket Evolution by Week",
         subtitle = "Trends over time",
         x = "Date",
         y = "Average Ticket Size") +
    theme_bw() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y")


## Transaction Volume evolution by month (Line Plot) ----

payment_data %>%
    mutate(month_year = floor_date(day, "month")) %>%
    group_by(month_year) %>%
    summarise(total_transactions = sum(quantity_transactions)) %>%
    ggplot(aes(x = month_year, y = total_transactions)) +
    geom_line(color = "darkorange", size = 1) +
    geom_point(color = "darkorange") +
    labs(title = "Transaction Volume Evolution by Month",
         subtitle = "Trends over time",
         x = "Date",
         y = "Total Transaction Volume") +
    theme_bw() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y")

## Transaction Volume evolution by week (Line Plot) ----

payment_data %>%
    mutate(week_date = floor_date(day, "week")) %>%
    group_by(week_date) %>%
    summarise(total_transactions = sum(quantity_transactions)) %>%
    ggplot(aes(x = week_date, y = total_transactions)) +
    geom_line(color = "saddlebrown", size = 1) +
    geom_point(color = "saddlebrown") +
    labs(title = "Transaction Volume Evolution by Week",
         subtitle = "Trends over time",
         x = "Date",
         y = "Total Transaction Volume") +
    theme_bw() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y")

