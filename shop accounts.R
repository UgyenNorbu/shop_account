library(tidyverse)
library(ggplot2)
library(scales)

# Import shop daily account and monthly ssumary details
path<- "Shop's Account statement.xlsx"
path_2 <- "account summary.xlsx"

daily_accounts <- readxl::read_xlsx(path)
glimpse(daily_accounts)

# Convert "Buy" to numeric type and replace empty entries with 0 indicating no transaction

daily_accounts <- daily_accounts %>% 
    mutate(Buy = as.numeric(Buy)) %>% 
    mutate(Sale = ifelse(is.na(Sale), 0, Sale)) %>% 
    mutate(Buy = ifelse(is.na(Buy), 0, Buy))

glimpse( daily_accounts)

# Functon to calculate number of rows without entry (the number 2 implies column, 
# and the function will be aplied on it)

na_sum <- function(col_name) {
    sum(is.na(col_name))
}

apply(daily_accounts, 2, na_sum)

# Entering monthly summary data

monthly_summary <- readxl::read_xlsx(path_2)

monthly_summary <- monthly_summary %>% 
    mutate(Monthly_profit = as.numeric(Monthly_profit))

glimpse(monthly_summary)
colnames(monthly_summary)

# Convert the daily_accounts into tidy data format
daily_accounts <- pivot_longer(data = daily_accounts[-1,], cols = -Date, 
             names_to = "Transaction_type", values_to = "Amount")

# The first entry of 

ggplot() +
    geom_area(aes(x = Month, y = Monthly_profit), data = monthly_summary[-1,],
              alpha = 0.3, fill = "#CAE1FF") +
    geom_line(aes(x = Month, y = Monthly_profit), data = monthly_summary[-1, ],
              size = 1.2, color = "#BCD2EE") +
    geom_point(aes(x = Date, y = Amount, color = Transaction_type), 
               data = daily_accounts, size = 1) +
    geom_line(aes(x = Date, y = Amount, color = Transaction_type),
              data = daily_accounts) +
    scale_color_manual(values = c("#B8B8B8","#CD6600"), name = "Transaction" )+
    labs(x = "Year", 
         y = "Amount in Nu. ", 
         title = "Sale and expenditure trends from 2018-2020"
        ) +
    scale_x_datetime(date_breaks = "1 month", labels = date_format("%b %Y")) +
        theme_minimal() +
    theme(axis.text.x = element_text(angle=15),
          legend.position = c(0.5, 1),
          legend.direction = "horizontal",
          plot.title = element_text(hjust = 0.5))

ggsave("Sale and expenditure trend from 2018-2020.jpg", dpi = 300, width = 25, height = 15, units = 'cm')
