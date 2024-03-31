# Import the packages
library(tidyverse)
library(lubridate)

# Read and preprocess the Bitcoin data
btc <- read_csv("BTC-USD.csv")
btc_data <- btc %>%
  mutate(daily_return = (btc_price / lag(btc_price) - 1)) %>%
  mutate(rank = rank(-daily_return, ties.method = "first"))
tail(btc_data)

# Create the function to simulate the various scenarios
simulate_btc_investment <- function(btc_data, days_missed, starting_amt) {
  btc <- btc_data %>%
    mutate(daily_return = (btc_price / lag(btc_price) - 1)) %>%
    mutate(rank = rank(-daily_return, ties.method = "first")) %>%
    mutate(missed = ifelse(rank <= days_missed, 1, 0)) %>%
    mutate(sell = ifelse(lead(missed) == 1, 1, 0)) %>%
    mutate(sell = ifelse(lag(sell) == 1, 0, sell)) %>%
    mutate(buy = ifelse(missed == 1, 1, 0)) %>%
    mutate(buy = ifelse(lead(buy) == 1, 0, buy)) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
    mutate(buy = ifelse(date == min(date) + days(1), 1, buy))
  
  btc$btc_held <- 0
  btc$usd_held <- starting_amt
  
  for(i in 2:nrow(btc)) {
    if(btc$buy[i] == 1) {
      btc$btc_held[i] <- btc$usd_held[i-1] / btc$btc_price[i]
      btc$usd_held[i] <- 0
    } else if(btc$sell[i] == 1) {
      btc$usd_held[i] <- btc$btc_held[i-1] * btc$btc_price[i]
      btc$btc_held[i] <- 0
    } else {
      btc$btc_held[i] <- btc$btc_held[i-1]
      btc$usd_held[i] <- btc$usd_held[i-1]
    }
  }
  
  btc <- btc %>%
    mutate(portfolio_value = btc_held * btc_price + usd_held,
           days_missed = days_missed)
  
  return(btc)
}

# Simulate for different days missed and bind the results
all_results <- tibble()
specific_days_missed <- c(0, 4, 40)
for(days_missed in specific_days_missed) {
  results <- simulate_btc_investment(btc_data, days_missed, 1000)
  # Bind the results with an additional column to differentiate simulations
  results$days_missed_scenario <- days_missed
  all_results <- bind_rows(all_results, results)
}

expanded_set1 <- c("green", "cyan2", "red")
custom_labels <- c(
  "Fully Invested All Days",
  "Missed the 4 Best Days \n(~0.1% of 10 Years)",
  "Missed the 40 Best Days \n(~1% of 10 Years)"
)
# Plot the results
p1 <- ggplot(all_results, aes(x = date, y = portfolio_value, color = as.factor(days_missed))) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.4) +
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_color_manual(values = expanded_set1, labels = custom_labels) +
  scale_x_date(
    limits = as.Date(c("2014-01-01", "2025-06-01")),
    date_labels = "%Y" # Format labels to show only the year
  ) +
  theme_minimal() +
  labs(title = "Missing Bitcoin's Best Days Has Been Costly",
       x = "",
       y = "Growth of $1,000",
       color = "Number of Best Days Missed",
       caption = "@bitcoinfool") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major.x = element_line(colour = "#2A2A2A80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#2A2A2A80"),
    panel.grid.minor.y = element_blank(),
    text = element_text(color = "white", size = 6),
    plot.title = element_text(color = "white", size = 11, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "white", size = 8),
    plot.caption = element_text(color = "white", size = 8, hjust = 1),
    plot.caption.position = "plot",  # Default value is "panel"
    plot.margin = margin(t = 10, r = 0, b = 5, l = 0, unit = "pt"),
    axis.text.y = element_text(size = 6, colour = "darkgrey"),
    axis.text.x = element_text(size = 6, colour = "darkgrey"),
    legend.position = "right",
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 5)
  )
p1

# Use the below code if you want to do an animation
p1_animation <- p1 +
  transition_reveal(date) +
  labs(subtitle = "Date: {frame_along}") +
  geom_text(aes(label = paste0("$", scales::comma(portfolio_value))), 
            nudge_x = 160, hjust = 0, size = 2.5, show.legend = FALSE) +
  ease_aes('linear') # Ensures smooth transition
a <- animate(p1_animation, height = 1200, width = 1920, fps = 40, duration = 24, end_pause = 150, res = 300, renderer = av_renderer())
anim_save("p1_animation_big.mp4", a)



# Now simulate for 0, 5, 10... to 50 best days missed
all_results_2 <- tibble()
specific_days_missed_2 <- seq(0, 50, 5)
for(days_missed in specific_days_missed_2) {
  results <- simulate_btc_investment(btc_data, days_missed, 1000)
  # Bind the results with an additional column to differentiate simulations
  results$days_missed_scenario <- days_missed
  all_results_2 <- bind_rows(all_results_2, results)
}

expanded_set2 <- c("#00DD00", "#FF7F00", "#ffdb58", "#E41A55", "#377EB8", "#46A444", "#984EA3",  "#A65628", "#F781BF", "#999999", "cyan3")
custom_labels <- c(
  "Fully Invested All Days",
  "Missed the 5 Best Days",
  "Missed the 10 Best Days",
  "Missed the 15 Best Days",
  "Missed the 20 Best Days",
  "Missed the 25 Best Days",
  "Missed the 30 Best Days",
  "Missed the 35 Best Days",
  "Missed the 40 Best Days",
  "Missed the 45 Best Days",
  "Missed the 50 Best Days"
)
last_points <- all_results_2 %>%
  group_by(days_missed) %>%
  filter(date == max(date)) %>%
  ungroup()
# Plot the results
p2 <- ggplot(all_results_2, aes(x = date, y = portfolio_value, color = as.factor(days_missed))) +
  geom_line(linewidth = 0.7) +
  geom_point(data = last_points, size = 4) +
  geom_text(data = last_points, aes(label = paste0("$", scales::comma(portfolio_value))), 
            nudge_x = 80, hjust = 0, size = 5, show.legend = FALSE) +
    scale_y_log10(labels = scales::label_dollar()) +
  scale_color_manual(values = expanded_set2, labels = custom_labels) +
  scale_x_date(
    limits = as.Date(c("2014-01-01", "2025-03-01")),
    date_labels = "%Y" # Format labels to show only the year
  ) +
  theme_minimal() +
  labs(title = "Missing Bitcoin's Best Days Has Been Costly",
       x = "",
       y = "Growth of $1,000",
       color = "Number of Best Days Missed",
       caption = "@bitcoinfool") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major.x = element_line(colour = "#2A2A2A80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#2A2A2A80"),
    panel.grid.minor.y = element_blank(),
    text = element_text(color = "white", size = 14),
    plot.title = element_text(color = "white", size = 22, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "white"),
    plot.caption = element_text(color = "white", hjust = 1),
    plot.caption.position = "plot",  # Default value is "panel"
    plot.margin = margin(t = 40, r = 20, b = 20, l = 40, unit data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg=== "pt"),
    axis.text.y = element_text(colour = "darkgrey"),
    axis.text.x = element_text(colour = "darkgrey"),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
p2
ggsave("p2.jpg", plot = p2, width = 16, height = 9, dpi = 300)
