library(hflights)
library(tidyr)
library(dplyr)

flight_analysis <- hflights %>%
  select(Year, Month, DayofMonth, FlightNum, Distance) %>%
  group_by(Month) %>%
  summarise(month_total = n()) %>%
  mutate(month_total = month_total - 5254)

ggplot(flight_analysis, aes(x = flight_analysis$Month, y = flight_analysis$month_total)) +
  geom_histogram(fill = "red", alpha = 0.5, stat = "identity") +
  geom_point(colour = "blue", alpha = 0.7, size = 2, y = flight_analysis$month_total / 2) +
  geom_line(colour = "blue", alpha = 0.7, size = 1, y = flight_analysis$month_total / 2) + 
  labs(x = "Month", y = "Month Total Flights") + 
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  geom_text(aes(label = flight_analysis$month_total, vjust = 0.5, hjust = 3.5, angle = 90), show_guide = FALSE)
