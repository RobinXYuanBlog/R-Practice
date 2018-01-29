pm2_5 <- read_csv("./Downloads/data 2/daily_SPEC_2014.csv.bz2")
head(pm2_5)

Q1 <- pm2_5 %>% 
  # group_by(`State Code`) %>% 
  filter(`Parameter Name` == "Bromine PM2.5 LC", `State Name` == "Wisconsin") %>% 
  summarise( average = mean(`Arithmetic Mean`, na.rm=TRUE))

head(Q1)

Q2 <- pm2_5 %>% 
  group_by(`Parameter Name`, `State Name`, `Site Num`, `Date Local`) %>%  # Group data with these columns
  filter(grepl("LC", `Parameter Name`)) %>%                               # Find out all `Parameter Name` contains LC
  summarise( total_average = mean(`Arithmetic Mean`) ) %>%                # Create a column called total_average and store mean                                                                             values into it
  arrange(desc(total_average))                                            # Arrange the dataset in descending order

head(Q2)
attach(Q2)

Q2_subtable <- Q2 %>%
  filter(grepl("^W", `State Name`))

head(Q2)

library(ggplot2)
ggplot(Q2_subtable, aes(x = `State Name`, y = total_average)) + 
  geom_bar(stat = "identity")


