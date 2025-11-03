install.packages(c("tidyverse", "ggplot2", "readr", "broom"))
library(readr)
# Load data
grape_data <- read_csv("FAOSTAT_data_en_8-11-2025.csv") %>% 
  select("Area", "Year", "Value") %>% 
  rename("Country" = "Area", "Yield" = "Value") 
  drop_na(grape_data)  # Remove missing values
head(grape_data)

library(ggplot2)

# Time-series plot
ggplot(grape_data, aes(x = Year, y = Yield)) +
  geom_line(stat = "summary", fun = "mean", color = "darkgreen") +
  labs(title = "Average grape Yield in Europe (2000-2023)", 
       y = "Yield (tonnes/ha)") +
  theme_minimal()


# Top 5 highest-yielding countries
grape_data %>% 
  group_by(Country) %>% 
  summarise(Mean_Yield = mean(Yield)) %>% 
  arrange(desc(Mean_Yield)) %>% 
  head(5) %>% 
  ggplot(aes(x = reorder(Country, Mean_Yield), y = Mean_Yield)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Highest-Yielding Countries (2000-2023)", 
       x = NULL, y = "Yield (tonnes/ha)")

library(broom)

# Fit model
model <- lm(Yield ~ Year, data = grape_data)
summary(model)  # Check p-value and R-squared

# Plot regression line
ggplot(grape_data, aes(x = Year, y = Yield)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression: Year vs. grape Yield")