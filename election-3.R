rm(list = ls())

elections <- read.csv("2020 November General Election - Turnout Rates.csv", header = TRUE)

head(elections)
str(elections)
required_packages <- c("ggplot2", "dplyr", "tidyr", "scales")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

print("All required packages are successfully installed and loaded.")


elections_stacked <- elections %>%
  select(State, X..Non.citizen, Prison, Probation, Parole) %>%
  mutate(across(-State, ~ as.numeric(gsub(",", "", .)), .names = "cleaned_{col}")) %>%
  pivot_longer(cols = starts_with("cleaned"), names_to = "Category", values_to = "Count") %>%
  mutate(Category = gsub("cleaned_", "", Category))  

elections_stacked <- elections_stacked %>%
  filter(!is.na(Count))

elections_stacked_normalized <- elections_stacked %>%
  group_by(State) %>%
  mutate(Percentage = ifelse(sum(Count, na.rm = TRUE) > 0, Count / sum(Count, na.rm = TRUE) * 100, NA)) %>%
  ungroup() %>%
  filter(!is.na(Percentage))  

# Create the normalized stacked bar chart
ggplot(elections_stacked_normalized, aes(x = State, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +  
  labs(
    x = "State", 
    y = "Percentage (%)", 
    title = "Normalized Stacked Bar Chart: Proportions of Categories by State"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  
