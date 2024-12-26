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



#1.  
#a)
elections$Vote.for.Highest.Office..President. <- as.numeric(gsub(",", "", elections$Vote.for.Highest.Office..President.))
elections$VEP.Turnout.Rate <- as.numeric(gsub("%", "", elections$VEP.Turnout.Rate))

ggplot(elections, aes(x = Vote.for.Highest.Office..President., y = VEP.Turnout.Rate)) +
  geom_point() +  # Plot points
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "Votes for Highest Office (President)", y = "VEP Turnout Rate (%)") +
  theme_minimal()


#b)
elections$VEP.Turnout.Rate <- as.numeric(gsub("%", "", elections$VEP.Turnout.Rate))

ggplot(elections, aes(x = VEP.Turnout.Rate)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +  
  geom_density(color = "red") +  # Density curve
  labs(
    title = "Distribution of VEP Turnout Rate Across States in the 2020 U.S. Election", 
    x = "VEP Turnout Rate (%)",
    y = "Density"
  ) +
  theme_minimal()





#2.
elections$VEP.Turnout.Rate <- as.numeric(gsub("%", "", elections$VEP.Turnout.Rate))
ggplot(elections, aes(x = State, y = VEP.Turnout.Rate)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "red") +  
  labs(x = "State", y = "VEP Turnout Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  




#b)
ggplot(elections, aes(x = VEP.Turnout.Rate)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +  
  geom_density(color = "red") +  
  labs(x = "VEP Turnout Rate (%)", y = "Density") +
  theme_minimal()

elections_stacked <- elections %>%
  select(State, X..Non.citizen, Prison, Probation, Parole) %>%
  mutate(across(-State, ~ as.numeric(gsub(",", "", .)), .names = "cleaned_{col}")) %>%
  pivot_longer(cols = starts_with("cleaned"), names_to = "Category", values_to = "Count") %>%
  mutate(Category = gsub("cleaned_", "", Category))  

# Drop rows with NA values in Count
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
