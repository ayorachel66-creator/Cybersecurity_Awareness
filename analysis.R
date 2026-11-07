
# Step 4: Cybersecurity Awareness Data Analysis

# Install ggplot2 if needed
# install.packages("ggplot2")

library(ggplot2)

# Load dataset
cybersecurity_awareness <- read.csv(
  "cybersecurity_awareness.csv"
)

# 1. Summary of the dataset
summary(cybersecurity_awareness)

# 2. Average internet usage hours
mean(cybersecurity_awareness$Internet_Usage_Hours)

# 3. Frequency of cybersecurity awareness levels
table(cybersecurity_awareness$Cybersecurity_Awareness_Level)

# 4. Graph 1: Cybersecurity Awareness Level
graph1 <- ggplot(
  cybersecurity_awareness,
  aes(x = Cybersecurity_Awareness_Level, fill = Cybersecurity_Awareness_Level)
) +
  geom_bar() +
  labs(
    title = "Cybersecurity Awareness Levels",
    x = "Awareness Level",
    y = "Number of Students"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph1)

# Save Graph 1
ggsave("graph1_awareness_levels.png", graph1,
       width = 8, height = 5)

# 5. Graph 2: Password Practices
graph2 <- ggplot(
  cybersecurity_awareness,
  aes(x = Password_Practices, fill = Password_Practices)
) +
  geom_bar() +
  labs(
    title = "Student Password Practices",
    x = "Password Practice",
    y = "Number of Students"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph2)

# Save Graph 2
ggsave("graph2_password_practices.png", graph2,
       width = 8, height = 5)

# 6. Graph 3: Phishing Awareness
graph3 <- ggplot(
  cybersecurity_awareness,
  aes(x = Phishing_Awareness, fill = Phishing_Awareness)
) +
  geom_bar() +
  labs(
    title = "Student Phishing Awareness",
    x = "Phishing Awareness",
    y = "Number of Students"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph3)

# Save Graph 3
ggsave("graph3_phishing_awareness.png", graph3,
       width = 8, height = 5)

