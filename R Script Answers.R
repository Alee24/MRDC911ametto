
install.packages(c("tidyverse", "ggplot2", "corrplot"))
library(tidyverse)
library(ggplot2)
library(corrplot)

Q1: 
  data <- read_csv("kenya_student_data.csv")
  str(data)
  glimpse(data)
  summary(data)
  Q2: 
    data %>% select(where(is.numeric)) %>% summary()
  Q3: 
    ggplot(data, aes(x = academic_performance)) +
    geom_bar(fill = "#443785") +
    labs(title = "Distribution of Academic Performance")
  Q4: 
    ggplot(data, aes(x = study_hours_weekly)) +
    geom_histogram(binwidth = 2, fill = "#443785") +
    facet_wrap(~residency) +
    labs(title = "Study Hours by Location")
  Q5: 
    ggplot(data, aes(x = academic_performance, y = math_score, fill = gender)) +
    geom_boxplot() +
    labs(title = "Math Score by Academic Performance and Gender")
  Q6: 
    data %>% count(extracurricular_activities) %>% mutate(prop = n / sum(n))
  data %>% count(faculty) %>% mutate(prop = n / sum(n))
  
  
  Q7: 
    num_data <- data %>% select(where(is.numeric), -student_id)
  corr_matrix <- cor(num_data, use = "complete.obs")
  corrplot(corr_matrix, method = "color", type = "lower")
  Q8: 
    chisq.test(table(data$internet_access, data$academic_performance))
    Q9: 
    missing_summary <- sapply(data, function(x) sum(is.na(x)) / length(x) * 100)
  missing_summary[missing_summary > 0]
  Q10: 
    data$family_income[is.na(data$family_income)] <- median(data$family_income, na.rm = TRUE)
  data$math_score[is.na(data$math_score)] <- median(data$math_score, na.rm = TRUE)
  Q11: 
    hist_before <- ggplot(data, aes(x = attendance_rate)) + geom_histogram()
  data$attendance_rate[is.na(data$attendance_rate)] <- mean(data$attendance_rate, na.rm = TRUE)
  hist_after <- ggplot(data, aes(x = attendance_rate)) + geom_histogram()
  Q12: 
    iqr <- IQR(data$family_income, na.rm = TRUE)
  lower <- quantile(data$family_income, 0.25, na.rm = TRUE) - 1.5 * iqr
  upper <- quantile(data$family_income, 0.75, na.rm = TRUE) + 1.5 * iqr
  sum(data$family_income < lower | data$family_income > upper)
  
  
  Q13: 
    data$family_income <- ifelse(data$family_income > upper, upper,
                                 ifelse(data$family_income < lower, lower, data$family_income))
  ggplot(data, aes(y = family_income)) + geom_boxplot()
  Q14: 
    data$study_hours_binned <- cut(data$study_hours_weekly,
                                   breaks = c(0, 10, 20, 30, Inf),
                                   labels = c("Low", "Moderate", "High", "Very High"))
  ggplot(data, aes(x = study_hours_binned)) + geom_bar(fill = "#443785")
  Q15: 
    data$family_income_binned <- cut(data$family_income,
                                     breaks = quantile(data$family_income, probs = seq(0, 1, 0.25), na.rm = TRUE),
                                     include.lowest = TRUE,
                                     labels = c("Low", "Medium-Low", "Medium-High", "High"))
  table(data$family_income_binned, data$academic_performance)
  Q16: 
    data$total_score <- rowMeans(data[,c("math_score", "science_score", "english_score")], na.rm = TRUE)
  ggplot(data, aes(x = total_score)) + geom_histogram(fill = "#443785")
  Q17: 
    table(data$extracurricular_activities, data$academic_performance)
  Q18: 
    ggplot(data, aes(x = study_hours_weekly, y = total_score, color = residency)) +
    geom_point(alpha = 0.6) +
    labs(title = "Study Hours vs Total Score by Residency")
 
  # save data 
  write_csv(data, "kenya_student_data_preprocessed.csv")
  
  