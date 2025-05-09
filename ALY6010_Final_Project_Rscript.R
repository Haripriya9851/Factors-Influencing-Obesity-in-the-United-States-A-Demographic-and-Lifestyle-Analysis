### ALY6010 - Final Project 
## Created By: Hari Priya Ramamoorthy
## Dataset : https://www.cdc.gov/brfss/annual_data/annual_2023.html

######################################### EDA ##################################################################################
### ALY6010 - Final Project Milestone 1 - R Assignment
## Created By: Hari Priya Ramamoorthy
## Dataset Details: https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-behavioral-risk-factor-surveillance-system

library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(knitr)
library(scales)
library(stringr)
library(janitor)
library(skimr)
library(haven)

data <- read.csv("Nutrition__Physical_Activity__and_Obesity_-_Behavioral_Risk_Factor_Surveillance_System.csv", stringsAsFactors = FALSE)
# Function to create a glimpse table
create_glimpse_table <- function(df) {
  tibble(
    Column_Name = names(df),
    Data_Type = sapply(df, class),
    Example_Value = sapply(df, function(x) if (length(x) > 0) x[1] else NA)
  )
}

raw_data_glimpse<-create_glimpse_table(data)

raw_data<-data

################################### Data Pre-processing ################################################################################

#Select Needed columns
data <- data %>% select (-c(
  YearEnd,
  LocationAbbr,
  Datasource,
  LocationID,
  GeoLocation,
  StratificationCategoryId1,
  StratificationID1,
  QuestionID,
  ClassID,
  TopicID,
  Data_Value_Alt,
  Data_Value_Type,
  DataValueTypeID, Data_Value_Unit,Data_Value_Footnote_Symbol,Data_Value_Footnote,
  Sample_Size,Low_Confidence_Limit,High_Confidence_Limit))

#Clean Column Names

data<-janitor::clean_names(data)
names(data)

# Data Inspection
dim(data)
str(data)  # Check data types
summary(data)  # Summary statistics for numerical columns
colSums(is.na(data))  # Count of missing values per column

# Remove Rows with any NA values
data <- na.omit(data)
colSums(is.na(data)) 

data$question[data$question=="Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)"]<-"Weekly 150/75 mins of moderate/rigorous aerobic exercise"
data$question[data$question=="Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic physical activity and engage in muscle-strengthening activities on 2 or more days a week"] <- "Weekly 150/75 mins of moderate/rigorous aerobic exercise + atleast 2 days muscle training"
data$question[data$question=="Percent of adults who achieve at least 300 minutes a week of moderate-intensity aerobic physical activity or 150 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)"] <- "Weekly 300 mins of moderate or 150 mins of rigorous aerobic exercise "
data$question[data$question=="Percent of adults who engage in muscle-strengthening activities on 2 or more days a week"]<-"Muscle-strengthening activities for 2 or more days"
data$question[data$question=="Percent of adults who engage in no leisure-time physical activity"]<-"No leisure-time physical activity"

processed_data_glimpse<-create_glimpse_table(data)

# Select only numerical columns to generate summary statistics
numerical_cols <- data %>%
  select(where(is.numeric))

# Convert summary output to a data frame
summary_df <- data.frame(
  Min = sapply(numerical_cols, min, na.rm = TRUE),
  Q1 = sapply(numerical_cols, function(x) quantile(x, 0.25, na.rm = TRUE)),
  Median = sapply(numerical_cols, median, na.rm = TRUE),
  Mean = sapply(numerical_cols, mean, na.rm = TRUE),
  Q3 = sapply(numerical_cols, function(x) quantile(x, 0.75, na.rm = TRUE)),
  Max = sapply(numerical_cols, max, na.rm = TRUE),
  SD = sapply(numerical_cols, sd, na.rm = TRUE),
  N = sapply(numerical_cols, function(x) sum(!is.na(x)))
)

## What are Survey questions For each Class?
question_var=data %>% group_by(class,question) %>% summarise(frequency_count=n()) %>% ungroup()

## What are Survey questions For each Class?
question_var=data %>% group_by(class) %>% summarise(question_count=n_distinct(question)) %>% ungroup()

## Class_topics frequency table : 3 classes and 3 Topics explains Nutritional behavior, physical exercise behavior affect obesity.
Class_topics_freq=data %>% group_by(class,topic,stratification_category1) %>% summarise(frequency_count=n()) %>% ungroup()

## Unique locations
unique(data$location_desc)

# Check for duplicate school names : No dupe
duplicates <- data %>%
  group_by(year_start,location_desc,class,topic,data_value,stratification_category1,stratification1,question) %>%
  filter(n() > 1)

data_clean <- data

dim(raw_data)
dim(data_clean)
################################### Data Pre-processing ################################################################################



######################################### PLOT 1: Distribution of obesity % Across Age ##############################################################################
obesity_dist_by_age <- data %>% filter(age_years!="") %>% 
  group_by(age_years) %>% 
  summarise("Dist_Freq"= mean(data_value,na.rm = TRUE)) %>% ungroup()
#hist(obesity_dist$Dist_Freq)
#obesity_dist_by_age <- na.omit(obesity_dist_by_age)
ggplot(obesity_dist_by_age, aes(x = age_years, y = Dist_Freq)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Mean Obesity/Overweight % by Age Group", x = "Age Group", y = "Obese %") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  ) +
  geom_text(aes(label = paste0(round(Dist_Freq,2),"%")), 
            vjust = -0.5,   # Position above the bar
            size = 4)+
  # Add mean line
  geom_hline(aes(yintercept = mean(Dist_Freq, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  
  # Add mean label on the mean line
  annotate("text", x = (max(obesity_dist_by_age$age_years)), 
           y = mean(obesity_dist_by_age$Dist_Freq, na.rm = TRUE), 
           label = paste("Mean:", formatC(mean(obesity_dist_by_age$Dist_Freq, na.rm = TRUE), 
                                          format = "f", digits = 2, big.mark = ",")), 
           color = "White", vjust = 1, size = 4) 


# Calculate the correlation matrix
obesity_dist_by_age$age_years<-as.numeric(as.factor(obesity_dist_by_age$age_years))
cor_matrix_age <- cor(obesity_dist_by_age, use = "complete.obs")

# View the correlation matrix
print(cor_matrix_age)

# Calculate Pearson's correlation coefficient
pearson_coeff_age <- cor(obesity_dist_by_age$age_years,obesity_dist_by_age$Dist_Freq)

# Print the Pearson correlation coefficient
# Print the result
cat(sprintf("Pearson's Correlation Coefficient between Age and BMI: %.4f\n", pearson_coeff_age))

pearson_df_age <- data.frame(
  Statistic = c("Tested Variable", "Null Hypothesis", "Alternative Hypothesis", 
                
                "Pearson's Coefficient",
                #"P-Value", 
                #"Significance Level",
                "Result"
  ),
  Value= c(" BMI And Age", 
           "H0: Negative or No significant association", 
           "Ha: Significant Positive association", 
           #round(t_value, 6),   
           #df, 
           round(pearson_coeff_age, 4),
           #format.pval(t_test_result_edu_level$p.value, digits = 6), 
           #"95% (0.05)",
           #conf_int_formatted, 
           #round( t_test_result_edu_level$estimate[1], 4),round( t_test_result_edu_level$estimate[2],4), 
           "Reject H0, Significant Positive Association Exists")
) 


######################################### PLOT 1: Distribution of obesity % Across Age ##############################################################################


######################################### PLOT 2: Distribution of obesity % Across Education  ##############################################################################
obesity_dist_by_edu <- data %>% filter(education!="") %>% 
  group_by(education) %>% 
  summarise("Dist_Freq"= mean(data_value,na.rm = TRUE)) %>% ungroup()
#hist(obesity_dist$Dist_Freq)

ggplot(obesity_dist_by_edu, aes(x = reorder(education,-Dist_Freq), y = Dist_Freq)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Mean Obesity/Overweight % by Population's Education", x = "Educatio", y = "Obese %") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10,  hjust = 0.5),
    axis.text.y = element_text(size = 10)
  ) +
  geom_text(aes(label = paste0(round(Dist_Freq,2),"%")), 
            vjust = -0.5,   # Position above the bar
            size = 4)+
  # Add mean line
  geom_hline(aes(yintercept = mean(Dist_Freq, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  
  # Add mean label on the mean line
  annotate("text", x = (min(obesity_dist_by_edu$education)), 
           y = mean(obesity_dist_by_edu$Dist_Freq, na.rm = TRUE), 
           label = paste("Mean:", formatC(mean(obesity_dist_by_edu$Dist_Freq, na.rm = TRUE), 
                                          format = "f", digits = 2, big.mark = ",")), 
           color = "Red", vjust = -1, size = 4)

# Calculate the correlation matrix
obesity_dist_by_edu$education<-as.numeric(as.factor(obesity_dist_by_edu$education))

# Calculate Pearson's correlation coefficient
pearson_coeff_edu <- cor(obesity_dist_by_edu$education,obesity_dist_by_edu$Dist_Freq)

# Print the Pearson correlation coefficient
# Print the result
cat(sprintf("Pearson's Correlation Coefficient between Education and BMI: %.4f\n", pearson_coeff_edu))

pearson_df_edu <- data.frame(
  Statistic = c("Tested Variable", "Null Hypothesis", "Alternative Hypothesis", 
                
                "Pearson's Coefficient",
                #"P-Value", 
                #"Significance Level",
                "Result"
  ),
  Value= c(" BMI And Education (Encoded education college graduate to less than school, as 1 to 4)", 
           "H0: Negative or No significant association", 
           "Ha: Significant Positive association", 
           #round(t_value, 6),   
           #df, 
           round(pearson_coeff_edu, 4),
           #format.pval(t_test_result_edu_level$p.value, digits = 6), 
           #"95% (0.05)",
           #conf_int_formatted, 
           #round( t_test_result_edu_level$estimate[1], 4),round( t_test_result_edu_level$estimate[2],4), 
           "Reject H0, Significant Positive Association Exists")
) 

######################################### PLOT 2: Distribution of obesity % Across Education  ##############################################################################


######################################### PLOT 3: Distribution of obesity % Across Physical Activity ##############################################################################
obesity_dist_by_physical_activity <- data %>% filter(class=="Physical Activity" & location_desc=="National") %>% 
  group_by(question) %>% 
  summarise("Dist_Freq"= mean(data_value,na.rm=TRUE)) %>% ungroup()


ggplot(obesity_dist_by_physical_activity, aes(x = reorder(question,-Dist_Freq), y = Dist_Freq)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Mean Obesity/Overweight % by Physical Activity ", x = "Physical Activity", y = "Obese %") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 11, angle=0, hjust = 0.5),
    axis.text.y = element_text(size = 10)
  ) +
  
  scale_x_discrete(labels = label_wrap(width = 35))+  
  geom_text(aes(label = paste0(round(Dist_Freq,2),"%")), 
            vjust = -0.5,   # Position above the bar
            size = 4)+
  # Add mean line
  geom_hline(aes(yintercept = mean(Dist_Freq, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  
  # Add mean label on the mean line
  annotate("text", x = 5, 
           y = mean(obesity_dist_by_physical_activity$Dist_Freq, na.rm = TRUE), 
           label = paste("Mean:", formatC(mean(obesity_dist_by_physical_activity$Dist_Freq, na.rm = TRUE), 
                                          format = "f", digits = 2, big.mark = ",")), 
           color = "Red", vjust = -1, size = 4)
######################################### PLOT 3: Distribution of obesity % Across  Physical Activity ##############################################################################

######################################### EDA ##################################################################################



######################################### Hypothesis Testing #########################################
#https://www.niddk.nih.gov/health-information/health-statistics/overweight-obesity

NHANES2023<-read_xpt(
  "LLCP2023.XPT "
)

NHANES2021<-read_xpt(
  "LLCP2021.XPT "
)


######################################### Hypothesis Test - Data Pre-Processing #########################################

#_AGE_G, _INCOMG1, _EDUCAG,_INCOMG1,
#_TOTINDA(  Adults who reported doing physical activity or exercise during the past 30 days other than their regular job)
selected_data_2023 <- NHANES2023 %>% 
  select(
    # Age group columns
    `_AGE_G`,
    
    # Income level
    `_INCOMG1`,
    
    # Education group
    `_EDUCAG`,
    
    # Race/ethnicity group
    `_RACEGR3`, 
    
    # BMI columns
    `_BMI5CAT`,`_RFBMI5`,
    `_PAREC3`,`_SEX`,`_BMI5`
    # Exercise
    #`_PAINDX3`, `_PA150R4`, `_PA300R4`, `_PASTRNG`, #"_PACAT3"  "_PAINDX3" "_PA150R4" "_PA300R4" "_PA30023" "_PASTRNG" "_PAREC3"  "_PASTAE3"
  )


selected_data_2023<-janitor::clean_names(selected_data_2023)
names(selected_data_2023)


# Adjust BMI - normalize between 0 and 100
selected_data_2023$bmi5_normalized <- selected_data_2023$bmi5 / 100
selected_data_2023$bmi5_normalized[selected_data_2023$bmi5 == 7777 | selected_data_2023$bmi5 == 9999] <- NA
selected_data_2023$incomg1[selected_data_2023$incomg1 %in% c(77, 99)] <- NA
selected_data_2023$educag[selected_data_2023$educag %in% c(7, 9)] <- NA
selected_data_2023$racegr3[selected_data_2023$racegr3 %in% c(7, 9)] <- NA
selected_data_2023$sex[selected_data_2023$sex %in% c(7, 9)] <- NA
selected_data_2023$parec3[selected_data_2023$parec3 %in% c(7, 9)] <- NA

# Convert education levels to factors and remove "Don’t know/Not sure/Missing"
selected_data_2023$educag <- factor(selected_data_2023$educag,
                                    levels = c(1, 2, 3, 4),
                                    labels = c("Did not graduate High School",
                                               "Graduated High School",
                                               "Attended College or Technical School",
                                               "Graduated from College or Technical School"))

selected_data_2023$age_g <- factor(selected_data_2023$`age_g`,
                                   levels = c(1, 2, 3, 4, 5, 6),
                                   labels = c("Age 18 to 24",
                                              "Age 25 to 34",
                                              "Age 35 to 44",
                                              "Age 45 to 54",
                                              "Age 55 to 64",
                                              "Age 65 or older"))

# Convert physical activity levels to factors and remove "Don’t know/Refused/Missing"
selected_data_2023$parec3 <- factor(selected_data_2023$`parec3`,
                                    levels = c(1, 2, 3, 4),
                                    labels = c("Met Both Guidelines",
                                               "Met Aerobic Guidelines Only",
                                               "Met Strengthening Guidelines Only",
                                               "Did not meet Either Guideline"
                                    ))

selected_data_2023$bmi5cat <- factor(selected_data_2023$bmi5cat,
                                     levels = c(1, 2, 3, 4),
                                     labels = c("Underweight",
                                                "Normal Weight",
                                                "Overweight",
                                                "Obese"
                                     ))

selected_data_2023$rfbmi5 <- factor(selected_data_2023$rfbmi5,
                                    levels = c(1, 2 ),
                                    labels = c("No",
                                               "Yes"
                                    ))


# check missing values
sum(is.na(selected_data_2023))
# Remove rows with missing values
selected_data_2023 <- na.omit(selected_data_2023)

# Confirm no missing values
sum(is.na(selected_data_2023))

######################################### Data Pre-Processing #########################################

########################################## normality check #########################################
## Histogram to check Normality
ggplot(selected_data_2023, aes(x = bmi5/100)) + 
  
  geom_histogram( bins=10 , fill = "red", color = "black") + 
  
  labs(title = "Distribution of BMI in U.S", x = "BMI (kg/m2)") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10, angle = 45, hjust = 1),  
    axis.text.x = element_text(color = "Black"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) 

# Q-Q plot for BMI  - Before Normalization
qqnorm(selected_data_2023$bmi5)
qqline(selected_data_2023$bmi5, col = "red") #This shows skewness to the right

selected_data_2023$log_bmi5_normalized <- log(selected_data_2023$bmi5_normalized)
selected_data_2023 <- na.omit(selected_data_2023)

ggplot(selected_data_2023, aes(x = log_bmi5_normalized)) + 
  
  geom_histogram( bins=10 , fill = "blue", color = "black") + 
  
  labs(title = "Distribution of Log-transformed BMI in U.S", x = "BMI (kg/m)")+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10, angle = 45, hjust = 1),  
    axis.text.x = element_text(color = "Black"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) 

summary(selected_data_2023$log_bmi5_normalized) 

# Remove -Inf values in log_pm25
selected_data_2023_clean <- selected_data_2023 %>%
  filter( log_bmi5_normalized > 0)

# Q-Q plot - After Normalization
qqnorm(selected_data_2023_clean$log_bmi5_normalized, main = "Q-Q Plot of Log-Transformed BMI")
qqline(selected_data_2023_clean$log_bmi5_normalized, col = "red")
########################################## normality check #########################################


############################## Hypothesis Test - 1: Prevalance of Obesity by Age ###########################################################################
# https://www.cdc.gov/obesity/php/data-research/adult-obesity-prevalence-maps.html

# Filter data for the two education levels 
group1_18_24 <- selected_data_2023_clean %>% 
  filter(age_g == "Age 18 to 24") %>% 
  pull(log_bmi5_normalized)  # Assuming 'log_bmi5_normalized' is the BMI variable

group2_45_54 <- selected_data_2023_clean %>% 
  filter(age_g == "Age 45 to 54") %>% 
  pull(log_bmi5_normalized)

# Perform F-test for variance equality
var_test_result_age_levels <- var.test(group1_18_24, group2_45_54)
## Result - Variances are Equal --> We can perform t-test for means

print(var_test_result_age_levels) # Equal variance

# Perform t-test for BMI means  
t_test_result_age_level <- t.test(group1_18_24, group2_45_54, var.equal = TRUE, alternative = "less")

# Output the t-test result
t_test_result_age_level

hypothesis_results_age_level <- data.frame(
  Statistic = c("Tested Variable", "Null Hypothesis", "Alternative Hypothesis", 
                "P-Value", 
                "Significance Level",
                "No_graduate_Group Mean", "Graduate_Group Mean", "Result"
  ),
  Value= c("Normalized BMI Across Age Level", 
           "H0: Mean BMI of age 18-24 >= 45-54", 
           "Ha: Mean BMI of age 18-24 < 45-54", 
           #round(t_value, 6),   
           #df, 
           format.pval(t_test_result_age_level$p.value, digits = 6), 
           "95% (0.05)",
           #conf_int_formatted, 
           round( t_test_result_age_level$estimate[1], 4),round( t_test_result_age_level$estimate[2],4), 
           ifelse(t_test_result_age_level$p.value<0.05,"Reject H0","No Evidence to reject H0")
  ) )
############################## Hypothesis Test - 1: Prevalance of Obesity by Age ###########################################################################

############################## Hypothesis Test - 2 : Prevalence of Obesity by Education  ##############################

#H0: obesity prevalenvce in "Did not graduate High School" <= "Graduated from College or Technical School"
#H1: obesity prevalenvce in "Did not graduate High School" > "Graduated from College or Technical School"

# Filter data for the two education levels 
group1_no_graduate <- selected_data_2023_clean %>% 
  filter(educag == "Did not graduate High School") %>% 
  pull(log_bmi5_normalized)  # Assuming 'log_bmi5_normalized' is the BMI variable

group2_graduate <- selected_data_2023_clean %>% 
  filter(educag == "Graduated from College or Technical School") %>% 
  pull(log_bmi5_normalized)

# Perform F-test for variance equality
var_test_result_education_levels <- var.test(group1_no_graduate, group2_graduate)
## Result - Variances are Equal --> We can perform t-test for means

print(var_test_result_education_levels) # Equal variance

# Perform t-test for BMI means  
t_test_result_edu_level <- t.test(group1_no_graduate, group2_graduate, var.equal = TRUE, alternative = "greater")

# Output the t-test result
t_test_result_edu_level

hypothesis_results_edu_level <- data.frame(
  Statistic = c("Tested Variable", "Null Hypothesis", "Alternative Hypothesis", 
                "P-Value", 
                "Significance Level",
                "No_graduate_Group Mean", "Graduate_Group Mean", "Result"
  ),
  Value= c("Normalized BMI Across Education Level", 
           "H0: Mean BMI of No graduates <= Graduates", 
           "Ha: Mean BMI of No graduate > Graduates", 
           #round(t_value, 6),   
           #df, 
           format.pval(t_test_result_edu_level$p.value, digits = 6), 
           "95% (0.05)",
           #conf_int_formatted, 
           round( t_test_result_edu_level$estimate[1], 4),round( t_test_result_edu_level$estimate[2],4), 
           ifelse(t_test_result_edu_level$p.value<0.05,"Reject H0","No Evidence to reject H0")
  ) )

############################## Hypothesis Test - 2 : Prevalence of Obesity by Education ###########################################################################


############################## Hypothesis Test - 3 : Physical Activity a Risk Factor? ###########################################################################
#According to CDC, https://www.cdc.gov/obesity/php/about/risk-factors.html - Lack of Physical activity is a factor for obesity
# H0: Obesity and Physical activity has No significant association
# H1: Obesity and Physical activity has Negative Significant Association


# Calculate means for each group
mean_values <- selected_data_2023 %>%
  group_by(parec3) %>%
  summarise(mean_bmi = mean(bmi5/100, na.rm = TRUE), .groups = 'drop')

# Create the selected_data_2023# Create the box plot with mean points
ggplot(selected_data_2023, aes(x = parec3, y = bmi5, fill = parec3)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Create the box plot
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", fill = "white", position = position_dodge(0.75)) +  # Add mean points
  labs(
    title = "Box Plot of BMI by Physical Activity Level",
    x = "Physical Activity Level",
    y = "BMI"
  ) +
  scale_y_continuous(limits = c(0, NA)) +  # Set y-axis to start at 0
  geom_text(data = mean_values, aes(x = parec3, y = mean_bmi, label = round(mean_bmi, 2)), 
            vjust = -0.5, color = "black", size = 4) + 
  theme_minimal() +
  
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10, angle = 45, hjust = 1),  
    axis.text.x = element_text(color = "Black"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "none"
  )



# Convert the 'parec3' variable to a factor and encode it with numeric levels
selected_data_2023_clean$parec3_encoded <- factor(selected_data_2023$parec3,
                                                  levels = c("Met Both Guidelines", 
                                                             "Met Aerobic Guidelines Only", 
                                                             "Met Strengthening Guidelines Only", 
                                                             "Did not meet Either Guideline"),
                                                  labels = c(4, 3, 2, 1))

# Convert it to numeric for correlation analysis
selected_data_2023_clean$parec3_encoded <- as.numeric(as.character(selected_data_2023_clean$parec3_encoded))


# Calculate Pearson's correlation coefficient
pearson_coeff_pa <- cor(selected_data_2023_clean$parec3_encoded, log(selected_data_2023_clean$bmi5))

# Print the Pearson correlation coefficient
# Print the result
cat(sprintf("Pearson's Correlation Coefficient between Physical Activity and BMI: %.4f\n", pearson_coeff_pa))

pearson_df_pa <- data.frame(
  Statistic = c("Tested Variable", "Null Hypothesis", "Alternative Hypothesis", 
                "Pearson's Coefficient",
                #"P-Value", 
                #"Significance Level",
                "Result"
  ),
  Value= c(" Normalized BMI And Physical Activity", 
           "H0: Positive or No significant association", 
           "Ha: Significant Negative association", 
           #round(t_value, 6),   
           #df, 
           round(pearson_coeff_pa, 4),
           #format.pval(t_test_result_edu_level$p.value, digits = 6), 
           #"95% (0.05)",
           #conf_int_formatted, 
           #round( t_test_result_edu_level$estimate[1], 4),round( t_test_result_edu_level$estimate[2],4), 
           "Reject H0, Significant Negative Association Exists")
) 


############################## Hypothesis Test - 3 : Physical Activity a Risk Factor? ###########################################################################

######################################### Hypothesis Testing #########################################


################################ Linear/Log Regression To predict Obesity #########################################

selected_data_2023_clean=na.omit(selected_data_2023_clean)

# Recode categorical columns as integer levels
selected_data_2023_clean$educag <- as.integer(factor(selected_data_2023_clean$educag))
selected_data_2023_clean$age_g <- as.integer(factor(selected_data_2023_clean$age_g))
selected_data_2023_clean$physical_act <- as.integer(factor(selected_data_2023_clean$parec3))
selected_data_2023_clean$bmi5cat <- as.integer(factor(selected_data_2023_clean$bmi5cat))
selected_data_2023_clean$rfbmi5 <- as.integer(factor(selected_data_2023_clean$rfbmi5))


model <- lm(rfbmi5 ~ physical_act + educag + age_g  , data = selected_data_2023_clean)
summary(model)

# Extract regression results
model_summary <- summary(model)
coefficients <- as.data.frame(model_summary$coefficients)
names(coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

# Optional: round values for a cleaner table
coefficients <- round(coefficients, 3)

################################ Linear/Log Regression To predict Obesity #########################################




######## Print Results ##########

# Age - Results
print(kable(pearson_df_age, caption = "Pearson's Test to check Association between Age and Obesity/Overweight") )
print(kable(hypothesis_results_age_level, caption = "Hypothesis Testing Summary"))

# Education - Results
print(kable(pearson_df_edu, caption = "Pearson's Test to check Association between Education and Obesity/Overweight") )
print(kable(hypothesis_results_edu_level, caption = "Hypothesis Testing Summary"))

# Physical Activity - Results
print(kable(pearson_df_pa, caption = "Pearson's Test to check Association between Physical Activity and Obesity/Overweight") )

# Regression Analysis - Results
print(kable(coefficients, 
            caption = "Logistic Regression Results for BMI Classification Model",
            col.names = c("Estimate", "Std. Error", "t Value", "p-value")))
