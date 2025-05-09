# Factors-Influencing-Obesity-in-the-United-States-A-Demographic-and-Lifestyle-Analysis

This study examines the influence of demographic and lifestyle factors on obesity rates in the
United States—a pressing public health issue closely tied to chronic diseases, including heart
disease, diabetes, and certain cancers. Understanding the demographic and lifestyle contributors
to obesity is essential for policymakers to design effective public health interventions. To support
this, the study leverages data from the Centers for Disease Control and Prevention’s (CDC)
Behavioral Risk Factor Surveillance System (BRFSS) to inform evidence-based policies that
address obesity’s root causes, helping federal and state agencies deploy resources more
effectively and foster healthier communities.

The analysis proceeds in two main parts. First, an exploratory data analysis (EDA) of the BRFSS
dataset, accessible via the U.S. Government's Open Data portal, identifies key statistical
questions about obesity trends among the U.S. adult population. In the second part, T-tests,
correlation, and regression analyses are used to evaluate these questions and determine the
significance of various demographic and lifestyle factors in predicting obesity. Let’s dive into
analysis and insights.

# EDA
**1.Obesity Distribution Across Age and Education**
Figure 1 depicts that young adults(18-24) show the lowest obesity rates than elder adults. Figure 2 depicts that less educated adults(less than high school) show the highest obesity rates than college graduates.

<img width="385" alt="image" src="https://github.com/user-attachments/assets/13d52d16-4bb8-450e-9e5f-c315a32d4583" />
<img width="385" alt="image" src="https://github.com/user-attachments/assets/5858eaa7-8c25-49df-a1c1-541c82f18336" />

                
**3.Distribution of physical activity Across Demographics**
Figure 3 show the obesity distribution of physical activity (muscle exercises, aerobic activity) across demographics.

<img width="635" alt="image" src="https://github.com/user-attachments/assets/43e731c0-eb42-43d4-becc-45bc99b1e812" />


# Logistic Regression Model
**Statistical Question:** How do age, education, and physical activity predict BMI?
•	Null Hypothesis(H0): No statistically significant relationship between the predictors and BMI classification, βi=0.
•	Null Hypothesis(H1): Statistically significant relationship between the predictors and BMI classification βi not equals 0.
**Test:** Multiple regression analysis was conducted.
**Test Results:** All predictor coefficients were significantly different from zero (p < 0.05; see Figure B6), suggesting that age, education, and physical activity are valuable predictors of BMI.

<img width="470" alt="image" src="https://github.com/user-attachments/assets/400cbed8-fc37-461b-89cd-248bb2fbe78f" />


## Key Insights & Recommendations
The analysis provides a comprehensive look at factors influencing obesity in the U.S., highlighting:
1.	**Age:** Obesity significantly increases with age, with older adults (aged 45-54) exhibiting higher mean BMI than younger adults (aged 18-24). Targeted interventions for older populations are essential.
2.	**Education Level:** Higher obesity rates were observed among individuals without a college education. This implies, educational initiatives promoting healthier lifestyles could help reduce obesity in these groups.
3.	**Physical Activity:** A strong negative correlation exists between physical activity levels and BMI, indicating that increased physical activity is crucial for obesity prevention.

## Conclusion
The present analysis underscores that age, education level, and physical activity significantly influence obesity rates in the United States. Age emerged as a key predictor, with older adults showing higher obesity prevalence, consistent with national health statistics (Centers for Disease Control and Prevention [CDC], 2023). Educational attainment also correlated with obesity, as those without a college degree showed higher BMI levels, supporting the idea that educational interventions could play a role in promoting healthier lifestyles. Physical activity demonstrated a negative association with obesity, suggesting that it is an essential factor in managing body weight and reducing obesity-related health risks.

In conclusion, a holistic approach addressing both demographic and lifestyle factors is essential for combating obesity. These findings offer valuable insights for public health initiatives targeting age-specific and educationally-informed strategies to foster healthy habits and reduce the national burden of obesity.

**References - Datasets:**
1. Centers for Disease Control and Prevention. (n.d.). Nutrition, Physical Activity, and Obesity Behavioral Risk Factor Surveillance System (BRFSS). Chronic Data. https://chronicdata.cdc.gov/Nutrition-Physical-Activity-and-Obesity/Nutrition-Physical-Activity-and-Obesity-Behavioral/hn4x-zwk7 (Website as of October 3)

2. Centers for Disease Control and Prevention. (2023). 2023 BRFSS codebook.
https://www.cdc.gov/brfss/annual_data/2023/zip/codebook23_llcp-v2-508.zip

3. U.S. Government Open Data Portal. (n.d.). Nutrition, Physical Activity, and Obesity Behavioral Risk Factor Surveillance System. Data.gov. https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-behavioral-risk-factor-surveillance-system (Website as of October 3)

