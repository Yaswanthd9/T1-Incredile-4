# Libraries 

library (dplyr)
library (funModeling)
library(ezids)
library (ggplot2)
library(ggcorrplot)
library(corrr)

# Load the Data and Rename dataframe 
```{r}
read.csv("heart_2020_cleaned.csv")

# Renaming data frame 
heart <- rename (heart_2020_cleaned)

```
# Lets take a look at our data
```{r}
nrow (heart)
```


# Descrpitive Statistics  
```{r}
status(heart)
```

From the table we can see that Physical and Mental health have a higher percentage of zeros. This means that just over 70% of the people in our data set are not physically active while about 60% of the respondents do not report having mental health issues. 

We can also see that there are no N/A values in our data set. This is not surprising because our source data was cleaned before. 

# Now to see which variables are categorical and which are numerical 
```{r}
data_integrity(heart)
```
# Description of every variable 
```{r}
describe (heart)
```
# Finding the mean of numerical variables grouped by heart disease
```{r}
plot_num(heart)

heart %>%
  group_by(HeartDisease) %>%
  summarise_at(vars("BMI", 
                    "PhysicalHealth", 
                    "MentalHealth",
                    "SleepTime"), mean)
```
On average, people with heart disease have a slightly higher BMI than those who don't have heart disease. However, the biggest difference is the number of days that a respondent feels physically unwell. HD people reported feeling physically unwell about 8 days per month, while healthy people felt unwell for only 3.

# Frequency distributions of all our categorical variables 
```{r}
freq(heart+ scale_y_continuous(labels = scales::comma))
```


# Changing Diabetes bordeline and pregnancy categories to Yes/No
```{r}
heart$Diabetic<- replace (heart$Diabetic, heart$Diabetic== "No, borderline diabetes" , "Yes")
heart$Diabetic[heart$Diabetic== "Yes (during pregnancy)"] <- "No"  
```


#Checking if diabetic has been changed 
```{r}
describe (heart$Diabetic)
freq(heart$Diabetic)
```

# Changing all the categorical variables to numerical dummies 
```{r}
heart$HeartDisease<-ifelse(heart$HeartDisease=="Yes",1,0)
heart$Smoking<-ifelse(heart$Smoking=="Yes",1,0)
heart$AlcoholDrinking<-ifelse(heart$AlcoholDrinking=="Yes",1,0)
heart$Stroke<-ifelse(heart$Stroke=="Yes",1,0)
heart$DiffWalking<-ifelse(heart$DiffWalking=="Yes",1,0)
heart$PhysicalActivity<-ifelse(heart$PhysicalActivity=="Yes",1,0)
heart$KidneyDisease<-ifelse(heart$KidneyDisease=="Yes",1,0)
heart$SkinCancer<-ifelse(heart$SkinCancer=="Yes",1,0)
heart$Diabetic<-ifelse(heart$Diabetic=="Yes",1,0)

heart$Asthma<-ifelse(heart$Asthma=="Yes",1,0)
```

#Correlation Plots of all variables
```{r}
 model.matrix(~0+., data=heart) %>% 
     cor(use="pairwise.complete.obs") %>% 
   ggcorrplot(show.diag = F, type="upper", lab=TRUE, lab_size=2)
```

Need bigger screen to see this corrplot properly 

## Which gender is more likely to get heart disease? 

# Contingency Table for Sex & Heart Disease 

```{r}

contable = table(heart$HeartDisease, heart$Sex)
xkabledply(contable, title="Contingency Table for Heart Disease and Sex")
addmargins(contable)

prop <-prop.table(contable)
percent <- round(prop*100, 2)

print ("Contingency Table for Heart Disease and Sex in Percentage")
print (percent)
barplot(with(heart, table(HeartDisease, Sex)), main="Heart Disease & Sex", beside=T, col=3:2)
```

Green (No HD): more women
Red (HD): More men

# Subset Male & Female 
```{r}
female <- subset(heart, Sex== "Female")
male  <- subset (heart, Sex == "Male")
```

# Male Heart Disease Boxplot 
```{r}
ggplot(male, aes(x=HeartDisease, y=BMI)) + 
    geom_boxplot() +
    facet_wrap(~HeartDisease, scale="free") + 
    ggtitle("Male Heart Disease by BMI")
```   
# Female Heart Disease by BMI 

```{r}
ggplot(female, aes(x=HeartDisease, y=BMI)) + 
    geom_boxplot() +
    facet_wrap(~HeartDisease, scale="free") +
    ggtitle("Female Heart Disease by BMI")

```

# Are there differences between the means of the sexes 

```{r}

t.test(female$BMI, male$BMI)
```
The p-value is less than 0.05% significance level, allowing us to reject the null hypothesis and conclude that the overall average BMI is different between the male and female populations. 

# Is there a difference between the sex with heart disease 

```{r}
female_HD<- subset(female, HeartDisease==1)
male_HD<- subset(male, HeartDisease== 1)

t.test(female_HD$BMI, 
       male_HD$BMI)
```

We see that there is no difference in average BMI between the men and women who do have heart disease. 

# Heart Disease & Secondary Illnesses 

```{r}
diseases <- data.frame (heart$HeartDisease,
                        heart$Stroke,
                        heart$Diabetic,
                        heart$Asthma, 
                        heart$KidneyDisease,
                        heart$SkinCancer)

# Rename columns

names(diseases) <- c ('heart_disease',
                      'stroke',
                      'diabetic',
                      'asthma',
                      'kidney_disease',
                      'skin_cancer')


```

# Correlation matrix among diseases 

```{r}
model.matrix(~0+., data=diseases) %>%
      cor (use="pairwise.complete.obs")
     ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=1)

```
None of them are particularly strong. 

# Chi-test on diseases to check independence

```{r}

chi_asthma=chisq.test(table(heart$HeartDisease, heart$Asthma))
chi_asthma


chi_diabetic=chisq.test(table(heart$HeartDisease,heart$Diabetic))
chi_diabetic


chi_kidney=chisq.test(table(heart$HeartDisease,heart$KidneyDisease))
chi_kidney


chi_skin=chisq.test(table(heart$HeartDisease,heart$SkinCancer))
chi_skin


chi_stroke=chisq.test(table(heart$HeartDisease,heart$Stroke))
chi_stroke

```
Since the p-values are all less than the 5% significance level, we can reject the null and state that there are significant associations between between respondents with heart disease and those that also have one or more secondary illnesses. 

Why are all the p-values the same??? 

# Smokers Vs Drinkers 

```{r}
table_SQ3 <- xtabs(~heart$AlcoholDrinking+ heart$Smoking + heart$HeartDisease)
ftable(prop.table(table_SQ3))* 100

```
https://rstudio-pubs-static.s3.amazonaws.com/308591_6dff566d383946c881cb5b6a735a79fd.html

# Correlation coefficient 

```{r}
cor(heart$AlcoholDrinking,heart$Smoking)
cor(heart$AlcoholDrinking,heart$HeartDisease)
cor(heart$HeartDisease,heart$Smoking)

```


```{r}
chisq.test(heart$Smoking,  heart$HeartDisease)
chisq.test(heart$AlcoholDrinking,heart$HeartDisease)
```
