#Prashant Karki
#2023GSP_ANA_515P_01 Exp Practicum Fundamentals

library(tidyverse)
library(openxlsx)
library(DataExplorer)
library(rworldmap)
library(dplyr)
library(readxl)
options(warn=-1) #suppress warning


#Importing Dataset
df <- read_excel("C:/Users/pcity/Downloads/survey.xlsx",sheet=1)
df$Timestamp <- convertToDateTime(df$Timestamp) # as Timestamp column was read as double so I needed to convert to datetime
df1 <- read_excel("C:/Users/pcity/Downloads/survey.xlsx",sheet=2)

#combining two dataframe consisting of 2 sheets
data <-rbind(df, df1)
head(data)

# Print the dimensions of the dataframe, which is the number of rows and columns
data %>% dim

#Glimpse of the dataset
data %>% glimpse()
plot_intro(data)


#Data Cleaning and exploration
#Demographics Age, Gender, no of employees and Country

#Variable 1 - Age (Numerical Variable)¶
# Get the numerical summary (minimum, first quartile, median, mean, third quartile, and maximum) of Age in data 
summary(data$Age)
# Print the statistics of each factor
barplot(table(data$Age))


# How do we handle outliers?
# The aforementioned graph and numerical breakdown make it abundantly evident that the age answer contains outliers with meaningless values. The age of the subjects is anticipated to fall within the working range of 16-75 given the objective of the survey.
#  Outliers can frequently be handled in one of two ways.
# 1. Replace the outlier with the factor's sample median value.
# 2. Throw out the whole record (row).
#The 2nd method was chosen
# Print out the outliers
outlier_age <- subset(data[2:4], Age < 16 | Age > 75 )
nrow(outlier_age)
outlier_age

# Drop the outliers
age_clean <- subset(data, Age > 16 & Age <75)
dim(age_clean)


# Print the statistics of each factor
boxplot(age_clean$Age)
summary(age_clean$Age)

# How might age groups be made meaningful?
#   Based on working seniority, we can divide the subjects into numerous age groups:
#   (16 to 24) Fresh (Later Adolescence
#                     Early Adult (Junior): 25 to 34
#                     Adult (Middle Age): 35 to 60
#                     Super: >60 (Later Adulthood)

# Age categorization
age_clean$Age<-cut(age_clean$Age, breaks = c(16, 24, 34, 60, 75), labels = c('Fresh', 'Junior', 'Senior', 'Super'))
# Create the frequency table of age group
table(age_clean$Age)

# Create the relative frequency table of age
table(age_clean$Age)/length(age_clean$Age)

# Group by Age Group and count each group
age_group <- age_clean %>%
  group_by(Age) %>%
  dplyr::summarize(count = n())
age_group

# Visualize the number of subjects in each Age Group  
ggplot(age_group, aes(x = Age, y = count, fill = Age)) +  
  geom_bar(stat = "identity", alpha = 0.5) +
  xlab("Age Group") + 
  ylab("No of People") + 
  ggtitle("Comparing Age Group in the 2014 Mental Health in Tech Survey")


#Variable 2 - Gender (Categorical Variable)¶
# Save a copy of dataframe
gender_clean <- age_clean

# Improve consistence in gender identification by lower the case of entries
gender_clean$Gender <- tolower(gender_clean$Gender)
unique(gender_clean$Gender)
length(unique(gender_clean$Gender))


# How should we handle the gender disparity?
#   According to the replies, there are 40 different gender entries. The following ideas should be taken into account by gender categories in light of the diversity of the gender spectrum:
#   Cisgender people are those whose biological sex, gender identity, and gender expression perfectly line up.
# Transgender people are those whose gender identity differs from the gender they were given at birth.
# The word "genderqueer" (GQ), also known as "non-binary," "enby," or "fluid," refers to gender identities that are not entirely male or female and fall outside of the gender binary and cisnormativity.

# As such, let's apply the gender spectrum to group the 40 entries into five categories:
# 
# Cis-female: 'female', 'cis female', 'f', 'woman', 'femake', 'female ', 'cis-female/femme', 'female (cis)', 'femail'
# Cis-male: 'm', 'male', 'male-ish', 'maile', 'cis male', 'mal', 'male (cis)', 'make', 'male ', 'man', 'msle', 'mail', 'malr', 'cis man'
# Genderqueer: 'queer/she/they', 'non-binary', 'nah', 'enby', 'fluid', 'genderqueer', 'androgyne', 'agender', 'guy (-ish) ^_^', 'male leaning androgynous', 'neuter', 'queer', 'ostensibly male, unsure what that really means'
# Trans-female: 'trans-female', 'trans woman', 'female (trans)'
# Trans-male: 'something kinda male?'


# Create the list of five categories
cis_female <- c('female', 'cis female', 'f', 'woman', 'femake', 'female ', 'cis-female/femme', 'female (cis)', 'femail')
cis_male <- c('m', 'male', 'male-ish', 'maile', 'cis male', 'mal', 'male (cis)', 'make', 'male ', 'man', 'msle', 'mail', 'malr', 'cis man')
GQ <- c('queer/she/they', 'non-binary', 'nah', 'enby', 'fluid', 'genderqueer', 'androgyne', 'agender', 'guy (-ish) ^_^', 'male leaning androgynous', 'neuter', 'queer', 'ostensibly male, unsure what that really means')
trans_female <- c('trans-female', 'trans woman', 'female (trans)')
trans_male <- c('something kinda male?')

# Categorize genders
gender <- as.vector(gender_clean$Gender)
n <- length(gender)
for (i in 1:n){
  if (gender[i] %in% cis_female){
    gender[i] <- "cis-female"
  }else if (gender[i] %in% cis_male){
    gender[i] <- "cis-male"
  }else if (gender[i] %in% GQ){
    gender[i] <- "genderqueer"
  }else if (gender[i] %in% trans_female){
    gender[i] <- "trans-female"
  }else if (gender[i] %in% trans_male){
    gender[i] <- "trans-male"
  }else{
    gender[i]
  }
}

table(gender)


# Update the Gender column
gender_clean$Gender <- gender

# Create the frequency table of gender
table(gender_clean$Gender)

# Create the relative frequency table of gender
table(gender_clean$Gender)/length(gender_clean$Gender)


# Group by Gender and count each group
gender_diversity <- gender_clean %>%
  group_by(Gender) %>%
  dplyr::summarize(count = n())
gender_diversity

# Visualize the number of subjects in each gender type  
ggplot(gender_diversity, aes(x = Gender, y = count, fill = Gender)) +  
  geom_bar(stat = "identity", alpha = 0.5) +
  xlab("Gender Diversity") + 
  ylab("No of People") + 
  ggtitle("Comparing Gender Diversity in the 2014 Mental Health in Tech Survey")


#Variable 3: Country (Categorical Variable)¶
country_clean <- gender_clean
# Group by Country and count each group
country_count <- country_clean %>%
  group_by(Country) %>%
  dplyr::summarize(count = n())
country_count

# Print the number of countries where subjects come from
nrow(country_count)
# Draw a world map of the location distribution of subjects.
# First, map the country name in our dataframe to the countries in the worldmap 
country_map <- joinCountryData2Map(country_count, joinCode="NAME", nameJoinColumn="Country")

# Second, visualization of 1251 subjects by geographic location
mapCountryData(country_map, nameColumnToPlot="count", mapTitle="World" ,catMethod="categorical",
               colourPalette = "topo", missingCountryCol="grey", aspect =0)

# company size
#we assign no_employees <100 to be small company, 100-1000 to medium company, >= 1000 is large company for future categorical analysis.
country_clean$no_employees<- ifelse(country_clean$no_employees %in% c("1-5" , "6-25", "26-100"), "Small",
                       
ifelse(country_clean$no_employees %in% c("100-500","500-1000"), "Meduim", "Large"))
country_clean$no_employees <- country_clean$no_employees


# Group by no of employess and count each group
no_employees_count <- country_clean %>%
  group_by(no_employees) %>%
  dplyr::summarize(count = n())
no_employees_count


#Company size
msp6 <- ggplot(country_clean, aes(x=no_employees))+
  geom_bar(fill="violet")+
  
  labs(x="Company size", y="Count",
       
       title="Company Size Distribution")+ theme(legend.position="none") 
msp6
#Results
# Demographics: Descriptive Statistics
# The initial comprehension and cleaning of the three demographic variables—age, gender, and country—have been completed. Here is a succinct summary:
# There are 27 survey questions and 1259 records in the survey. We have 1251 useful records for further research after deleting 8 records of outliers (meaningless age).
# The subjects are 32 years old on average. Based on the employment seniority, we divide the age variable into four categories: fresh (16–24), junior (25–34), senior (35–60), and super (above 60). The distribution of subjects among the four age groups is as follows: freshmen (12.47%), juniors (56.51%), seniors (30.70%), and seniors (0.32%).
# The poll shows a very diverse representation of gender recognition. We group the reported gender into five basic categories: cisgender (both genders are present), transgender (both genders are present), genderqueer, and intersex. 19.74% of the respondents are cis-female, 78.82% are cis-male, 1.04% are genderqueer, 0.32% are transgender, and 0.08% are transgender men.
# The participants are from 46 different countries. US, UK, and Canada are the top three countries. To show the location of the 1251 useful subjects globally, we generate a heat map.
#There are 724 large companies, 233 medium and 284 small companies

final_clean <- country_clean
write.csv(final_clean, "cleaned_data.csv") 
