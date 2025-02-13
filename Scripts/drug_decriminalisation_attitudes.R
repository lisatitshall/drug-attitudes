#load required packages
library(tidyverse)
library(tidymodels)
library(readxl)

#Data Notes
# AGE:PARTY columns are demographics questions
# HARDSOFT:MORALITY are 9 Likert questions (where 5 means favour decriminalisation)
# RESOURCES:OTHERNOCONCERN select all question about no concern
# CRIME RATE:OTHERCONCERN select all question about concern
# LAW is explicitly asked decriminalisation question(Likert)
# WORRIED? is concern about drug decriminalisation 

#Aim
# Predict LAW/WORRIED category based on other answers
# Analyse demographics impact on LAW/WORRIED answers (if any)

#import data from xlsx
demographics <- 
  read_excel("Data/Demographics_and_Attitude_Towards_Drug_Decriminalisation_Policy_Data_Set.xlsx")

concerns <- 
  read_excel("Data/Concerns_Toward_Drug_Decriminalisation_and_Demographics_Data_Set.xlsx")

#join datasets, ID 1703SM isn't matching when it should
#had to fix problem with Law numbers not matching up across two datasets
all_data_raw <- inner_join(demographics, concerns, 
                           by = c("MEMORABLE ID", "AGE", "GENDER", "EDUCATION", "LEANING", "LAW"), 
                           keep = FALSE )

#rename other columns and ones with unacceptable punctuation
all_data_raw <- all_data_raw %>% 
  rename("OTHERNOCONCERN" = `OTHER...14`,
         "OTHERCONCERN" = `OTHER...20`, 
         "USESELLING" = `USESELLING*`,
         "PRISONTREATMENT" = `PRISONTREATMENT*`,
         "MORALITY" = `MORALITY*`,
         "CRIMERATE" = `CRIME RATE`, 
         "NOPRISON" = `NO PRISON`,
         "WORRIED" = `WORRIED?`)

#rearrange columns
all_data_raw <- all_data_raw %>% 
  relocate(PARTY, .after = LEANING) %>%
  relocate(LAW, .after = OTHERCONCERN) %>% 
  relocate(WORRIED, .after = LAW)

#remove row with null value
all_data_amended <- na.omit(all_data_raw)

#remove favourability score for now, not convinced they should include
# the law question
all_data_amended <- all_data_amended %>% select(!starts_with("Favourability"))

#don't need ID column any more
all_data_amended <- all_data_amended %>% select(AGE:WORRIED)

#check datatypes
glimpse(all_data_amended)

#everything should be a factor instead of a double
all_data_amended <- as.data.frame(sapply(all_data_amended, as.factor), 
                                  stringsAsFactors = TRUE)

#ok
glimpse(all_data_amended)

#understand demographics

#add age group 
all_data_amended$AGERAW <- as.factor(
  case_when(
    all_data_amended$AGE == "1" ~ "18-25",
    all_data_amended$AGE == "2" ~ "26-35",
    all_data_amended$AGE == "3" ~ "36-45",
    all_data_amended$AGE == "4" ~ "46-55",
    all_data_amended$AGE == "5" ~ "56-65",
    all_data_amended$AGE == "6" ~ "66+"
))

#apart from 66+ (arguably) enough of each
plot(all_data_amended$AGERAW, xlab = "Age Groups", ylab = "Participants",
     main = "Age Breakdown")

#add gender
all_data_amended$GENDERRAW <- as.factor(
  case_when(
    all_data_amended$GENDER == "1" ~ "Male",
    all_data_amended$GENDER == "2" ~ "Female",
    all_data_amended$GENDER == "3" ~ "Non-binary",
    all_data_amended$GENDER == "4" ~ "Other"
  ))


#women overrepresented
plot(all_data_amended$GENDERRAW, xlab = "Gender", ylab = "Participants",
     main = "Gender Breakdown")

#lots of ethnicities and not easy to combine so remove this demographic question
plot(all_data_amended$ETHNICITY, xlab = "Ethnicities", ylab = "Participants",
     main = "Ethnicity Breakdown")

all_data_amended <- all_data_amended %>% select(!ETHNICITY)

#add education column
all_data_amended$EDUCATIONRAW <- as.factor(
  case_when(
    all_data_amended$EDUCATION == "1" ~ "No schooling",
    all_data_amended$EDUCATION == "2" ~ "Primary school",
    all_data_amended$EDUCATION == "3" ~ "Secondary school",
    all_data_amended$EDUCATION == "4" ~ "College",
    all_data_amended$EDUCATION == "5" ~ "Undergraduate",
    all_data_amended$EDUCATION == "6" ~ "Masters",
    all_data_amended$EDUCATION == "7" ~ "PhD"
  ))

#mostly undergrads, only a few PhD's, OK spread
plot(all_data_amended$EDUCATION, xlab = "Education", ylab = "Participants",
     main = "Education Breakdown")

#add political leaning column
all_data_amended$LEANINGRAW <- as.factor(
  case_when(
    all_data_amended$LEANING == "1" ~ "Non political",
    all_data_amended$LEANING == "2" ~ "Left wing",
    all_data_amended$LEANING == "3" ~ "Centre",
    all_data_amended$LEANING == "4" ~ "Right wing"
  ))

#not many right wing, imagine this would affect overall views!
plot(all_data_amended$LEANINGRAW, xlab = "Political Leaning", ylab = "Participants",
     main = "Political Leaning Breakdown")

#can't be 100% sure of party order because not in data dictionary
# but have political leaning so this will suffice, remove party
plot(all_data_amended$PARTY, xlab = "Political Party", ylab = "Participants",
     main = "Political Party Breakdown")

all_data_amended <- all_data_amended %>% select(!PARTY)

#Exploration --------------------
#how worried are participants about drug decriminalisation, more not concerned
ggplot(data = all_data_amended, aes(x = WORRIED)) +
  geom_bar() +
  labs(x = "Worried", 
       y = "Participants",
       title = "Drug Decriminalisation Concern") +
  scale_x_discrete(labels = c("Concerned", "Neutral", "Not concerned")) +
  theme_bw()

#what do participants think about the law, more favour liberalisation
ggplot(data = all_data_amended, aes(x = LAW)) +
  geom_bar() +
  labs(x = NULL, 
       y = "Participants",
       title = "Opinion of Drugs Law") +
  scale_x_discrete(labels = c("Tougher drugs law", "Same drugs law", "Neutral",
                              "Some drugs decriminalised", "All drugs decriminalised")) +
  theme_bw()

#will be interesting to see if LAW and WORRIED correlate
#expect 1/2 law to map to 1 concern, 3 law to 2 concern, 4/5 law to 3 concern
#100% stacked bar
ggplot(data = all_data_amended, aes(x = LAW, fill = WORRIED)) +
  geom_bar(position = "fill") +
  labs(x = NULL, 
       y = "Participant %", 
       title = "Comparing Drug Law Opinion to Decriminalisation Concern",
       fill = NULL) +
  scale_fill_manual(labels = c("Concerned", "Neutral", "Not concerned"),
                     values = c("Orange", "Grey",  "Blue")) +
  scale_x_discrete(labels = c("Tougher drugs law", "Same drugs law", "Neutral",
                              "Some drugs decriminalised", "All drugs decriminalised")) +
  theme_bw() +
  theme(legend.position="top")

