#load required packages
library(tidyverse)
library(tidymodels)
library(readxl)
#Cramer's V
library(sjstats)
#MCA
library(FactoMineR)
library(factoextra)

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

remove(demographics)
remove(concerns)
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

#Demographics Exploration --------------------
#how worried are participants about drug decriminalisation, more not concerned
ggplot(data = all_data_amended, aes(x = WORRIED)) +
  geom_bar() +
  labs(x = NULL, 
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
                     values = c("DarkRed", "Grey",  "DarkBlue")) +
  scale_x_discrete(labels = c("Tougher drugs law", "Same drugs law", "Neutral",
                              "Some drugs decriminalised", "All drugs decriminalised")) +
  theme_bw() +
  theme(legend.position="top")

#how do demographics affect opinions?
#age first, generally older people more concerned
ggplot(data = all_data_amended, aes(x = AGERAW, fill = WORRIED)) +
  geom_bar(position = "fill") +
  labs(x = NULL, 
       y = "Participant %", 
       title = "Decriminalisation Concern by Age",
       fill = NULL) +
  scale_fill_manual(labels = c("Concerned", "Neutral", "Not concerned"),
                    values = c("DarkRed", "Grey",  "DarkBlue")) +
  theme_bw() +
  theme(legend.position="top")

#less linear trend, 26-35/56-65 look surprisingly similar
ggplot(data = all_data_amended, aes(x = AGERAW, fill = LAW)) +
  geom_bar(position = "fill") +
  labs(x = NULL, 
       y = "Participant %", 
       title = "Drug Law Opinion by Age",
       fill = NULL) +
  scale_fill_manual(labels = 
                      c("Tougher drugs law", "Same drugs law", "Neutral",
                        "Some drugs decriminalised", "All drugs decriminalised"),
                    values = c("DarkRed", "Orange", "Grey", "DarkGreen", "DarkBlue")) +
  theme_bw() +
  theme(legend.position="right")

#gender, not that different
ggplot(data = all_data_amended %>% filter(GENDERRAW %in% c("Female", "Male")), 
       aes(x = GENDERRAW, fill = WORRIED)) +
  geom_bar(position = "fill") +
  labs(x = NULL, 
       y = "Participant %", 
       title = "Decriminalisation Concern by Gender",
       fill = NULL) +
  scale_fill_manual(labels = c("Concerned", "Neutral", "Not concerned"),
                    values = c("DarkRed", "Grey",  "DarkBlue")) +
  theme_bw() +
  theme(legend.position="top")

ggplot(data = all_data_amended  %>% filter(GENDERRAW %in% c("Female", "Male")), 
       aes(x = GENDERRAW, fill = LAW)) +
  geom_bar(position = "fill") +
  labs(x = NULL, 
       y = "Participant %", 
       title = "Drug Law Opinion by Gender",
       fill = NULL) +
  scale_fill_manual(labels = 
                      c("Tougher drugs law", "Same drugs law", "Neutral",
                        "Some drugs decriminalised", "All drugs decriminalised"),
                    values = c("DarkRed", "Orange", "Grey", "DarkGreen", "DarkBlue")) +
  theme_bw() +
  theme(legend.position="right")

#education level, v.clear trend
ggplot(data = all_data_amended, aes(x = fct_reorder2(EDUCATIONRAW, EDUCATION, EDUCATION, .desc = FALSE),
                                    fill = WORRIED)) +
  geom_bar(position = "fill") +
  labs(x = NULL, 
       y = "Participant %", 
       title = "Decriminalisation Concern by Education Level",
       fill = NULL) +
  scale_fill_manual(labels = c("Concerned", "Neutral", "Not concerned"),
                    values = c("DarkRed", "Grey",  "DarkBlue")) +
  theme_bw() +
  theme(legend.position="top")


ggplot(data = all_data_amended, aes(x = fct_reorder2(EDUCATIONRAW, EDUCATION, EDUCATION, .desc = FALSE), 
                                     fill = LAW)) +
  geom_bar(position = "fill") +
  labs(x = NULL, 
       y = "Participant %", 
       title = "Drug Law Opinion by Education Level",
       fill = NULL) +
  scale_fill_manual(labels = 
                      c("Tougher drugs law", "Same drugs law", "Neutral",
                        "Some drugs decriminalised", "All drugs decriminalised"),
                    values = c("DarkRed", "Orange", "Grey", "DarkGreen", "DarkBlue")) +
  theme_bw() +
  theme(legend.position="right")

#political leaning, left wing a lot different to rest

all_data_amended$LEANINGRAW <- factor(all_data_amended$LEANINGRAW, 
                                      levels = 
                                        c("Non political",
                                          "Left wing",
                                          "Centre",
                                          "Right wing"
                                          ))

ggplot(data = all_data_amended, aes(x = LEANINGRAW, fill = WORRIED)) +
  geom_bar(position = "fill") +
  labs(x = NULL, 
       y = "Participant %", 
       title = "Decriminalisation Concern by Political Leaning",
       fill = NULL) +
  scale_fill_manual(labels = c("Concerned", "Neutral", "Not concerned"),
                    values = c("DarkRed", "Grey",  "DarkBlue")) +
  theme_bw() +
  theme(legend.position="top")

ggplot(data = all_data_amended, aes(x = LEANINGRAW, fill = LAW)) +
  geom_bar(position = "fill") +
  labs(x = NULL, 
       y = "Participant %", 
       title = "Drug Law Opinion by Political Leaning",
       fill = NULL) +
  scale_fill_manual(labels = 
                      c("Tougher drugs law", "Same drugs law", "Neutral",
                        "Some drugs decriminalised", "All drugs decriminalised"),
                    values = c("DarkRed", "Orange", "Grey", "DarkGreen", "DarkBlue")) +
  theme_bw() +
  theme(legend.position="right")

#try Chi squared test of independence
# two categorical variables and independence of groups is fulfilled
# would need to check expected frequencies are above 1 and 80% of expected
#  frequencies are above 5

#all of these are significant but are returning error due to low frequencies
#chisq_test(all_data_amended, WORRIED ~ EDUCATION)
#chisq_test(all_data_amended, LAW ~ EDUCATION)
#chisq_test(all_data_amended, WORRIED ~ AGE)
#chisq_test(all_data_amended, LAW ~ AGE)
#for comparison this one isn't significant
#chisq_test(all_data_amended, WORRIED ~ GENDER)

#try Fisher Exact test instead, all are significant
fisher1 <- fisher.test(x = all_data_amended$EDUCATION,
            y = all_data_amended$WORRIED,
            simulate.p.value = TRUE)

fisher2 <- fisher.test(x = all_data_amended$EDUCATION,
            y = all_data_amended$LAW,
            simulate.p.value = TRUE)


fisher3 <- fisher.test(x = all_data_amended$AGE,
            y = all_data_amended$WORRIED,
            simulate.p.value = TRUE)

fisher4 <- fisher.test(x = all_data_amended$AGE,
                       y = all_data_amended$LAW,
                       simulate.p.value = TRUE)

#wonder about political leaning where one group was different
#yes both p are significant

fisher5 <- fisher.test(x = all_data_amended$LEANING,
            y = all_data_amended$WORRIED,
            simulate.p.value = TRUE)

fisher6 <- fisher.test(x = all_data_amended$LEANING,
                       y = all_data_amended$LAW,
                       simulate.p.value = TRUE)

fisher7 <- fisher.test(x = all_data_amended$GENDER,
                       y = all_data_amended$WORRIED,
                       simulate.p.value = TRUE)

fisher8 <- fisher.test(x = all_data_amended$GENDER,
                       y = all_data_amended$LAW,
                       simulate.p.value = TRUE)



#this version of Cramer's V can use fisher test instead of Chi-square
# if expected values are too low
# first two are marginal, second two are moderate, final two moderate
cramer1 <- cramers_v(WORRIED ~ EDUCATION, data = all_data_amended, 
          statistics = "cramer")

cramer2 <- cramers_v(LAW ~ EDUCATION, data = all_data_amended, 
          statistics = "cramer")

cramer3 <- cramers_v(WORRIED ~ AGE, data = all_data_amended, 
          statistics = "cramer")

cramer4 <- cramers_v(LAW ~ AGE, data = all_data_amended, 
                     statistics = "cramer")

cramer5 <- cramers_v(WORRIED ~ LEANING, data = all_data_amended, 
                     statistics = "cramer")

cramer6 <- cramers_v(LAW ~ LEANING, data = all_data_amended, 
       statistics = "cramer")

cramer7 <- cramers_v(LAW ~ GENDER, data = all_data_amended , 
                     statistics = "cramer")

cramer8 <- cramers_v(WORRIED ~ GENDER, data = all_data_amended , 
                     statistics = "cramer")

#put fisher / cramer results in dataframe
Pairings <- c("Concern/EducationLevel", "DrugPolicy/EducationLevel", 
            "Concern/Age","DrugPolicy/Age", 
            "Concern/PoliticalLeaning", "DrugPolicy/PoliticalLeaning",
            "Concern/Gender", "DrugPolicy/Gender")
Fisher_exact <- c(fisher1$p.value, fisher2$p.value, fisher3$p.value,
             fisher4$p.value, fisher5$p.value, fisher6$p.value,
             fisher7$p.value, fisher8$p.value)
Cramer_v <- c(cramer1, cramer2, cramer3, cramer4, cramer5, cramer6,
              cramer7, cramer8)

fisher_cramer_list <- data.frame(Pairings, Fisher_exact, Cramer_v)

fisher_cramer_list$Cramer_v <- round(fisher_cramer_list$Cramer_v, 2)
fisher_cramer_list$Fisher_exact <- round(fisher_cramer_list$Fisher_exact, 5)

#Full exploration ------------------------
#We've seen moderate relationships between age/politics and drug policy,
#  weak/moderate relationship between education/drug policy and no
#  link between gender/drug policy

#What about the rest of the answers?
#reduce dataset to remove repeated demographics and underrepresented genders

all_data_amended_reduced <- all_data_amended %>% 
  select(AGE:WORRIED) %>% 
  filter(GENDER %in% c(1, 2)) %>% 
  relocate(LAW, .after = LEANING) %>% 
  relocate(WORRIED, .after = LAW) 

#all variables
#not much variation explained by 2 dimensions
#worried is highly correlated with first dimension
#leniency, police and trial are important in first 2 dimensions
#none of the demographics are highly correlated with first two dimensions
mca <- MCA(all_data_amended_reduced, quali.sup = 1:6)

#shows different data we can retrieve 
print(mca)

#this shows how important variables are to each dimension
View(mca$var$eta2)

#see how much variance is explained by each dimension, not much
fviz_screeplot(mca, addlabels = TRUE, ylim = c(0,20))

#age 6 and education 7 are far from the origin meaning they 
# are discriminatory attributes
# this makes sense as graphs show they had very different
# views to different age/education groups 
plot(mca, cex = 0.7, invisible = c("ind"))

#alternative way of plotting biplot, only 10 largest cos2
#older participants answer 1 to police/leniency
fviz_mca_var(mca, select.var = list(cos2 = 10), repel = TRUE)

#bar chart of 10 variables most explained by 2 dimensions
fviz_cos2(mca, choice = "var", axes = 1:2, top = 10)

#summarises the correlation between supplementary variables and dimensions
mca$quali.sup$eta2

#Idea: because worried is highly correlated with first dimension, 
# use age/political and top 3 contributing to first dim (LENIENCY, TRIAL, POLICE)
# to predict concern based on 5 variables
# Try multinomial regression using tidymodels to compare

# Multinomial logistic regression

# split data into test train
set.seed(1001) 
split <- initial_split(all_data_amended_reduced, strata = WORRIED) 
train <- training(split) 
test <- testing(split) 

#cross validation for tuning
train_folds <- vfold_cv(train, v = 5, strata = WORRIED) 

#try these formulas for a start
formulas <- list(
  all = recipe(WORRIED ~ LENIENCY + TRIAL + POLICE + LEANING + AGE,
               data = train) %>% 
    step_dummy(all_nominal_predictors()),
  one_demographic = recipe(WORRIED ~ LENIENCY + TRIAL + POLICE + LEANING, 
                           data = train) %>% 
    step_dummy(all_nominal_predictors()),
  two_questions = recipe(WORRIED ~ LENIENCY + TRIAL + LEANING + AGE,
                         data = train) %>% 
    step_dummy(all_nominal_predictors()),
  two_questions_one_demographic = recipe(WORRIED ~ LENIENCY + TRIAL + LEANING,
                         data = train) %>% 
    step_dummy(all_nominal_predictors()),
  one_question_one_demographic = recipe(WORRIED ~ LENIENCY + LEANING,
                                         data = train) %>% 
    step_dummy(all_nominal_predictors())
)

#try this model for a start
glmnet_model <- 
  multinom_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

#try these values for penalty and mixture
penalty_grid <- 10^seq(1, -2, length = 10) %>% as.data.frame() %>%
  rename("penalty" = ".")
penalty_grid$mixture <- seq(0, 1, length = 10)
                            
#define workflow set
workflows <- workflow_set(preproc = formulas, models = list(
  glmnet = glmnet_model))

#tune across workflows
workflows <- workflows %>%
  workflow_map(resamples = train_folds, 
               verbose = TRUE, 
               grid = penalty_grid,
               seed = 1001,
               metrics = metric_set(accuracy),
               control = control_grid(
                 save_workflow = TRUE
               ))

remove(models)
  
#have a look at the average accuracy across all folds and workflows
#View(collect_metrics(models) %>% filter(.metric == "accuracy"))

#best was accuracy around 0.65
autoplot(workflows)

#rank best of each workflow, 
#not much difference betweendifferent number of predictors
View(workflows %>% 
  rank_results(rank_metric = "accuracy", select_best = TRUE) %>% 
  filter(.metric == "accuracy"))

#choose simplest model and get tuned parameters
best_results <- workflows %>%
  extract_workflow_set_result("all_glmnet") %>% 
  select_best(metric = "accuracy")

#fit on whole train set to see if assumptions are fulfilled

#fit on test data
test_results <- 
  models %>% 
  extract_workflow("all_glmnet") %>% 
  finalize_workflow(best_results) %>% 
  last_fit(split = split)

#60% accuracy on test set
collect_metrics(test_results)

#plot actual vs predicted
test_results %>% 
  collect_predictions() %>%
  ggplot(aes(x = WORRIED, fill = .pred_class)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Actual concern", fill = "Predicted concern")

#look at predictions vs answers 
train_augment <- augment(test_results) %>%
  relocate(WORRIED, .after = .pred_class) %>%
  relocate(LENIENCY, .after = WORRIED) %>%
  relocate(TRIAL, .after = LENIENCY) %>%
  relocate(POLICE, .after = TRIAL) %>%
  relocate(LEANING, .after = POLICE) %>%
  relocate(AGE, .after = LEANING)

#To Do
# understand more about penalty and mixture in multinomial logistic regression
# understand which other Parsnip models could be used for this problem
# document the assumptions for multinomial logistic regression
# document classification metrics in OneNote
# document new formulas in OneNote
# add findings to README


