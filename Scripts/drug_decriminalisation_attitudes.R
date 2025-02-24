#load required packages
library(tidyverse)
library(tidymodels)
library(readxl)
#Cramer's V
library(sjstats)
#MCA
library(FactoMineR)
library(factoextra)
#tree models parsnip
library(bonsai)
#ordinal linear regression
library(MASS)
library(Hmisc)
#variance inflation factors
library(car)

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
all_data_amended <- all_data_amended %>% dplyr::select(!starts_with("Favourability"))

#don't need ID column any more
all_data_amended <- all_data_amended %>% dplyr::select(AGE:WORRIED)

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

all_data_amended <- all_data_amended %>% dplyr::select(!ETHNICITY)

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

all_data_amended <- all_data_amended %>% dplyr::select(!PARTY)

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

#Multiple Correspondence Analysis ------------------------
#We've seen moderate relationships between age/politics and drug policy,
#  weak/moderate relationship between education/drug policy and no
#  link between gender/drug policy

#What about the rest of the answers?
#reduce dataset to remove repeated demographics and underrepresented genders

all_data_amended_reduced <- all_data_amended %>% 
  dplyr::select(AGE:WORRIED) %>% 
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

#Multinomial logistic regression ---------------------------

# Multinomial logistic regression - can be used for ordinal but will try 
# ordinal approach as well

#change variables to ordered factors
all_data_amended_reduced$LENIENCY <- factor(all_data_amended_reduced$LENIENCY, 
                                            ordered = TRUE, 
                                            levels = c("5", "4", "3", "2","1"))

all_data_amended_reduced$TRIAL <- factor(all_data_amended_reduced$TRIAL,
                                            ordered = TRUE,
                                         levels = c("5", "4", "3", "2","1"))

all_data_amended_reduced$POLICE <- factor(all_data_amended_reduced$POLICE,
                                         ordered = TRUE, 
                                         levels = c("5", "4", "3", "2","1"))

all_data_amended_reduced$AGE <- factor(all_data_amended_reduced$AGE,
                                          ordered = TRUE)
#reverse order of worried factor
all_data_amended_reduced$WORRIED <- factor(all_data_amended_reduced$WORRIED,
                                           levels = c("3", "2", "1")
                                           )


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
    step_dummy(LEANING) %>%
    step_ordinalscore(LENIENCY, TRIAL, POLICE, AGE),
  one_demographic = recipe(WORRIED ~ LENIENCY + TRIAL + POLICE + LEANING, 
                           data = train) %>% 
    step_dummy(LEANING) %>%
    step_ordinalscore(LENIENCY, TRIAL, POLICE),
  two_questions = recipe(WORRIED ~ LENIENCY + TRIAL + LEANING + AGE,
                         data = train) %>% 
    step_dummy(LEANING) %>%
    step_ordinalscore(LENIENCY, TRIAL, AGE),
  two_questions_one_demographic = recipe(WORRIED ~ LENIENCY + TRIAL + LEANING,
                         data = train) %>% 
    step_dummy(LEANING) %>%
    step_ordinalscore(LENIENCY, TRIAL),
  one_question_one_demographic = recipe(WORRIED ~ LENIENCY + LEANING,
                                         data = train) %>% 
    step_dummy(LEANING) %>%
    step_ordinalscore(LENIENCY)
)

#try these models for a start
glmnet_model <- 
  multinom_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

tree_model <-
  decision_tree(min_n = 10) %>%
  set_engine("partykit") %>%
  set_mode("classification")
  

#try these values for penalty and mixture
penalty_grid <- 10^seq(1, -2, length = 10) %>% as.data.frame() %>%
  rename("penalty" = ".")
penalty_grid$mixture <- seq(0, 1, length = 10)
                            
#define workflow set
workflows <- workflow_set(preproc = formulas, models = list(
  glmnet = glmnet_model, tree = tree_model))

#tune across workflows
workflows <- workflows %>%
  workflow_map(resamples = train_folds, 
               verbose = TRUE, 
               grid = penalty_grid,
               seed = 1001,
               metrics = metric_set(accuracy, kap),
               control = control_grid(
                 save_workflow = TRUE
               ))
  
#have a look at the average accuracy across all folds and workflows
#View(collect_metrics(models) %>% filter(.metric == "accuracy"))

#best was accuracy around 0.68
autoplot(workflows)

#rank best of each workflow, 
#not much difference between different number of predictors
#trees are performing worse
#use kappa because classes are imbalanced
View(workflows %>% 
  rank_results(rank_metric = "kap", select_best = TRUE) %>% 
  filter(.metric == "kap"))

#choose best model and get tuned parameters
best_results <- workflows %>%
  extract_workflow_set_result("all_glmnet") %>% 
  select_best(metric = "kap")

#fit on test data
test_results <- 
  workflows %>% 
  extract_workflow("all_glmnet") %>% 
  finalize_workflow(best_results) %>% 
  last_fit(split = split)

tree_test_results <- 
  workflows %>% 
  extract_workflow("all_tree") %>% 
  last_fit(split = split)

#extract fit
fit <- test_results %>%
       extract_fit_parsnip("two_questions_glmnet")

fit2 <- tree_test_results %>%
  extract_fit_parsnip("all_tree")

#this gives the coefficients of the equations
View(tidy(fit) %>% filter(estimate != 0) %>% filter(term != "(Intercept)"))

#this gives all coefficients on the regularization path
#View(tidy(fit$fit))

table(test_augment$WORRIED, test_augment$.pred_class)

#60% accuracy on test set, 31% kappa (only fair)
collect_metrics(test_results)
kap(test_augment, truth = WORRIED, estimate = .pred_class)

#plot actual vs predicted
test_results %>% 
  collect_predictions() %>%
  ggplot(aes(x = WORRIED, fill = .pred_class)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Actual concern", fill = "Predicted concern")

#look at predictions vs answers 
test_augment <- augment(test_results) %>%
  relocate(WORRIED, .after = .pred_class) %>%
  relocate(LENIENCY, .after = WORRIED) %>%
  relocate(TRIAL, .after = LENIENCY) %>%
  relocate(POLICE, .after = TRIAL) %>%
  relocate(LEANING, .after = POLICE) %>%
  relocate(AGE, .after = LEANING)

#plot best prediction against whether it was correct or not
test_augment <- test_augment %>% 
  mutate(best_pred = pmax(.pred_1,.pred_2,.pred_3)) %>%
  mutate(correct = ifelse(.pred_class == WORRIED, TRUE, FALSE))


plot(test_augment$best_pred, test_augment$correct)


#To Do
# try ordinal logistic regression with same variables as winning multinomial
#   model, Parsnip doesn't include this

#Ordinal logistic regression ---------------------------

#first assess whether variables are correlated, 
#need to be uncorrelated to use ordinal logistic regression
#assess using plots, Chi Square/Fisher(+ Cramer)

#All variables are correlated with each other but some 
# to a lesser extent
ggplot(data = all_data_amended, aes(x = AGE, fill = LEANING)) +
  geom_bar(position = "fill") +
  labs(x = NULL, 
       y = "Participant %",
       fill = NULL) +
  scale_fill_manual(
                    values = c("DarkRed", "Orange", "Grey", "DarkGreen", "DarkBlue")) +
  theme_bw() +
  theme(legend.position="right")


chisq_test(all_data_amended, LEANING ~ AGE)
fisher.test(x = all_data_amended$LEANING,
            y = all_data_amended$AGE,
            simulate.p.value = TRUE)
cramers_v(LEANING ~ AGE, data = all_data_amended, 
          statistics = "cramer")

#test proportional odd assumption
#this calculates the log odds of being greater than or equal to
# each value of the target variable
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

#this gives coefficient values without parallel lines assumption
#for each variable we want to check if the differences between 
# Y >= 2 and Y>= 3 are the same
#LENIENCY 1.07, 1.03, 1.43, 0.4 (has -inf value)
#TRIAL 1.06, 0.49, 2.2, 1.32, 0.52
#POLICE 1.8, 1.64, 0.8, 0.67, 0
#AGE 2.1, 0.18, 1.15, 0.73, 0.6, 0.52
#LEANING 1.11, 0.76, 0.79, 0.62
(s <- with(all_data_amended, 
           summary(as.numeric(WORRIED) ~ 
                     TRIAL + POLICE + AGE + LEANING, fun=sf)))

#plot this
#LEANING just about OK, rest aren't good
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
plot(s, which=1:3, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))

#ordinal logistic regression model can't be used but here's the code

#ordinal logistic regression model
olr_model <- polr(WORRIED ~ LENIENCY + TRIAL + POLICE + LEANING + AGE,
                  data = train, Hess = TRUE)

#show coefficients, t-values, intercepts and deviance/AIC
summary(olr_model)

#confidence intervals
#no sign change: LENIENCY 4, TRIAL L, POLICE L, AGE L
(ci <- confint(olr_model))

#get odds ratios
exp(coef(olr_model))

#check variance inflation factors, leniency/trial are too large
vif(olr_model)
vif(lm(as.numeric(WORRIED) ~ LENIENCY + TRIAL + POLICE + LEANING + AGE,
         data = train))

#OK with TRIAL removed
vif(polr(WORRIED ~ LENIENCY  + POLICE + LEANING + AGE,
       data = train, Hess = TRUE))
vif(lm(as.numeric(WORRIED) ~ LENIENCY  + POLICE + LEANING + AGE,
       data = train))


#ordinal logistic regression model with no TRIAL
olr_model_2 <- polr(WORRIED ~ LENIENCY + POLICE + LEANING + AGE,
                  data = train, Hess = TRUE)

#show coefficients, t-values, intercepts and deviance/AIC
summary(olr_model_2)

#confidence intervals
#no sign change: LENIENCY.L, LENIENCY^4, POLICE.L
(ci_2 <- confint(olr_model_2))

#get odds ratios
exp(coef(olr_model_2))

#predict on test data
olr_predicted <- predict(olr_model_2, test)

#get classification matrix
table(test$WORRIED, olr_predicted)

#add predicted to new test dataframe for plotting
olr_test <- test
olr_test$PRED <- olr_predicted
ggplot(data = olr_test, aes(x = WORRIED, fill = PRED)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Actual concern", fill = "Predicted concern")

#64% accuracy 
accuracy(olr_test, truth = WORRIED, estimate = PRED)

#35% kappa
kap(olr_test, truth = WORRIED, estimate = PRED)

#Ideas
# Could we have two groups "concerned" and "not concerned" and look
#   at the demographic association only? 
# Are age/political leaning still significant when we include 
#  education level (a socioeconomic factor)?

#Binary logistic regression ---------------------------

#smaller dataset with only demographics and concern
all_data_amended_reduced_demographics_only <-
  all_data_amended_reduced  %>% dplyr::select(GENDER, AGE, LEANING, 
                                              EDUCATION, WORRIED)

#check whether factors are ordered
# in this case it's better for them not to be
glimpse(all_data_amended_reduced_demographics_only)

#un-order the age variable
all_data_amended_reduced_demographics_only$AGE <- factor(
  all_data_amended_reduced_demographics_only$AGE, ordered = FALSE)

#change the order of education because we'd expect PhD's to be 
# less concerned
all_data_amended_reduced_demographics_only$EDUCATION <- factor(
  all_data_amended_reduced_demographics_only$EDUCATION, 
  levels = c("7", "6", "5", "4", "3"))

#change neutral and no concern to 0
all_data_amended_reduced_demographics_only$WORRIED <- case_when(
  all_data_amended_reduced_demographics_only$WORRIED == "2" ~ 0,
  all_data_amended_reduced_demographics_only$WORRIED == "3" ~ 0,
  .default = 1)


#binary logistic regression
binary_model <- glm(WORRIED ~ EDUCATION + GENDER + AGE + LEANING, 
                    family = "binomial",
                    data = all_data_amended_reduced_demographics_only)

#summary of coefficients
# significant - AGE 4-6
summary(binary_model)

#odds ratios
exp(binary_model$coefficients)

#confidence intervals for plotting
confidence_intervals <- exp(confint(binary_model))

confidence_intervals <- data.frame(group = row.names(confidence_intervals),
                                   confidence_intervals) %>%
  rename("LowerBound" = "X2.5..") %>% rename("UpperBound" = "X97.5..")

confidence_intervals$LowerBound <- round(confidence_intervals$LowerBound, 2) 
confidence_intervals$UpperBound <- round(confidence_intervals$UpperBound, 2) 


#lollipop of confidence intervals
ggplot(data = confidence_intervals %>% filter(group != "(Intercept)")) + 
  geom_segment(aes(x=group, y=LowerBound, 
                   yend=ifelse(UpperBound < 50, UpperBound, 50)), 
               color="grey") + 
  geom_point( aes(x = group, y=LowerBound), col = "blue") + 
  geom_point( aes(x = group, y=UpperBound), col = "orange" ) + 
  coord_flip() +
  ylim(0,50) +
  geom_abline(intercept = 1, slope = 0, linewidth = 1, colour = "red",
              lty = 2) +
  theme_bw() +
  labs(
    x = NULL,
    y = NULL,
    title = "Confidence intervals for group odds ratios"
  )
  

