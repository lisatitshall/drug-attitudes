# Overview
Use R to analyse survey data relating to drug decriminalisation. This is an open dataset retrieved from the [UK Data Service](https://reshare.ukdataservice.ac.uk/857543/).

## Survey Summary
People from the UK responded to a 19 question survey about their attitudes towards drug policy. The aim of the survey was to assess whether the public favour a more liberal drug policy. Furthermore, demographic information was collected to see if opinions differed between groups. 

### Steps taken
- Step 1: Imported two datasets from Excel
- Step 2: Joined the datasets based on ID and demographic information (note: fixed data error in LAW column)
- Step 3: Tidied data and removed rows with nulls
- Step 4: Investigated demographics and added decoded demographic columns
- Step 5: Tested whether demographic groups had different attitudes to drug policy
- Step 6: Conducted Multiple Correspondence Analysis to assess important variables
- Step 7: Conducted Multiple Logistic Regression to predict drug decriminalisation concern

## Findings
### [1] Survey participants favoured a more liberal drug policy
Two explicit questions were asked to investigate participant's attitudes towards drug decriminalisation. The first was a Likert question about current drug policy with answers ranging from "current drug policy should be tougher" to "all drugs should be decriminalised". The second questioned whether participants were concerned about the prospect of drug decriminalisation. There were three possible answers: "concerned", "neutral" and "not concerned". The bar charts show in both cases participants were more liberal. 

![image](https://github.com/user-attachments/assets/dcfc1068-5490-4f99-acf7-8d099d5790e9)

![image](https://github.com/user-attachments/assets/5e9313ee-9333-4d86-ac3d-05d74a5c5e79)

The following bar chart shows how responses to both questions compared. As we'd expect participants who favoured a more liberal drug policy were less likely to be concerned about drug
decriminalisation. However, this wasn't a clear cut relationship so it is worth investigating responses to both questions. 

![image](https://github.com/user-attachments/assets/5315f7ca-bf88-4885-8aa7-81ee42873a3d)

### [2] Demographics affected attitudes to drug decriminalisation
The clearest demographic trends were observed for age and education level. The bar charts below show that participants with a higher education level were less concerned about drug decriminalisation and older participants were more concerned. Note: in these bar charts responses are shown as a proportion of 100% for ease of comparison but some groups e.g. 66+ were less well represented.

![image](https://github.com/user-attachments/assets/6c1b1f8c-53f2-486d-ad7f-3b33dc4e1e65)

![image](https://github.com/user-attachments/assets/aa754c60-a52d-480c-8880-292baed40159)

There were no significant differences between genders. For political leaning the responses of left leaning participants differed from apolitical, centre and right wing participants. The other three groups were fairly similar.  

Fisher Exact tests were conducted to test the relationship between demographics and drug policy attitudes. The table below shows each pairing, the Fisher Exact p-value and the Cramer V effect size (where values below 0.2 are weak and values between 0.2 and 0.6 are moderate). 

![image](https://github.com/user-attachments/assets/bbfdf326-8500-4fca-9e9f-bf0ab0741edb)

Most Fisher Exact p-values were statistically significant at the 0.05 level (except gender). The Cramer V values showed a borderline weak/moderate relationship between drug policy and education level and moderate relationships between drug policy and age/political leaning. 

Binary logistic regression was also conducted to look at the association between demographics and drug decriminalisation attitudes. To start neutral/not concerned responses were coded to 0 and concerned responses coded to 1. 

The following screenshot shows the odds ratios compared to reference groups. We can see that:
- Participants aged over 66 were 21 times more likely than 18-25 year olds to be concerned about drug decriminalisation
- Participants with secondary school education were 5 times more likely than those with PhD's to be concerned about drug decriminalisation
- Left wing partipicants were half as likely as non-political participants to be concerned about drug decriminalisation

![image](https://github.com/user-attachments/assets/4bebda87-b3bb-4436-857b-9175526e95e0)

The only statistically significant results when running this analysis were age groups: 46-55, 56-65 and 66+. The following chart shows the confidence intervals for the odds ratios (lower bounds in blue). The significant results correspond to the groups where the lower bound is above 1 (red dotted line on the chart). Note: 3 groups had a higher upper bound than 50 so the orange dot isn't present on the chart. 

![image](https://github.com/user-attachments/assets/520d2cb2-351f-4166-9091-d99648199e2d)

### [3] Two dimensions are insufficient to describe survey data variation
The following were the findings after conducting Multiple Correspondence Analysis:
- 2 dimensions only explains 20% of the data variation
- Drug decriminalisation concern was highly correlated with the first principal component (see first chart below, variable WORRIED)
- Participants age 66+ were more likely to answer that they strongly agree police should intervene when people are taking drugs and that they strongly disagree that the government should introduce more lenient drug policy (see upper right quadrant in second chart below)
- Participants who were concerned about drug decriminalisation were more likely to be concerned that more lenient drug policy will result in higher crime and addiction rates (variables WORRIED_1, CRIMERATE_1 and ADDICTION_1 in upper right quadrant of second chart) 

![image](https://github.com/user-attachments/assets/08b35f29-e94c-4de3-a5df-3d92acfd06a9)

![image](https://github.com/user-attachments/assets/c096a6e1-7fee-49d2-813d-7b1e6530fe10)

### [4] Multiple logistic regression on 5 variables has 60% accuracy on test dataset
We've already seen that Age and Political Leaning are the demographics most strongly correlated with drug decriminalisation opinions. From Multiple Correspondence Analysis we saw the three questions most strongly correlated to the first principal component were Leniency, Trial and Police.

The assumptions for ordinal logistic regression weren't fulfilled (in particular proportional odds). Therefore, multinomial logistic regression was performed. 

The best performing model was the one with all 5 variables. On the test set the accuracy was 60% and kappa was 31% (normalized accuracy to account for class imbalance, this is a fair result but not the best). 

The graph below shows the actual vs predicted values. 

![image](https://github.com/user-attachments/assets/cd6dbf0e-ef45-479c-9b2c-fa2452922ca3)

When we looked at the coefficients of the equations we found the following:
- Participants least concerned about drug decriminalisation were left wing and disagreed with police involvement in drug offences
- Participants most concered about drug decriminalisation were older and didn't support a drug decriminalisation trial
