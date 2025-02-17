# Overview
Use R to analyse survey data relating to drug decriminalisation. This is an open dataset retrieved from the [UK Data Service](https://reshare.ukdataservice.ac.uk/857543/).

## Survey Summary
People from the UK responded to a 19 question survey about their attitudes towards drug policy. The aim of the survey was to assess whether the public favour a more liberal drug policy. Furthermore, demographic information was collected to see if opinions differed between groups. 

### Steps taken
- Step 1: Imported two datasets from Excel
- Step 2: Joined the datasets based on ID and demographic information (note: fixed data error in LAW column)
- Step 3: Tidied data and removed rows with nulls
- Step 4: Investigated demographics and added decoded demographic columns

## Findings
### [1] Survey participants favoured a more liberal drug policy
Two explicit questions were asked to investigate participant's attitudes towards drug decriminalisation. The first was a Likert question about current drug policy with answers ranging from "current drug policy should be tougher" to "all drugs should be decriminalised". The second questioned whether participants were concerned about the prospect of drug decriminalisation. There were three possible answers: "concerned", "neutral" and "not concerned". The bar charts show in both cases participants were more liberal. 

![image](https://github.com/user-attachments/assets/dcfc1068-5490-4f99-acf7-8d099d5790e9)

![image](https://github.com/user-attachments/assets/5e9313ee-9333-4d86-ac3d-05d74a5c5e79)

The following bar chart shows how responses to both questions compared. As we'd expect participants who favoured a more liberal drug policy were less likely to be concerned about drug
decriminalisation. However, this wasn't a clear cut relationship so it is worth investigating responses to both questions. 

![image](https://github.com/user-attachments/assets/5315f7ca-bf88-4885-8aa7-81ee42873a3d)

### [2] Demographics affected attitudes to drug decriminalisation
The clearest demographic trends were observed for age and education level. The bar charts below show that participants with a higher education level were less concerned about drug decriminalisation and older participants were more concerned. 

![image](https://github.com/user-attachments/assets/6c1b1f8c-53f2-486d-ad7f-3b33dc4e1e65)

![image](https://github.com/user-attachments/assets/aa754c60-a52d-480c-8880-292baed40159)

There were no significant differences between genders. For political leaning the responses of left leaning participants differed from apolitical, centre and right wing participants. The other three groups were fairly similar. Note: in these bar charts responses are shown as a proportion of 100% for ease of comparison but some groups e.g. 66+ were less well represented. 
