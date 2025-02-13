#load required packages
library(tidyverse)
library(tidymodels)
library(readxl)


#datasets need to be combined because they show different things
#remove row with memorable ID 2804JG, data was incomplete
#lots of ethnicities - probably not enough to analyse / compare - remove
#have demographics coded/not coded for visualisations

#concerns sheet
# demographics
# decriminalisation question
# concern about drug decriminalisation question
# two select all questions about concerning / not concerning reasons
#  1 means they chose that reason

#demographics sheet
# demographics
# 10 likert questions were answered (1-5 scale where 5 means favour decriminalisation)
# these answers were added up and 3 categories were chosen
#  18-28 unfavourable
#  29-35 neutral
#  36-46 favourable
#BUT law question was specifically about decriminalisation to see how that
# answer compared to the other questions

#aim predict "worried" category based on other answers
#what about law question though...
