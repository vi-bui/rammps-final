#_______________________________________________________________________________
#  Referral Analyses
#  This script contains analyses on the referral call outcome.
#  Flow charts were created to analyse why there are discrepancies between 
#  'REFER' call outcomes and eligibility question answers.
#  05/2023
#_______________________________________________________________________________

# Load data and packages from response_rates.R

# Eligibility question answers -------------------------------------------------

# Following the questionnaire forwards
# Will create a flow chart to follow the responses from E1

# E1 Asks for age
# 3220 refuses
datamw %>%
  group_by(E1_1) %>%
  dplyr::summarise(n=n())

# E2 Gender
datamw %>%
  group_by(E2_1) %>%
  dplyr::summarise(n=n())

# Check how many respondents answered both E1_1 and E2_2
datamw %>%
  filter(E1_1 == 2) %>%
  group_by(E1_1, E2_1) %>%
  dplyr::summarise(n=n())

# E3 Age
datamw %>%
  group_by(e3_a_1) %>%
  dplyr::summarise(n=n())

# select e3_1 that is not 18-64
# 395 respondents don't meet eligibility criteria
e3_nonelig_age <- datamw %>%
  # filter(e3_1 >17 & e3_1<65) %>%
  filter(e3_1 < 18 | e3_1>64) %>%
  group_by(e3_1) %>%
  dplyr::summarise(n=n()) 
sum(e3_nonelig_age$n)

# check whether caseids of non-age eligible respondents are same as those who answered E10_1
e3_1_caseid <- datamw %>%
  filter(e3_1 < 18 | e3_1>64) %>%
  select(caseid)
e10_noNA_caseid <- datamw %>%
  filter(!is.na(E10_1)) %>%
  select(caseid)
all.equal(e3_1_caseid,e10_noNA_caseid)
identical(e3_1_caseid, e10_noNA_caseid)

# Check whether those who are age ineligible answered E10
# All those who don't meet age eligibility criteria answered E10
datamw %>%
  # filter(e3_1 >17 & e3_1<65) %>%
  filter(e3_1 < 18 | e3_1>64) %>%
  filter(is.na(E10_1))  %>%
  group_by(caseid, e3_1, E10_1) %>%
  dplyr::summarise(n=n()) %>%
  View()

# Region
datamw %>%
  group_by(region_1) %>%
  dplyr::summarise(n=n())

# District
datamw %>%
  group_by(e4b_1) %>%
  dplyr::summarise(n=n())

# Village or small town
datamw %>%
  group_by(e4c_1) %>%
  dplyr::summarise(n=n())

# E9
E9_all <- data.frame(X = c(datamw$e9_1_1, 
                           datamw$e9_2_1,
                           datamw$e9_3_1,
                           datamw$e9_4_1,
                           datamw$e9_5_1,
                           datamw$e9_6_1,
                           datamw$e9_7_1,
                           datamw$e9_8_1,
                           datamw$e9_9_1,
                           datamw$e9_10_1,
                           datamw$e9_11_1,
                           datamw$e9_12_1,
                           datamw$e9_13_1,
                           datamw$e9_14_1,
                           datamw$e9_15_1,
                           datamw$e9_16_1,
                           datamw$e9_17_1,
                           datamw$e9_18_1,
                           datamw$e9_19_1,
                           datamw$e9_20_1,
                           datamw$e9_21_1,
                           datamw$e9_22_1,
                           datamw$e9_23_1,
                           datamw$e9_24_1))
E9_all %>%
  group_by(X) %>%
  dplyr::summarise(n=n())

# E10
datamw %>%
  group_by(E10_1) %>%
  drop_na(E10_1) %>%
  dplyr::summarise(n=n()) %>%
  mutate(total = sum(n))
# Call outcomes of E10
datamw %>%
  group_by(E10_1, Outcome2) %>%
  drop_na(E10_1) %>%
  dplyr::summarise(n=n()) 

# E11 Retain number for defer?
datamw %>%
  group_by(E11_1) %>%
  dplyr::summarise(n=n())

# E12 Require call-back?
datamw %>%
  group_by(E12_1) %>%
  dplyr::summarise(n=n())

# Check whether there are duplicates for the 34 referred respondents that can answer right away
# 2 caseids were repeated: 52398 + 52411
datamw %>%
  filter(E12_1 ==1) %>%
  group_by(caseid,  E2_1) %>%
  dplyr::summarise(n=n()) %>%
  View()
# first get caseids where referred respondents can come to the phone (E12_1 == 1)
E12_1_caseids <- datamw %>%
  filter(E12_1 == 1) %>%
  select(caseid)
# some of these cases have multiple answers for E2_1 for different call attempts
datamw %>%
  filter(caseid %in% E12_1_caseids$caseid) %>%
  group_by(caseid, call_num, E2_1) %>%
  dplyr::summarise(n=n()) %>%
  View()

# E14 Require call-back?
datamw %>%
  group_by(E14_1) %>%
  dplyr::summarise(n=n())

# E15 Day preference
datamw %>%
  group_by(E15_1) %>%
  drop_na(E15_1) %>%
  dplyr::summarise(n=n()) %>%
  mutate(total = sum(n))

# E16 Day preference
datamw %>%
  group_by(E16_1) %>%
  drop_na(E16_1) %>%
  dplyr::summarise(n=n()) %>%
  mutate(total = sum(n))

# Consent
datamw %>%
  group_by(C1_1) %>%
  dplyr::summarise(n=n())

# create df containing caseids that did not answer any of the elig questions
no.elig <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  filter(is.na(E2_1)) %>%
  group_by(caseid) %>%
  summarise(frequency=n()) 

# include those that did not answer E10
no.e10 <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  filter(is.na(E10_1)) %>%
  drop_na(E2_1) %>%
  group_by(caseid) %>%
  summarise(frequency=n())

refer.caseid <- merge(no.elig, no.e10)
write.csv(no.elig, paste0(r.functions.dir, 'no_elig.csv'))
write.csv(no.e10, paste0(r.functions.dir, 'no_e10.csv'))
