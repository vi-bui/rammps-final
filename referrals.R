mwdata_raw %>%
  group_by(e9_3_1) %>%
  dplyr::summarise(n=n())

datamw %>%
  group_by(caseid,Outcome2, E10_1) %>%
  filter(Outcome2 == 'REFER') %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  filter(E10_1 == 1) %>%
  group_by(Outcome2) %>%
  dplyr::summarise(n=n())

################################################################################
# E10 answers
datamw %>%
  group_by(E10_1) %>%
  dplyr::summarise(n=n())

# Call outcomes
datamw %>%
  group_by(caseid)
  filter(Outcome2 == 'REFER') %>%
  group_by(L1_A_1, Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(E2_1, Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  select(caseid)
unique(datamw$caseid)
head(mwdata_raw$text_audit)


n_occur <- data.frame(table(datamw$caseid))
#gives you a data frame with a list of ids and the number of times they occurred.
n_occur[n_occur$Freq > 1,]
#vocabulary[vocabulary$id %in% n_occur$Var1[n_occur$Freq > 1],]
datamw[datamw$id %in% n_occur$Var1[n_occur$Freq > 1],]

# group by call attempt number and call outcom
# shows this case took three call attempts to get completed interview
datamw %>%
  filter(caseid == 10787) %>%
  group_by(call_num, Outcome2) %>%
  dplyr::summarise(n=n())


datamw %>%
  filter(caseid == 7871) %>%
  group_by(call_num, Outcome2) %>%
  dplyr::summarise(n=n())

datamw %>%
  filter(E10_1 == 1) %>%
  group_by(E10_1, caseid, call_num, Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  filter(caseid == 9375) %>%
  group_by(call_num, Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()

# E3_1 for age 
age <- datamw %>%
  group_by(e3_1) %>%
  drop_na(e3_1) %>%
  dplyr::summarise(n=n()) 
# 18417 answered
sum(age$n)

# age ineligible
age_ineligble <- datamw %>%
  group_by(e3_1) %>%
  drop_na(e3_1) %>%
  filter(!(e3_1 > 17 & e3_1 < 65)) %>%
  dplyr::summarise(n=n()) 
# 390 not age eligible
sum(age_ineligble$n)

# E10_1
E10_all <- datamw %>%
  group_by(E10_1) %>%
  drop_na(E10_1) %>%
  summarise(n=n())
# 390 answered
sum(E10_all$n)

datamw %>%
  filter(E10_1 == 1) %>%
  group_by(caseid, call_num, Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()

# E16 is when calls are classified as referrals
E16_num <- datamw %>%
  group_by(E16_1) %>%
  drop_na(E16_1) %>%
  filter(!(E16_1 == 99)) %>%
  summarise(n=n())
# 38 calls are classified as answered so could be referrals
sum(E16_num$n)
# only 26 calls are classified as referrals
datamw %>%
  drop_na(E16_1) %>%
  filter(!(E16_1 == 99)) %>%
  group_by( Outcome2) %>%
  summarise(n=n())%>%
  View()
  
datamw %>%
  filter(caseid %in% E10_id$caseid) %>%
  group_by(caseid, call_num, Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  filter(caseid %in% calloutcome_id$caseid) %>%
  group_by(caseid, call_num, Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()



a <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  # filter(call_num == 1) %>%
  select(caseid, phone_1, call_num, Outcome2,L1_A_1, L2_1,L3_1, L4_1,l5_1,  E2_1, e3_1, E4_1,e4b_1, e4c_1, E10_1,  E14_1,E15_1, E16_1, C1_1) 

sum(is.na(a$E10_1))
a %>%
  View()
datamw %>%
  select(e14_b_1)

unique(datamw$e14_b_1)
datamw %>%
  filter(e14_b_1 == 881226321) %>%
  select(caseid, call_num, Outcome2,L1_A_1, E1_1,E2_1, e3_1, E4_1, l5_1,e4b_1, e4c_1, E10_1,  E14_1,E15_1, E16_1, C1_1)

datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(E2_1) %>%
  dplyr::summarise(n=n())

15980
datamw %>%
  filter(caseid == 10764) %>%
  group_by(caseid, call_num, Outcome2, L1_A_1, E1_1, e3_1, E10_1,  E14_1, E16_1, C1_1) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  filter(caseid == 16202) %>%
  group_by(caseid, call_num, Outcome2, L1_A_1, E1_1, E2_1, e3_1, E10_1,  E14_1, E16_1, C1_1) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  filter(phone_1 == 996111061) %>%
  group_by(caseid, phone_1, call_num, Outcome2, L1_A_1, E1_1, e3_1, E10_1,  E14_1, E16_1, C1_1) %>%
  dplyr::summarise(n=n()) 

datamw %>%
  group_by(e3_1) %>%
  summarise(n=n())

unique(datamw$caseid)
unique(datamw$phone_2)
datamw %>%
  group_by(E10_1, Outcome2) %>%
  summarise(n=n())
datamw %>%
  filter(caseid == 5093) %>%
  group_by(call_num, Outcome2, phone_1, L1_A_1, E1_1, e3_1,E10_1) %>%
  dplyr::summarise(n=n())
datamw %>%
  filter(phone_1 == 880240058) %>%
  group_by(call_num, Outcome2, caseid, L1_A_1, E1_1, e3_1, E10_1)%>%
  dplyr::summarise(n=n())
head(dat.widemw$call_num)
head(lag(dat.widemw$call_num, 6) == 0)


datamw %>%
  group_by(e9_2_1) %>%
  drop_na(e9_2_1) %>%
  summarise(n=n())
mwdata_raw$e19_1
unique(mwdata_raw$e19_1)

# work out whether all refer cases answered the first lang questions
datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(L1_A_1, Outcome2) %>%
  dplyr::summarise(n=n())

a <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(E16_1) %>%
  dplyr::summarise(n=n()) 

sum(a$n)

# select e3_1 that is between 18-64
e3_elig_age <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  filter(e3_1 >17 & e3_1<65) %>%
  group_by(e3_1) %>%
  dplyr::summarise(n=n()) 
sum(e3_elig_age$n)


datamw %>%
  filter(Outcome2 == 'REFER') %>%
  dplyr::summarise(n=n())
datamw %>%
  group_by(E10_1, Outcome2) %>%
  summarise(n=n())


library('DiagrammeR')

DiagrammeR::grViz("               # All instructions are within a large character string
digraph surveillance_diagram {    # 'digraph' means 'directional graph', then the graph name 
  # graph statement
  #################
  graph [layout = dot,
         rankdir = TB,            # layout top-to-bottom
         fontsize = 10]
  # nodes (square)
  #################
  node [shape = square,           # shape = square
       fixedsize = true
       width = 2]  
       
  Outcome2   [label = 'Call Outcomes: Refer\n(292) '] 
  e3_1 [label = 'e3_1: Age required\nAnswered=261\nNA=31\nTotal=292'] 
  E10_1  [label = 'E10_1: Eligible person?\nAnswered=61\nNA=231\nTotal=292'] 
  E16_1 [label = 'E16_1: Time pref\nAnswered=27\nNA=265\nTotal=292'] 
  elig_age [label = '200 calls between \nages 18-64'] 
  
  # edges

  #######
  Outcome2 -> e3_1
  e3_1 -> elig_age
  Outcome2 -> E10_1
  Outcome2 -> E16_1
  
}
")

datamw %>%
  group_by(L1_A_1) %>%
  summarise(n=n())
DiagrammeR::grViz("               # All instructions are within a large character string
digraph surveillance_diagram {    # 'digraph' means 'directional graph', then the graph name 
  # graph statement
  #################
  graph [layout = dot,
         rankdir = LR,            # layout top-to-bottom
         fontsize = 10]
  # nodes (square)
  #################
  node [shape = square,           # shape = square
       fixedsize = true
       width = 2]  
       
  e3_1 [label = 'e3_1: Age required\n395 non-eligible'] 
  E10_1  [label = 'E10_1: Eligible person?\nYes=93\nNo=295\nDo not Know=4\nRefuse=3\nTotal=395'] 
  E12_1 [label = 'E12_1: Require callback?\nYes=63\nNo=30\nTotal=93']
  E14_1 [label = 'E14_1: Same number?\nYes=54\nNo=6\nRefuse=3\nTotal=63'] 
  E15_1 [label = 'E15_1: Day preference\nAnswered=39\nTotal=39']
  E16_1 [label = 'E16_1: Time pref\nAnswered=38\nRefused=1\nTotal=39'] 

  # edges
  
  #######
  e3_1 -> E10_1
  E10_1 -> E12_1
  E12_1 -> E14_1
  E14_1 -> E15_1
  E15_1 -> E16_1
  
}
")

DiagrammeR::grViz("               # All instructions are within a large character string
digraph surveillance_diagram {    # 'digraph' means 'directional graph', then the graph name 
  # graph statement
  #################
  graph [layout = dot,
         rankdir = LR,            # layout top-to-bottom
         fontsize = 10]
  # nodes (square)
  #################
  node [shape = square,           # shape = square
       fixedsize = true
       width = 2,
       style = filled,
       fillcolor = Linen]  
       
  e3_1 [label = 'e3_1: Age required\n395 non-eligible'] 
  E10_1  [label = 'E10_1: Eligible person?\nYes=93\nNo=295\nDo not Know=4\nRefuse=3\nTotal=395'] 
  E12_1 [label = 'E12_1: Require callback?\nYes=63\nNo=30\nTotal=93']
  E14_1 [label = 'E14_1: Same number?\nYes=54\nNo=6\nRefuse=3\nTotal=63'] 
  E15_1 [label = 'E15_1: Day preference\nAnswered=39\nTotal=39']
  E16_1 [label = 'E16_1: Time pref\nAnswered=38\nRefused=1\nTotal=39'] 
  E10_1_ref  [label = 'Yes: 60 referred\nNo: 1 referred\nTotal=61 referred', fillcolor = Beige] 
  E12_1_ref [label = 'Yes: 43 referred\nNo: 18 referred\nTotal=61 referred', fillcolor = Beige]
  E14_1_ref [label = 'Yes: 35 referred\nNo: 6 referred\nRefuse: 1\nTotal=42 referred', fillcolor = Beige]
  E15_1_ref [label = 'Answered=27 referred\nTotal=27 referred', fillcolor = Beige]
  E16_1_ref [label = 'Answered=26 referred\nRefused=1 referred\nTotal=27 referred', fillcolor = Beige]


  # edges
  
  #######
  e3_1 -> E10_1
  E10_1 -> E12_1
  E12_1 -> E14_1
  E14_1 -> E15_1
  E15_1 -> E16_1
  E10_1 -> E10_1_ref
  E12_1 -> E12_1_ref
  E14_1 -> E14_1_ref
  E15_1 -> E15_1_ref
  E16_1 -> E16_1_ref
}
")


# select e3_1 that is not 18-64
# 395 respondents don't meet eligibility criteria
e3_nonelig_age <- datamw %>%
  # filter(e3_1 >17 & e3_1<65) %>%
  filter(e3_1 < 18 | e3_1>64) %>%
  group_by(e3_1) %>%
  dplyr::summarise(n=n()) 
sum(e3_nonelig_age$n)

# E10_1
# 93 said there was an eligible person
datamw %>%
  filter(call_num == 3) %>%
  filter(E10_1 == 2) %>%
  drop_na(E10_1) %>%
  group_by(caseid, E10_1, Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  filter(caseid == 28125) %>%
  group_by(call_num, E10_1, Outcome2)%>%
  dplyr::summarise(n=n())

# 63 required call-back
datamw %>%
  group_by(E12_1, Outcome2) %>%
  dplyr::summarise(n=n())

datamw %>%
  filter(E12_1 == 2) %>%
  group_by(caseid, E12_1, Outcome2) %>%
  dplyr::summarise(n=n())
# Get calls that require a callack and use the same number
datamw %>%
  filter(E12_1 == 2) %>%
  filter(E14_1 == 1) %>%
  group_by(caseid, call_num, Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()
# Check whether referrals are given new caseids
datamw %>%
  filter(caseid == 20372) %>%
  group_by(call_num, Outcome2, phone_1) %>%
  dplyr::summarise(n=n())
# Referrals are given new caseid
datamw %>%
  filter(phone_1 == 997221627) %>%
  group_by(call_num, Outcome2, caseid) %>%
  dplyr::summarise(n=n())

# Get calls that can come to the phone now and use the same number
datamw %>%
  filter(E12_1 == 1) %>%
  group_by(caseid, call_num, Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()
# Check whether referrals are given new caseids
datamw %>%
  filter(caseid == 36705) %>%
  group_by(call_num, Outcome2, phone_1) %>%
  dplyr::summarise(n=n())
# Referrals are given new caseid
datamw %>%
  filter(phone_1 == 995804078) %>%
  group_by(call_num, Outcome2, caseid) %>%
  dplyr::summarise(n=n())

datamw %>%
  filter(E10_1 == 2) %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(caseid, call_num, Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()
datamw %>%
  filter(caseid == '28125') %>%
  group_by(call_num, Outcome2, phone_1) %>%
  dplyr::summarise(n=n())
# Referrals are given new caseid
datamw %>%
  filter(phone_1 == 880299152) %>%
  group_by(call_num, Outcome2, caseid) %>%
  dplyr::summarise(n=n())

datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(Outcome2) %>%
  dplyr::summarise(n=n())

dat.widemw 
lag(datamw$call_num,2) == 0

A <- mwdata_raw %>%
  group_by(caseid) %>%
  dplyr::arrange(DateTime, .by_group = TRUE) 
lag(A$call_num,3) == 0
A %>%
  group_by(call_num) %>%
  dplyr::summarise(n=n())
# 54 = same number
# 6 = another number
# 3 = refused
datamw %>%
  drop_na(E14_1) %>%
  group_by(caseid, E14_1, Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  filter(caseid == 21096) %>%
  group_by(call_num, E14_1, Outcome2) %>%
  dplyr::summarise(n=n())

# Day preference
# 39 answered
ref_day_pref <- datamw %>%
  group_by(E15_1, Outcome2) %>%
  drop_na(E15_1) %>%
  dplyr::summarise(n=n())
sum(ref_day_pref$n)
ref_day_pref %>%
  View()

# Time preference
# 39 answered
# 1 refused
ref_time_pref <- datamw %>%
  group_by(E16_1) %>%
  drop_na(E16_1) %>%
  dplyr::summarise(n=n())
sum(ref_day_pref$n)

caseid_refer <- grep("-Refer",datamw$caseid, value = TRUE) 

grep("Refer",dat.widemw$caseid, value = TRUE) 

datamw %>%
  group_by(L3_1) %>%
  dplyr::summarise(n=n()) %>%
  View()


###################################
## Referrals based on each eligibiliy critera
e3_1_age <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(e3_1) %>%
  drop_na(e3_1) %>%
  dplyr::summarise(n=n()) 
sum(e3_1_age$n)
str(e3_1_age)
e3_1_age$e3_1<- as.numeric(e3_1_age$e3_1)
e3_1_age$n<- as.numeric(e3_1_age$n)

ggplot(e3_1_age, aes(x=e3_1, y=n)) +
  geom_bar(stat = 'identity') +
  labs(x='Age', y='Number of respondents') +
  geom_vline(xintercept=c(18,64), linetype="dotted", col = 'red') +
  theme_classic()
  
datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(e4b_1) %>%
  dplyr::summarise(n=n())

DiagrammeR::grViz("               # All instructions are within a large character string
digraph surveillance_diagram {    # 'digraph' means 'directional graph', then the graph name 
  # graph statement
  #################
  graph [layout = dot,
         rankdir = TP,            # layout top-to-bottom
         fontsize = 15]
  # nodes (square)
  #################
  node [shape = square,           # shape = square
       fixedsize = true
       width = 2.5,
       style = filled,
       fillcolor = Linen]  
       
  refer [label = 'Refer call outcomes\n292 calls']
  E2_1 [label = 'E2_1: gender\nMale = 237\nFemale = 24\nNA = 31\nTotal=292'] 
  e3_1 [label = 'e3_1: Age\n14-82 = 261\nNA = 31\nTotal = 292'] 
  E10_1  [label = 'E10_1: Eligible person\nin HH\nYes=60\nNo=1\nNA=231\nTotal=292'] 
  E12_1 [label = 'E12_1: Can person come\nto phone now?\nNow=18\nNo=42\nNA=232\nTotal=292']
  E14_1 [label = 'E14_1: Phone NO.\nSame=35\nDifferent=6\nDo not know=0\nRefuse=0\nNA=250\nTotal=292'] 
  E15_1 [label = 'E15_1: Day pref\nNo Pref=10\nMon=2\nTues=8\nWed=4\nThur=0\nFri=1\nSat=0\nSun=2\nNA=265\nTotal=292']
  E16_1 [label = 'E16_1: Time pref\nNo Pref=7\nBefore 09:00=0\n9.00-12.00=3\n12.00-14.00=2\n14.00-17.00=6\n17.00-19.00=8\nAfter 19:00=0\nRefuse=1\nNA=265\nTotal=292'] 

  # edges
  
  #######
  refer ->E2_1
  E2_1 -> e3_1
  e3_1 -> E10_1
  E10_1 -> E12_1
  E12_1 -> E14_1
  E14_1 -> E15_1
  E15_1 -> E16_1
}
")

# how many males are within the age eligibility criteria
males.elig <- datamw %>%
  filter(E2_1 == 2) %>%
  filter(e3_1 > 17 & e3_1 < 65) %>%
  filter(Outcome2 == 'REFER') %>%
  select(L1_A_1, E1_1,E2_1, e3_1, E4_1, e4b_1, e4c_1,E10_1,  E14_1,E15_1, E16_1)
nrow(males.elig)  


library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      refer [label = '@@1']
      E2_1 [label = '@@2']
      e3_1 [label = '@@3']
      E10_1 [label = '@@4']
      E12_1 [label = '@@5']
      E14_1 [label = '@@6']
      E15_1 [label = '@@7']
      E16_1 [label = '@@8']
      
      # edge definitions with the node IDs
      refer -> E2_1 -> e3_1 -> E10_1 -> E12_1 -> E14_1 -> E15_1 -> E16_1;
      }

      [1]: 'Refer call outcomes\\n296 calls'
      [2]: 'E2_1: gender\\nMale = 239\\nFemale = 24\\nNA = 33\\nTotal=296'
      [3]: 'e3_1: Age\\n14-82 = 263\\nNA = 33\\nTotal = 296'
      [4]: 'E10_1: Eligible person in HH\\nYes=61\\nNo=1\\nNA=234\\nTotal=296'
      [5]: 'E12_1: Can person come to phone now?\\nNow=18\\nNo=43\\nNA=235\\nTotal=296'
      [6]: 'E14_1: Phone NO.\\nSame=36\\nDifferent=6\\nDo not know=0\\nRefuse=1\\nNA=253\\nTotal=296'
      [7]: 'E15_1: Day pref\\nNo Pref=10\\nMon=2\\nTues=8\\nWed=4\\nThur=0\\nFri=1\\nSat=0\\nSun=2\\nNA=269\\nTotal=296'
      [8]: 'E16_1: Time pref\\nNo Pref=7\\nBefore 09:00=0\\n9.00-12.00=3\\n12.00-14.00=2\\n14.00-17.00=6\\n17.00-19.00=8\\nAfter 19:00=0\\nRefuse=1\\nNA=269\\nTotal=296'
      ")

datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(E16_1) %>%
  dplyr::summarise(n=n())

# Get cases that didn't answer E2_1
E2_1.noanswer <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  filter(is.na(E2_1)) %>%
  group_by(caseid, phone_1, E2_1) %>%
  dplyr::summarise(n=n())

datamw %>%
  filter(caseid == 15289) %>%
  group_by(call_num, L1_A_1,Outcome2, E2_1) %>%
  dplyr::summarise(n=n())

datamw %>%
  filter(phone_1 == 884015335) %>%
  group_by(call_num, L1_A_1,Outcome2, E2_1) %>%
  dplyr::summarise(n=n())

E2_1.noanswer[duplicated(E2_1.noanswer) | duplicated(E2_1.noanswer, fromLast=TRUE)]



datamw %>%
  group_by(caseid,Outcome2, E10_1) %>%
  filter(!is.na(E10_1)) %>%
  group_by(Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  filter(!is.na(E10_1)) %>%
  filter(E10_1 == 1) %>%
  group_by(Outcome2, E10_1) %>%
  dplyr::summarise(n=n()) %>%
  View()

# why did only 61 calls that were classed as 'REFER' answer E10?
datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(Outcome2, E10_1, caseid) %>%
  dplyr::mutate(n=n()) %>%
  View()

datamw %>%
  filter(caseid == 44318) %>%
  group_by(call_num,caseid,  phone_1, Outcome2, E10_1) %>%
  dplyr::summarise(n=n()) 

datamw %>%
  filter(phone_1 == 999955026) %>%
  group_by(call_num,caseid,  phone_1, Outcome2, E10_1) %>%
  dplyr::summarise(n=n()) 

datamw %>%
  group_by(Outcome2) %>%
  dplyr::summarise(n=n())
# get list of repeated caseids
refer.repeat <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(caseid) %>%
  filter(n()>1) 
# find number of repreated caseids
unique(refer.repeat$caseid)



yas <- merge(ref.id, outcome.final.df, all.x = FALSE)

# get list of repeated caseids
ref.Outcome.FINAL.repeat <- ref.Outcome.FINAL %>%
  group_by(caseid) %>%
  filter(n()>1) 
# find number of repeated caseids
unique(ref.Outcome.FINAL.repeat$caseid)

dat.widemw %>%
  filter(caseid == 46684) %>%
  group_by(call_num, Outcome.FINAL)%>%
  dplyr::summarise(n=n())
datamw %>%
  filter(caseid == 46684) %>%
  group_by(call_num, Outcome2)%>%
  dplyr::summarise(n=n())

################################################################################
# Check whether any of the caseids are labelled with -Refer.
# This was previously done by Kelly.
# Removed it to avoid confusion.
caseid_refer <- grep("-Refer",datamw$caseid, value = TRUE) 

# Number of 'REFER' cases
datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(Outcome2) %>%
  dplyr::summarise(n=n()) 

# How many 'REFER' cases answered first language question?
# All 'REFER' cases answered lang question
datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(!is.na(L1_A_1)) %>%
  dplyr::summarise(n=n()) 

# Eligibility questions
# How many answered E2 and E3?
# Questions required
# 262 answered. 33 NA
datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(!is.na(E2_1)) %>%
  dplyr::summarise(n=n()) 

# Get cases that didn't answer E2_1
E2_1.noanswer <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  filter(is.na(E2_1)) %>%
  group_by(caseid, phone_1, E2_1) %>%
  dplyr::mutate(n=n())
# Check individual caseids of those that did not answer E2_1
datamw %>%
  filter(caseid == 46686) %>%
  group_by(call_num, users, phone_1, Outcome2,L1_A_1, E2_1, e4b_1, E10_1) %>%
  dplyr::summarise(n=n())

datamw %>%
  filter(phone_1 == 883208005) %>%
  group_by(call_num, L1_A_1,Outcome2, E2_1) %>%
  dplyr::summarise(n=n())

# E10
# How many answered E10?
# Only 61 answered E10
datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(!is.na(E10_1)) %>%
  dplyr::summarise(n=n()) 

datamw %>%
  filter(Outcome2 == 'REFER') %>%
  filter(!is.na(E10_1)) %>%
  group_by(caseid, E10_1) %>%
  dplyr::summarise(n=n()) 
# CATI outcomes of 61 respondents that answered E10 yes
## Get Outcome.FINAL of referred that answered E10
# create df containing caseid of 'REFER' that answered E10
E10.ans.id <- datamw %>%
  group_by(caseid,Outcome2, E10_1) %>%
  filter(Outcome2 == 'REFER') %>%
  filter(!is.na(E10_1)) %>%
  dplyr::summarise(n=n()) 
# create df containing caseid and Outcome.FINAL
outcome.final.df <- dat.widemw %>%
  select(caseid, Outcome.FINAL)
# Get CATI outcomes of of E10.ans.id caseids 
E10.Outcome.FINAL <- left_join(E10.ans.id, outcome.final.df, by='caseid')
E10.Outcome.FINAL %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(n=n()) %>%
  View()
# Get list of repeated caseids
E10.Outcome.FINAL.repeat <- E10.Outcome.FINAL %>%
  group_by(caseid) %>%
  filter(n()>1) 
# find number of repreated caseids
unique(E10.Outcome.FINAL.repeat$caseid)
# Look into the repeated caseids
dat.widemw %>%
  filter(caseid == 44384) %>%
  group_by(call_num, Outcome.FINAL) %>%
  dplyr::summarise(n=n())


## Get Outcome.FINAL of 'REFER' 
# create df containing caseid of 'REFER' 
ref.id <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(caseid) %>%
  dplyr::mutate(n.Outcome2 =n())
# create df containing caseid and Outcome.FINAL
outcome.final.df <- dat.widemw %>%
  select(caseid, Outcome.FINAL) 
# Get CATI outcomes of of ref.id caseids 
ref.Outcome.FINAL <- left_join(ref.id, outcome.final.df, by='caseid')
ref.Outcome.FINAL %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(freq=n_distinct(caseid)) %>%
  View()
# get list of repeated caseids
ref.Outcome.FINAL.repeat <- ref.Outcome.FINAL %>%
  group_by(caseid) %>%
  filter(n()>1) 
# find number of repreated caseids
unique(ref.Outcome.FINAL.repeat$caseid)
# Look into the repeated caseids
dat.widemw %>%
  filter(caseid == 19054) %>%
  group_by(call_num, Outcome.FINAL) %>%
  dplyr::summarise(n=n())

ref.Outcome.FINAL %>%
  filter(caseid == 44703) %>%
  summarise(n=n())

# Get all E10 answered
datamw %>%
  group_by(E10_1) %>%
  summarise(n=n())

# Call outcomes based on those that answered E10
datamw %>%
  filter(E10_1 == 1) %>%
  group_by(Outcome2) %>%
  summarise(n=n())

# CATI outcomes based on those that answered E10
# create df containing caseid of 'REFER' 
E10.id <- datamw %>%
  filter(E10_1 == 1) %>%
  select(caseid) 
# create df containing caseid and Outcome.FINAL
outcome.final.df <- dat.widemw %>%
  select(caseid, Outcome.FINAL) 
# Get CATI outcomes of of ref.id caseids 
E10.Outcome.FINAL <- left_join(E10.id, outcome.final.df, by='caseid')
E10.Outcome.FINAL %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(freq=n_distinct(caseid)) %>%
  View()

###############################################################################
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

datamw %>%
  filter(caseid == 52398) %>%
  group_by(call_num, DateTime, E2_1, e3_1, E10_1, E12_1, E14_1,Outcome2, call_status, phone_1, e14_b_1) %>%
  dplyr::summarise(n=n())

# dat.widemw %>%
#   filter(caseid == 15980) %>%
#   group_by(Outcome.FINAL) %>%
#   dplyr::summarise(n=n())
datamw %>%
  filter(phone_1 == 884599612) %>%
  group_by(call_num, E2_1,  E12_1,Outcome2, phone_1) %>%
  dplyr::summarise(n=n())

datamw %>%
  filter(E12_1 ==1) %>%
  group_by( caseid, call_num,Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()

# E14 Require call-back?
datamw %>%
  group_by(E14_1) %>%
  dplyr::summarise(n=n())
# first get caseids where referred respondents require same number (E14_1 == 1)
E14_1_caseids <- datamw %>%
  filter(E14_1 == 2) %>%
  select(caseid)
datamw %>%
  filter(caseid %in% E14_1_caseids$caseid) %>%
  group_by(caseid, call_num, E2_1) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  filter(E14_1 == 1) %>%
  select(caseid) %>%
  View()

datamw %>%
  group_by(caseid, Outcome2) %>%
  drop_na(E14_1) %>%
  dplyr::summarise(n=n()) %>%
  View()

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

library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      E1_1 [label = '@@1']
      E1_1_refu [label = '@@2']
      E2_1 [label = '@@3']
      e3_1 [label = '@@4']
      E10_1 [label = '@@5']
      E12_1 [label = '@@6']
      E14_1 [label = '@@7']
      E15_1 [label = '@@8']
      E16_1 [label = '@@9']
      
      # edge definitions with the node IDs
      E1_1 -> E1_1_refu
      E1_1 -> E2_1 -> e3_1 -> E10_1 -> E12_1 -> E14_1 -> E15_1 -> E16_1;
      }

      [1]: 'E1_1: Name \\n Answered = 16927 calls'
      [2]: 'Refuse = Refuse = 3220 calls'
      [3]: 'E2_1: gender\\nMale = 239\\nFemale = 24\\nNA = 33\\nTotal=296'
      [4]: 'e3_1: Age\\n14-82 = 263\\nNA = 33\\nTotal = 296'
      [5]: 'E10_1: Eligible person in HH\\nYes=61\\nNo=1\\nNA=234\\nTotal=296'
      [6]: 'E12_1: Can person come to phone now?\\nNow=18\\nNo=43\\nNA=235\\nTotal=296'
      [7]: 'E14_1: Phone NO.\\nSame=36\\nDifferent=6\\nDo not know=0\\nRefuse=1\\nNA=253\\nTotal=296'
      [8]: 'E15_1: Day pref\\nNo Pref=10\\nMon=2\\nTues=8\\nWed=4\\nThur=0\\nFri=1\\nSat=0\\nSun=2\\nNA=269\\nTotal=296'
      [9]: 'E16_1: Time pref\\nNo Pref=7\\nBefore 09:00=0\\n9.00-12.00=3\\n12.00-14.00=2\\n14.00-17.00=6\\n17.00-19.00=8\\nAfter 19:00=0\\nRefuse=1\\nNA=269\\nTotal=296'
      ")


df <- data.frame(call_num=datamw$call_num,
           phone_1 = datamw$phone_1)

df.wide <- pivot_wider(df, names_from = call_num)


  
datamw %>% 
  filter(E12_1 == 1) %>%
  filter(Outcome2 == 'INEL') %>%
  group_by(caseid,call_num, E2_1) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  filter(caseid == 46722) %>%
  group_by(call_num, DateTime, username, L1_A_1,E2_1, e3_1,region_1, e4b_1,E10_1, E12_1, E14_1,C1_1, Outcome2) %>%
  dplyr::summarise(n=n()) 

datamw %>%
  filter(caseid == 55838) %>%
  group_by(call_num, DateTime, username, L1_A_1,L2_1, L3_1, L4_1, E2_1, e3_1,region_1, e4b_1,E10_1, E12_1, Outcome2) %>%
  dplyr::summarise(n=n()) 


grep("referral",datamw$comments, value = TRUE) 

datamw %>%
  group_by(call_status) %>%
  summarise(n=n())

datamw %>%
  filter(Outcome2 == 'REFER') %>%
  filter(is.na(E2_1)) %>%
  group_by(caseid) %>%
  summarise(n=n()) %>%
  View()

datamw %>%
  filter(Outcome2 == 'REFER') %>%
  filter(is.na(E10_1)) %>%
  drop_na(E2_1) %>%
  group_by(caseid, username,region_1,comments) %>%
  summarise(n=n()) %>%
  View()

datamw %>%
  filter(E10_1 == 2) %>%
  group_by(caseid,Outcome2) %>%
  summarise(n=n()) %>%
  View()

datamw %>%
  filter(E12_1 == 1) %>%
  filter(Outcome2 == 'INEL') %>%
  group_by(caseid,Outcome2) %>%
  summarise(n=n()) %>%
  View()

datamw %>%
  filter(caseid == 41300) %>%
  group_by(call_num, DateTime,username,comments, Outcome2) %>%
  dplyr::summarise(n=n()) 

# create df containing caseids that did not answer any of the elig questions
# include those that did not answer E10
no.elig <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  filter(is.na(E2_1)) %>%
  group_by(caseid) %>%
  summarise(frequency=n()) 


no.e10 <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  filter(is.na(E10_1)) %>%
  drop_na(E2_1) %>%
  group_by(caseid) %>%
  summarise(frequency=n())

refer.caseid <- merge(no.elig, no.e10)
write.csv(no.elig, paste0(r.functions.dir, 'no_elig.csv'))
write.csv(no.e10, paste0(r.functions.dir, 'no_e10.csv'))

datamw %>%
  filter(Outcome2 == 'REFER') %>%
  group_by(Outcome2) %>%
  summarise(frequency=n())


