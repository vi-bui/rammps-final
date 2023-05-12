################################################################################
## Response Rates Paper Code
## Vi Bui and Kelly McCaine 
################################################################################

################################################################################
## Load Libraries
################################################################################
pkgs <- c('purrr','plyr','tidyverse', 'plotrix','lubridate','kableExtra','hrbrthemes','ggplot2','extrafont','float','reshape',
          'gridExtra','rsvg','png','devtools','readxl','date', 'ggpubr', 'tidyselect', 'httr', 'jsonlite', 'extrafont', 'colorspace',
          'ggrepel', 'forcats', 'ggpubr', 'readstata13', 'cowplot', 'scales')
lapply(pkgs, require, character.only = TRUE)

dark2pal <- c("#1B9E77", "#D95F02", "#7570B3", "#66A61E", "#E6AB02")

################################################################################
## Reading in data
################################################################################

# Setting directories
r.functions.dir <- '/Users/lshvb5/Documents/rammps/'
dir.inputdata <- ('/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/UNIKIN/SurveyCTO Audits/')


# DRC
Consented <- read.csv(paste0(dir.inputdata, "Clean data/Consented-2023-03-07.csv"))%>%
  mutate(source = ifelse(grepl('Fer', Source.prov),'Feroxus',
                         ifelse(grepl('IVR', Source.prov),'IVR',NA)),
         Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2)),
         RuralUrban = case_when(E4a_lab == 'City' ~ 'Urban',
                                E4a_lab == 'Town/Trading Centre' ~ 'Urban',
                                E4a_lab == 'Rural' ~ 'Rural'))

data <- read.csv(paste0(dir.inputdata, "Clean data/Data_Long2023-03-07.csv"))%>%
  mutate(source = ifelse(grepl('Fer', Source.prov),'Feroxus',
                         ifelse(grepl('IVR', Source.prov),'IVR',NA)),
         Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2)),
         RuralUrban = case_when(E4a_lab == 'City' ~ 'Urban',
                                E4a_lab == 'Town/Trading Centre' ~ 'Urban',
                                E4a_lab == 'Rural' ~ 'Rural'))
# dat.wide <- read.csv(paste0(dir.inputdata, "Clean data/Data_Wide2022-08-23.csv")) %>%
dat.wide <- read.csv(paste0(dir.inputdata, "Clean data/Data_Wide-2023-03-07.csv")) %>%
  mutate(source = ifelse(grepl('Fer',Source.prov.1),'Feroxus',
                         ifelse(grepl('IVR',Source.prov.1),'IVR', NA)),
         Eligibility = case_when(Outcome.FINAL %in% c('COMP', 'INCO', 'PART') ~ 'Eligible',
                                 Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                 Outcome.FINAL %in% c('REFU','NNA','NR','NNA/NR','LANG','PEND') ~ 'Eligibility unknown'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligible Unknown', 'CATI Other Eligibility Unknown')),
         Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)))

# Malawi
Consentedmw <- read.csv(paste0(r.functions.dir, 'RaMMPS_MWApp/ConsentedMW.csv')) %>%
  mutate(Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2))) 
dat.widemw <- read.csv(paste0(r.functions.dir,'RaMMPS_MWApp/DatWide.csv'))%>%
  mutate(Eligibility = case_when(Outcome.FINAL %in% c('COMP', 'INCO', 'PART') ~ 'Eligible',
                                 Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                 Outcome.FINAL %in% c('REFU','NNA','NR','NNA/NR','LANG','PEND') ~ 'Eligibility unknown'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligible Unknown', 'CATI Other Eligibility Unknown')),
         Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)))
datamw <- read.csv(paste0(r.functions.dir,'RaMMPS_MWApp/DatLongMW.csv')) %>%
  dplyr::filter(!is.na(Outcome2)) %>%
  mutate(Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2))) 

# Burkina Faso 
dir.input <- "/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/Burkina/Survey Data/"
eval(parse(paste0(r.functions.dir,'CleaningScript_func_BF_RDD.R'), encoding="UTF-8"))
listbf <- clean_bf() #clean BF data

rdd.data <- listbf[[1]] %>%
  mutate(
    Outcome = ifelse(Outcome =='NNA'| Outcome =='NR', 'NNA/NR', as.character(Outcome)),
    Eligibility = case_when(Outcome %in% c('COMP','PART') ~ 'Eligible',
                            Outcome %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                            Outcome %in% c('REFU','NNA','NR','LANG','PEND', 'NNA or NR', 'Other') ~ 'Eligibility unknown'),
    Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligibility Unknown', 'CATI Other Eligibility Unknown')),
    Outcome = ifelse(Outcome =='NNA'| Outcome =='NR', 'NNA/NR', as.character(Outcome)),
    gender = case_when(gender==1 ~ "Male",
                       gender==2 ~ "Female"),
    Resp.Sex = gender)

hello <- rdd.data[grep("-Refer", rdd.data$caseid), ]
hello$caseid

# ehcvm.data <- listbf[[2]]
rdd.dat.wide <- listbf[[2]] %>%
  mutate(
    Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)),
    Eligibility = case_when(Outcome.FINAL %in% c('COMP','PART') ~ 'Eligible',
                            Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                            Outcome.FINAL %in% c('REFU','NNA','NR','LANG','PEND', 'NNA or NR', 'Other') ~ 'Eligibility unknown'),
    Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligibility Unknown', 'CATI Other Eligibility Unknown')),
    Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)),
    gender = case_when(gender==1 ~ "Male",
                       gender==2 ~ "Female"),
    Resp.Sex = gender)


# ehcvm.dat.wide <- listbf[[4]]
bf.dat.wide <- listbf[[4]]
bf.data <- listbf[[3]]
Consentedrdd <- rdd.data %>%
  filter(Outcome == 'COMP') %>%
  dplyr::mutate(month.interview1 = lubridate::floor_date(as.Date(Date.Interview), 'month'),
                gender = case_when(is.na(gender) ~ 'Male', TRUE ~ as.character(gender)),
                Resp.Age.pyr = cut(as.numeric(age), c(14,19,29,39,49,59,64))
  )#month(Date.Interview),

# Consentedehcvm <- ehcvm.data %>%
#   filter(Outcome == 'COMP') %>%
#   dplyr::mutate(month.interview1 = lubridate::floor_date(as.Date(Date.Interview), 'month'),#month(Date.Interview),
#                 month.interview = case_when(month.interview == '2022-01-01'~ 'Jan-22',
#                                             month.interview == '2022-02-01'~ 'Feb-22',
#                                             month.interview == '2022-03-01'~ 'Mar-22',
#                                             month.interview == '2022-04-01'~'Apr-22',
#                                             month.interview == '2022-05-01'~'May-22',
#                                             month.interview == '2022-06-01'~'Jun-22',
#                                             month.interview == '2022-07-01'~'Jul-22',
#                                             month.interview == '2022-08-01'~'Aug-22',
#                                             month.interview == '2022-09-01'~'Sep-22',
#                                             month.interview == '2021-08-01'~'Aug-21',
#                                             month.interview == '2021-09-01'~'Sep-21',
#                                             month.interview == '2021-10-01'~'Oct-21',
#                                             month.interview == '2021-11-01'~'Nov-21',
#                                             month.interview == '2021-12-01'~'Dec-21'))
Consentedbf <- bf.data %>%
  filter(Outcome == 'COMP')

################################################################################
## Response Rate Calculations
################################################################################
# # response rate 1 COMP / [COMP+ PART+ REFU + NR (5 attempts) + NNA (5attempts)+LANG]
# # DRC 
# ferkin <- (dat.wide) %>%
#   filter(Source.prov.1 == 'Feroxus-Kin') %>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# 
# # Adding the Resp.Sex data to the dat.wide df 
# # First extrace the Resp.Sex column along with caseid as the key
# dat.wide.sex <- (data) %>%
#   select(caseid, Resp.Sex)
# # Join the Resp.Sex column dat.wide df through inner_join based on caseid as key
# # select_sex <- dat.wide %>%
# #   inner_join(dat.wide.sex, by = 'caseid', na.rm=True) %>% 
# #   distinct(caseid, .keep_all = TRUE) # one-to-one match
# 
# select_sex <- dat.wide %>%
#   left_join(dat.wide.sex, by = 'caseid', na.rm=True) %>% 
#   distinct(caseid, .keep_all = TRUE) # one-to-one match
# select_sex$Resp.Sex
# 
# # Check whether the combined sex dfs has the same number of entries 
# ferkin_sex <- select_sex %>%
#   filter(Source.prov.1 == 'Feroxus-Kin') %>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# 
# # Get all DRC based on sex
# ferkin_female <- select_sex %>%
#   filter(Source.prov.1 == 'Feroxus-Kin', Resp.Sex == 'Female') %>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# ferkin_male <- select_sex %>%
#   filter(Source.prov.1 == 'Feroxus-Kin', Resp.Sex == 'Male') %>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# 
# fernk_female <- select_sex %>%
#   filter(Source.prov_1 == 'Feroxus-NK', Resp.Sex == 'Female')%>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# fernk_male <- select_sex %>%
#   filter(Source.prov_1 == 'Feroxus-NK', Resp.Sex == 'Male')%>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# 
# ivrkin <- dat.wide %>%
#   filter(Source.prov.1 == 'IVR group 1-Kin')%>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# ivrkin_female <- select_sex %>%
#   filter(Source.prov.1 == 'IVR group 1-Kin', Resp.Sex == 'Female')%>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# ivrkin_male <- select_sex %>%
#   filter(Source.prov.1 == 'IVR group 1-Kin', Resp.Sex == 'Male')%>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# 
# ivrnk_female <- select_sex %>%
#   filter(Source.prov.1 == 'IVR group 1-NK', Resp.Sex == 'Female')%>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# ivrnk_male <- select_sex %>%
#   filter(Source.prov.1 == 'IVR group 1-NK', Resp.Sex == 'Male')%>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# ivr_female <- select_sex %>%
#   filter(Source.prov.1=='IVR group 1-NK'|Source.prov.1=='IVR group 1-Kin'|Source.prov.1=='IVR group 2', Resp.Sex == 'Female') %>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# ivr_male <- select_sex %>%
#   filter(Source.prov.1=='IVR group 1-NK'|Source.prov.1=='IVR group 1-Kin'|Source.prov.1=='IVR group 2', Resp.Sex == 'Male') %>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# fer_female <- select_sex %>%
#   filter(Source.prov.1=='Feroxus-Kin'|Source.prov.1=='Feroxus-NK', Resp.Sex == 'Female')%>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# fer_male <- select_sex %>%
#   filter(Source.prov.1=='Feroxus-Kin'|Source.prov.1=='Feroxus-NK', Resp.Sex == 'Male')%>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# alldrc_female <- select_sex %>%
#   filter(Resp.Sex == 'Female')%>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# alldrc_male <- select_sex %>%
#   filter(Resp.Sex == 'Male')%>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# alldrc <- dat.wide%>%
#   group_by(Outcome.FINAL) %>%
#   tally()

alldrc_female <- dat.wide %>%
  filter(Resp.Sex == 'Female')%>%
  group_by(Outcome.FINAL) %>%
  tally()
alldrc_male <- dat.wide %>%
  filter(Resp.Sex == 'Male')%>%
  group_by(Outcome.FINAL) %>%
  tally()

# MW
allmw_female <- dat.widemw %>%
  filter(Resp.Sex == "Female") %>%
  group_by(Outcome.FINAL) %>%
  tally()
allmw_male <- dat.widemw %>%
  filter(Resp.Sex == "Male") %>%
  group_by(Outcome.FINAL) %>%
  tally()

# BF
# Change the gender categorises into "Male" and "Female" first
# rdd.dat.wide.gender <- rdd.dat.wide %>%
#   select(gender) %>%
#   mutate(Resp.Sex = case_when(gender==1 ~ "Male",
#                           gender==2 ~ "Female"))  

rddbf_female <- rdd.dat.wide%>%
  filter(gender == "Female") %>%
  group_by(Outcome.FINAL) %>%
  tally()
rddbf_male <- rdd.dat.wide %>%
  filter(gender == "Male") %>%
  group_by(Outcome.FINAL) %>%
  tally()
# ehcvm.dat.wide.gender <-ehcvm.dat.wide %>%
#   mutate(gender = case_when(gender==1 ~ "Male",
#                             gender==2 ~ "Female"))
# ehcvmbf_female <- ehcvm.dat.wide.gender %>%
#   filter(gender == "Female") %>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# ehcvmbf_male <- ehcvm.dat.wide.gender %>%
#   filter(gender == "Male") %>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# bf.dat.wide.gender <-bf.dat.wide %>%
#   mutate(gender = case_when(gender==1 ~ "Male",
#                             gender==2 ~ "Female"))
# allbf_female <- bf.dat.wide.gender %>%
#   filter(gender == "Female") %>%
#   group_by(Outcome.FINAL) %>%
#   tally()
# allbf_male <- bf.dat.wide.gender %>%
#   filter(gender == "Male") %>%
#   group_by(Outcome.FINAL) %>%
#   tally()

# Create a list with all the sex dfs
# dfs <- list('ivrkin_female'=ivrkin_female, 
#             'ivrkin_male' =ivrkin_male, 
#             'ivrnk_male'=ivrnk_male, 
#             'ivrnk_female'=ivrnk_female, 
#             'alldrc_female'=alldrc_female,
#             'alldrc_male'=alldrc_male,
#             'alldrc'=alldrc,
#             'allmw_female' =allmw_female,
#             'allmw_male'=allmw_male,
#             'rddbf_female'=rddbf_female,
#             'rddbf_male'=rddbf_male,
#             'ehcvmbf_female'=ehcvmbf_female,
#             'ehcvmbf_male'=ehcvmbf_male,
#             'allbf_female'=allbf_female,
#             'allbf_male'=allbf_male)

dfs <- list('alldrc_female'=alldrc_female,
            'alldrc_male'=alldrc_male,
            'allmw_female' =allmw_female,
            'allmw_male'=allmw_male,
            'rddbf_female'=rddbf_female,
            'rddbf_male'=rddbf_male)

list_names <- names(dfs)
# to get estimation of e (percent eligible)
# OLD DEF: INEL / (INEL + COMP + PART + LANG + PEND(?) + DEFER)
# ELIG/ (ELIG+INEL) = (COMP + PART + DEFER) / ([INEL + NNU] + [COMP + PART + DEFER])
e_est <- function(df){
  elig <- round(sum(df[df$Outcome.FINAL=='COMP',]$n, df[df$Outcome.FINAL=='PART',]$n)  / 
                  sum(df[df$Outcome.FINAL=='INEL',]$n, df[df$Outcome.FINAL=='NNU',]$n, df[df$Outcome.FINAL=='COMP',]$n,
                      df[df$Outcome.FINAL=='PART',]$n,df[df$Outcome.FINAL=='DEFER',]$n), 2)
  return(elig)
}
e_est_func <- function(dfs){
  es_list <- list()
  
  for (i in 1:length(dfs)){
    e <- e_est(dfs[[i]])
    es_list[[i]] <- e
    names(es_list)[i] <- list_names[i]
  }
  return(es_list)
}
e_est_list <- e_est_func(dfs)

# function to calculate the sum of those that have unknown eligibility 
unk_inel <- function(df){
  suminel <- sum(df[df$Outcome.FINAL=='NR',]$n, df[df$Outcome.FINAL=='NNA',]$n, df[df$Outcome.FINAL=='LANG',]$n, 
                 df[df$Outcome.FINAL=='REFU',]$n, df[df$Outcome.FINAL=='Other',]$n, na.rm = TRUE)
  return(suminel)
}


rr13_func <- function(df, e){
  rr1 <- c()
  unk_inel1 <- unk_inel(df)
  rr1 <- round(df[df$Outcome.FINAL=='COMP',]$n / 
                 sum(df[df$Outcome.FINAL=='COMP',]$n, df[df$Outcome.FINAL=='PART',]$n, df[df$Outcome.FINAL=='DEFER',]$n, unk_inel1*e, na.rm = T) *100,2)
  rr1 <- c('RR1'=rr1, "p_elig"=e)
  return(rr1)
}

rr1s <- list()
rr1es <- list()
rr10s <- list()
for(i in 1:length(dfs)){
  rr1es[[i]] <- rr13_func(dfs[[i]], e = e_est_list[[i]])
  rr1s[[i]] <- rr13_func(dfs[[i]], e = 1)
  rr10s[[i]] <- rr13_func(dfs[[i]], e = 0)
}

rr24_func <- function(df, e){
  rr2 <- c()
  unk_inel1 <- unk_inel(df)
  rr2 <- round(sum(df[df$Outcome.FINAL=='COMP',]$n, df[df$Outcome.FINAL=='PART',]$n,na.rm=TRUE) / 
                 sum(df[df$Outcome.FINAL=='COMP',]$n, df[df$Outcome.FINAL=='PART',]$n, df[df$Outcome.FINAL=='DEFER',]$n, unk_inel1*e, na.rm = T) *100,2)
  rr2 <- c('RR2'=rr2, "p_elig"=e)
  return(rr2)
}

# rr2s <- rr13_func(dfs, e= 1) 
rr2s <- list()
rr2es <- list()
rr20s <- list()
for(i in 1:length(dfs)){
  rr2es[[i]] <- rr24_func(dfs[[i]], e = e_est_list[[i]])
  rr2s[[i]] <- rr24_func(dfs[[i]], e = 1)
  rr20s[[i]] <- rr24_func(dfs[[i]], e = 0)
}

# Response rate among only those who have been contacted
rr5_func <- function(df, e){
  rr2 <- c()
  unk_inel1 <- unk_inel(df)
  rr2 <- round(df[df$Outcome.FINAL=='COMP',]$n / 
                 sum(df[df$Outcome.FINAL=='COMP',]$n, df[df$Outcome.FINAL=='PART',]$n, df[df$Outcome.FINAL=='LANG',]$n, df[df$Outcome.FINAL=='REFU',]$n,  na.rm = T) *100,2)
  rr2 <- c('RR2'=rr2, "p_elig"=e)
  return(rr2)
}

# rr2s <- rr13_func(dfs, e= 1) 
rr5s <- list()
for(i in 1:length(dfs)){
  rr5s[[i]] <- rr5_func(dfs[[i]], e = 1)
}

# Calculating refusal rates
refr12_func <- function(df, e){
  refr1 <- c()
  unk_inel1 <- unk_inel(df)
  refr1 <- round(df[df$Outcome.FINAL=='REFU',]$n / 
                   sum(df[df$Outcome.FINAL=='COMP',]$n, df[df$Outcome.FINAL=='PART',]$n, df[df$Outcome.FINAL=='DEFER',]$n, unk_inel1*e, na.rm = T) *100,2)
  refr1 <- c('RefR'=refr1, "p_elig"=e)
  return(refr1)
}

refr1s <- list()
refr1es <- list()
refr10s <- list()
for(i in 1:length(dfs)){
  refr1es[[i]] <- refr12_func(dfs[[i]], e = e_est_list[[i]])
  refr1s[[i]] <- refr12_func(dfs[[i]], e = 1)
  refr10s[[i]] <- refr12_func(dfs[[i]], e = 0)
}

# Interview completion rate
# COMP/COMP+PART
interview_comp_func <- function(df){
  comp_rate <- round(df[df$Outcome.FINAL=='COMP',]$n/ 
                       sum(df[df$Outcome.FINAL=='COMP',]$n,df[df$Outcome.FINAL=='PART',]$n, na.rm = TRUE)*100,2)
  return(comp_rate)
}

interview_comp <- list()
for(i in 1:length(dfs)){
  interview_comp[[i]] <- interview_comp_func(dfs[[i]])
}


# Eligibility Screener Completion Rate
# COMP+PART+NNU+INEL+DEFER/COMP+PART+NNU+INEL+DEFER+REFU+LANG+NR+NNA+PEND
screener_func <- function(df){
  unk_inel1 <- unk_inel(df)
  screener_rate <- round(sum(df[df$Outcome.FINAL=='COMP',]$n, df[df$Outcome.FINAL=='INEL',]$n, df[df$Outcome.FINAL=='PART',]$n, df[df$Outcome.FINAL=='DEFER',]$n)/ 
                           sum(df[df$Outcome.FINAL=='COMP',]$n, df[df$Outcome.FINAL=='INEL',]$n, df[df$Outcome.FINAL=='PART',]$n, df[df$Outcome.FINAL=='DEFER',]$n, unk_inel1, na.rm = TRUE)*100,2)
  return(screener_rate)
}

screener_comp <- list()
for(i in 1:length(dfs)){
  screener_comp[[i]] <- screener_func(dfs[[i]])
}

# Processing the output of these RRs into nice tables
rrs <- list('rr1s'=rr1s, #'rr3s_0.2'=rr3s_0.2, 'rr3s_0.3'=rr3s_0.3, 'rr3s_0.5'=rr3s_0.5, 'rr3s_0.8'=rr3s_0.8,
            'rr1es'=rr1es,
            'rr10s' = rr10s,
            'rr2s'=rr2s, #'rr4s_0.2'=rr4s_0.2, 'rr4s_0.3'=rr4s_0.3, 'rr4s_0.5'=rr4s_0.5, 'rr4s_0.8'=rr4s_0.8,
            'rr2es'=rr2es,
            'rr20s'=rr20s,
            'rr5s'=rr5s,
            'refr1s'=refr1s,
            'refr1es'=refr1es,
            'refr10s'=refr10s,
            'interview_comp'=interview_comp,
            'screener_comp'=screener_comp) #'refr2s_0.2'=refr2s_0.2, 'refr2s_0.3'=refr2s_0.3, 'refr2s_0.5'=refr2s_0.5, 'refr2s_0.8'=refr2s_0.8)
rrs_df <- purrr::map_df(rrs, bind_cols) %>% as.data.frame() 
rrs_df <- rrs_df[-c(2,6,8,10,12,14,16,18,20),]
rownames(rrs_df) <- c('RR1', #'RR3, e=0.2', 'RR3, e=0.3','RR3, e=0.5','RR3, e=0.8',
                      'RR1e',
                      'P_Elig',
                      'RR1_0',
                      'RR2', #'RR4, e=0.2', 'RR4, e=0.3', 'RR4, e=0.5', 'RR4, e=0.8',
                      'RR2e',
                      'RR2_0',
                      'RR5',
                      # 'RR5e',
                      'RefR1',#'RefR2, e=0.2','RefR2, e=0.3','RefR2, e=0.5','RefR2, e=0.8'
                      'RefR1e',
                      'RefR1_0',
                      'interview_comp',
                      'screener_comp')
colnames(rrs_df) <- names(dfs)

write.csv(rrs_df, paste0('/Users/lshvb5/Documents/rammps/', 'response_data-', Sys.Date(),'.csv'))


# # Adding the Resp.Sex data to the dat.wide df 
# # First extrace the Resp.Sex column along with caseid as the key
# dat.wide.females <- data %>%
#   select(caseid, Resp.Sex)
# # Join the Resp.Sex column dat.wide df through inner_join based on caseid as key
# select_sex <- dat.wide %>%
#   inner_join(dat.wide.females, by = c('caseid'='caseid'))
# # Check the number of sex and percentages newly joined df
# ferkin_sex <- select_sex %>%
#   group_by(Resp.Sex) %>%
#   dplyr::summarise(n = n()) %>%
#   mutate(percentages = (n/sum(n))*100)
# # Check number of sex and percentages against original df
# original_df <- data %>%
#   group_by(Resp.Sex)%>%
#   dplyr::summarize(n = n()) %>%
#   mutate(percentages = (n/sum(n))*100)
# # Split df based on sex
# split_sex_drc<-split(ferkin_sex, ferkin_sex$Resp.Sex)
# drc_females <- split_sex_drc[[1]] # used to access either characteristic
# drc_males <- split_sex_drc[[2]]
# str(split_sex_drc)


# Total CATIs completed
# DRC
CATIs_drc_female <- rbind(
  Consented %>%
    filter(Resp.Sex == "Female")  %>%
    group_by(Source.prov) %>%
    dplyr::summarise(catis = n())  %>%
    dplyr::rename(group = Source.prov) ,
  Consented %>% 
    filter(Resp.Sex == "Female")  %>%
    group_by(source) %>%
    dplyr::summarise(catis = n()) %>%
    dplyr::rename(group = source),
  Consented %>%
    filter(Resp.Sex == "Female")  %>%
    dplyr::summarise(catis=n()) %>%
    mutate(group = 'drc')
)
CATIs_drc_male <- rbind(
  Consented %>%
    filter(Resp.Sex == "Male")  %>%
    group_by(Source.prov) %>%
    dplyr::summarise(catis = n())  %>%
    dplyr::rename(group = Source.prov) ,
  Consented %>% 
    filter(Resp.Sex == "Male")  %>%
    group_by(source) %>%
    dplyr::summarise(catis = n()) %>%
    dplyr::rename(group = source),
  Consented %>%
    filter(Resp.Sex == "Male")  %>%
    dplyr::summarise(catis=n()) %>%
    mutate(group = 'drc')
)

mwcatis <- nrow(Consentedmw) # total MW completed 
mwcatis_female <- Consentedmw %>%
  filter(Resp.Sex == "Female") %>%
  dplyr::summarise(sum = n())
mwcatis_male <- Consentedmw %>%
  filter(Resp.Sex == "Male") %>%
  dplyr::summarise(sum = n())
(mwcatis_male/(mwcatis_female+mwcatis_male))*100
(mwcatis_female/(mwcatis_female+mwcatis_male))*100


rddbfcatis <- nrow(rdd.dat.wide[rdd.dat.wide$Outcome.FINAL=='COMP',])
rddbfcatis

rddcatis_female <- rdd.dat.wide %>%
  filter(Outcome.FINAL=='COMP', gender=="Female") %>%
  dplyr::summarise(sum = n())
rddcatis_male <- rdd.dat.wide%>%
  filter(Outcome.FINAL=='COMP', gender=="Male") %>%
  dplyr::summarise(sum = n())
rddcatis_male/((rddcatis_female+rddcatis_male))*100
rddcatis_female/((rddcatis_female+rddcatis_male))*100

# calls placed + per completed
drccalls_female <- rbind(data %>%
                         filter(Resp.Sex == 'Female') %>%
                         group_by(Source.prov) %>%
                         dplyr::summarize(calls=n()) %>%
                         dplyr::rename(group = Source.prov) ,
                       data %>%
                         filter(Resp.Sex == 'Female') %>%
                         group_by(source) %>%
                         dplyr::summarize(calls=n()) %>%
                         dplyr::rename(group = source) ,
                       data %>% 
                         filter(Resp.Sex == 'Female') %>%
                         dplyr::summarize(calls=n()) %>%
                         mutate(group = 'drc') 
)
drccalls_male <- rbind(data %>%
                    filter(Resp.Sex == 'Male') %>%
                    group_by(Source.prov) %>%
                    dplyr::summarize(calls=n()) %>%
                    dplyr::rename(group = Source.prov) ,
                  data %>%
                    filter(Resp.Sex == 'Male') %>%
                    group_by(source) %>%
                    dplyr::summarize(calls=n()) %>%
                    dplyr::rename(group = source) ,
                  data %>% 
                    filter(Resp.Sex == 'Male') %>%
                    dplyr::summarize(calls=n()) %>%
                    mutate(group = 'drc') 
)

mwcalls_female <- datamw %>%
  filter(Resp.Sex == 'Female') %>%
  dplyr::summarise(calls = n()) %>%
  mutate(group = 'MW')
mwcalls_male <- datamw %>%
  filter(Resp.Sex == 'Male') %>%
  dplyr::summarise(calls = n()) %>%
  mutate(group = 'MW')

rddcalls_female <- rdd.dat.wide %>%
  filter(gender == 'Female') %>%
  dplyr::summarise(calls = n()) %>%
  mutate(group = 'RDD BF')
rddcalls_male <- rdd.dat.wide %>%
  filter(gender == 'Male') %>%
  dplyr::summarise(calls = n()) %>%
  mutate(group = 'RDD BF')

calls <- rbind(drccalls_female, drccalls_male, mwcalls_female, mwcalls_male, rddcalls_female, rddcalls_male)
calls

# Call attempts per completed CATI
drc_call_female <- left_join(drccalls_female, CATIs_drc_female) %>%
  mutate(attperCATI = calls/catis)
drc_call_male <- left_join(drccalls_male, CATIs_drc_male) %>%
  mutate(attperCATI = calls/catis)

mw_call_female <- mwcalls_female$calls/mwcatis_female
mw_call_male <- mwcalls_male$calls/mwcatis_male

rddatt_female <- rddcalls_female$calls/rddcatis_female 
rddatt_male <- rddcalls_male$calls/rddcatis_male 

# numbers w/ completed cati (CATIs/total #s per group)
drcnums_female <- rbind(dat.wide %>%
                   filter(Resp.Sex == 'Female') %>%
                   group_by(Source.prov.1) %>%
                   dplyr::summarize(nums = n()) %>%
                   dplyr::rename(group = Source.prov.1),
                   dat.wide %>%
                   filter(Resp.Sex == 'Female') %>%
                   group_by(source) %>%
                   dplyr::summarize(nums =n()) %>%
                   dplyr::rename(group = source) ,
                   dat.wide %>% ungroup() %>%
                   filter(Resp.Sex == 'Female') %>%
                   dplyr::summarize(nums = n()) %>%
                   mutate(group = 'drc'))

drcperc_female <- left_join(drc_call_female, drcnums_female) %>%
  mutate(percnumswCATI = catis/nums * 100)
drcperc_female

drcnums_male <- rbind(dat.wide %>%
                          filter(Resp.Sex == 'Male') %>%
                          group_by(Source.prov.1) %>%
                          dplyr::summarize(nums = n()) %>%
                          dplyr::rename(group = Source.prov.1),
                      dat.wide %>%
                          filter(Resp.Sex == 'Male') %>%
                          group_by(source) %>%
                          dplyr::summarize(nums =n()) %>%
                          dplyr::rename(group = source) ,
                      dat.wide %>% ungroup() %>%
                          filter(Resp.Sex == 'Male') %>%
                          dplyr::summarize(nums = n()) %>%
                          mutate(group = 'drc'))
drcperc_male <- left_join(drc_call_male, drcnums_male) %>%
  mutate(percnumswCATI = catis/nums * 100)
drcperc_male

mwperc_female <- mwcatis_female/mwcalls_female$calls*100
mwperc_male <- mwcatis_male/mwcalls_male$calls*100

mwperc <- mwcatis/nrow(dat.widemw)*100#[dat.widemw$Outcome.FINAL=='COMP',])
mwperc
mwperc_female <- mwcatis_female/nrow(dat.widemw)*100
mwperc_female
mwperc_male <- mwcatis_male/nrow(dat.widemw)*100
mwperc_male

rddperc <- rddbfcatis/nrow(rdd.dat.wide)*100
rddperc
rddperc_female <- rddcatis_female/nrow(rdd.dat.wide)*100
rddperc_female
rddperc_male <- rddcatis_male/nrow(rdd.dat.wide)*100
rddperc_male



rdd.dat.wide %>%
  group_by(gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(percentages = (n/sum(n))*100)

### CATI Completion rates by call order plot 
#DRC
outcomesdrc <- dat.wide %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
  dplyr::group_by(call_num_grp, Outcome.FINAL, Resp.Sex) %>%
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome.FINAL == 'COMP') %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(dat.wide), # get all the numbers in dat.wide
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'DRC') 
outcomesdrc <-outcomesdrc[complete.cases(outcomesdrc), ]
# MW
outcomesmw <- dat.widemw %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
  dplyr::group_by(call_num_grp, Outcome.FINAL, Resp.Sex) %>%
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome.FINAL == 'COMP') %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(dat.widemw),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'MW') 

#BF - RDD
outcomesrdd <- rdd.dat.wide %>%
  mutate(call_num_grp = ifelse(nbr_appel > 5, '5+',as.character(nbr_appel))) %>%
  dplyr::group_by(call_num_grp, Outcome.FINAL, Resp.Sex) %>%
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome.FINAL == 'COMP') %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(rdd.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'BF-RDD') %>%
  na.omit(Resp.Sex) # Remove rows with NA


# Combined df
data <- data %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num)))
datamw <- datamw %>% 
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num)))
rdd.data <- rdd.data %>% 
  mutate(call_num_grp = ifelse(nbr_appel > 5, '5+',as.character(nbr_appel)))

# outcomesdrc_join$Calls <- c(table(data$call_num_grp))
drc_data <- data %>%
  select(call_num_grp, Resp.Sex)
outcomesdrc$Calls <- c(table(data$call_num_grp, data$Resp.Sex))
outcomesmw$Calls <- c(table(datamw$call_num_grp, datamw$Resp.Sex))
outcomesrdd$Calls <- c(table(rdd.data$call_num_grp,rdd.data$Resp.Sex))


# Outcomes for rdd
outcomes_rdd <- rbind(outcomesdrc, outcomesmw, outcomesrdd) %>%
  mutate(country = forcats::fct_inorder(country))

################################################################################
## Call attempt order updated plots
################################################################################

ratio_cati <- max(outcomes_rdd$Calls)/max(outcomes_rdd$perccompletedpercallnum)

# MW
phone_call_cati_mw <- ggplot(data = outcomesmw) + 
  geom_line(aes(x = call_num_grp, y = perccompletedpercallnum, group=Resp.Sex, color = Resp.Sex), size = 0.8) + 
  geom_point(aes(x = call_num_grp, y =  perccompletedpercallnum, group=Resp.Sex,color = Resp.Sex), size = 2)+
  xlab('Call attempt order') +
  ylab('% of completed CATIs per call num') +
  theme_bw()+
  coord_cartesian(ylim = c(0,45)) + 
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  scale_color_manual(values = c('#D2042D','#0671B7')) +
  labs(color='Respondent Sex') # legend title
phone_call_cati_mw

# DRC
phone_call_cati_drc <- ggplot(data = outcomesdrc) + 
  geom_line(aes(x = call_num_grp, y = perccompletedpercallnum, group=Resp.Sex, color = Resp.Sex), size = 0.8) + 
  geom_point(aes(x = call_num_grp, y =  perccompletedpercallnum, group=Resp.Sex,color = Resp.Sex), size = 2) +
  xlab('Call attempt order') +
  ylab('% of completed CATIs per call num') +
  theme_bw()+
  coord_cartesian(ylim = c(0,45)) + 
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  scale_color_manual(values = c('#D2042D','#0671B7')) +
  labs(color='Respondent Sex')
phone_call_cati_drc 

# BF
phone_call_cati_bf <- ggplot(data = outcomesrdd) + 
  geom_line(aes(x = call_num_grp, y = perccompletedpercallnum, group=Resp.Sex, color = Resp.Sex), size = 0.8) + 
  geom_point(aes(x = call_num_grp, y =  perccompletedpercallnum, group=Resp.Sex,color = Resp.Sex), size = 2)+
  xlab('Call attempt order') +
  ylab('% of completed CATIs per call num') +
  theme_bw()+
  coord_cartesian(ylim = c(0,45))  + 
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  scale_color_manual(values = c('#D2042D','#0671B7')) +
  labs(color='Respondent Sex') 

# Combine the plots
CATIs_comb <- plot_grid(
  phone_call_cati_mw + theme(legend.position="none"),
  phone_call_cati_drc + theme(legend.position="none"),
  phone_call_cati_bf+ theme(legend.position="none"),
  labels = c("MW", "DRC", "BF"),
  label_size = 10,
  hjust = -0.25, vjust = 1.7)

# extract the legend from one of the plots
legend_plot_grid <- get_legend(
  phone_call_cati_bf + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 4),
          legend.key.size = unit(2, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15))
)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(CATIs_comb, legend_plot_grid, ncol = 1, rel_heights = c(1, .1))

################################################################################
## Average Phone Call Duration
################################################################################

library(scales)
# MW
# phonedur_mw <- dat.widemw %>%
#   mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
#   filter(Outcome.FINAL == 'COMP') %>%
#   group_by(call_num_grp, Resp.Sex) %>%
#   summarise(total = n(),
#             sum_phone_dur = sum(phone_call_duration/60),
#             avg_phone_dur = mean(phone_call_duration/60))

phonedurcon_mw <- Consentedmw %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num)))%>%
  filter(Outcome2 == 'COMP') %>%
  group_by(call_num_grp, Resp.Sex)%>%
  dplyr::summarise(total = n(),
            sum_phone_dur = sum(phone_call_duration/60),
            avg_phone_dur = mean(phone_call_duration/60))

phonedurcon_mw <- Consentedmw %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num)),
         total_phone_dur = sum(phone_call_duration/60))%>%
  filter(Outcome2 == 'COMP') %>%
  group_by(call_num_grp, Resp.Sex)%>%
  dplyr::summarise(total = n(),
                   sum_phone_dur = (phone_call_duration/60),
                   avg_phone_dur = (sum_phone_dur/total_phone_dur))

aggregate(cbind(phonedurcon_mw$sum_phone_dur, phonedurcon_mw$avg_phone_dur), list(phonedurcon_mw$call_num_grp,phonedurcon_mw$Resp.Sex, phonedurcon_mw$total), mean)


phonedur_mw_plot <- ggplot(phonedurcon_mw) +
  geom_point(aes(x = call_num_grp,y= avg_phone_dur, group= Resp.Sex, color = Resp.Sex))+
  geom_line(aes(x = call_num_grp,y= avg_phone_dur, group= Resp.Sex, color = Resp.Sex)) +
  xlab('Call attempt order') +
  ylab('Average Phone Call Duration (mins)') +
  theme_bw()+
  coord_cartesian(ylim = c(0,20))  + 
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  scale_color_manual(values = c('#D2042D','#0671B7')) +
  labs(color='Respondent Sex') 

# DRC

# # Merge phone_call_duration from the data df to the select_sex one
# # Select the caseid and phone_call_duration from the data df
# drc_phone_dur <- data %>%
#   select(caseid, phone_call_duration)
# 
# # Join the phone_call_duration from data df onto the select_sex df
# phone_dur_drc <- select_sex %>%
#   left_join(drc_phone_dur, by = 'caseid', na.rm=True)
# 
# 
# phonedur_drc <- phone_dur_drc %>%
#   mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
#   filter(Outcome.FINAL == 'COMP') %>%
#   group_by(call_num_grp, Resp.Sex) %>%
#   summarise(total = n(),
#             sum_phone_dur = sum(phone_call_duration/60),
#             avg_phone_dur = mean(phone_call_duration/60)) %>%
#   na.omit(phone_dur_drc) # Remove rows with NA

phonedurcon_drc <- Consented %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
  filter(Outcome2 == 'COMP') %>%
  group_by(call_num_grp, Resp.Sex) %>%
  dplyr::summarise(total = n(),
            sum_phone_dur = sum(phone_call_duration/60),
            avg_phone_dur = mean(phone_call_duration/60)) 

phonedur_drc_plot <- ggplot(phonedurcon_drc) +
  geom_point(aes(x = call_num_grp,y= avg_phone_dur, group= Resp.Sex, color = Resp.Sex))+
  geom_line(aes(x = call_num_grp,y= avg_phone_dur, group= Resp.Sex, color = Resp.Sex))  +
  xlab('Call attempt order') +
  ylab('Average Phone Call Duration (mins)') +
  theme_bw()+
  coord_cartesian(ylim = c(0,20))  + 
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  scale_color_manual(values = c('#D2042D','#0671B7')) +
  labs(color='Respondent Sex') 

# BF
# 
# # Convert phone_call_duration from characters to numeric
# rdd.dat.wide$phone_call_duration <- as.numeric(rdd.dat.wide$phone_call_duration)
# 
# # Remove rows with zeros
# # Some interviews were conducted outside the Survey CTO app (calls with duration=0) 
# phone_dur_bf <- rdd.dat.wide[apply(rdd.dat.wide!=0, 1, all),]
# phone_dur_bf <- phone_dur_bf %>%
#   filter(complete.cases(.))
# 
# phonedur_bf <- phone_dur_bf %>%
#   mutate(call_num_grp = ifelse(nbr_appel > 5, '5+',as.character(nbr_appel))) %>%
#   filter(Outcome.FINAL == 'COMP') %>%
#   group_by(call_num_grp, Resp.Sex) %>%
#   summarise(total = n(),
#             sum_phone_dur = sum(phone_call_duration/60),
#             avg_phone_dur = mean(phone_call_duration/60))

# Convert phone_call_duration from characters to numeric
Consentedrdd$phone_call_duration <- as.numeric(Consentedrdd$phone_call_duration)

phonedur_bf <- Consentedrdd %>%
  mutate(call_num_grp = ifelse(nbr_appel > 5, '5+',as.character(nbr_appel))) %>%
  filter(Outcome == 'COMP') %>%
  group_by(call_num_grp, Resp.Sex) %>%
  dplyr::summarise(total = n(),
            sum_phone_dur = sum(phone_call_duration/60),
            avg_phone_dur = mean(phone_call_duration/60))%>%
  na.omit(Resp.Sex) # Remove rows with NA

phonedur_bf_plot <- ggplot(phonedur_bf) +
  geom_point(aes(x = call_num_grp,y= avg_phone_dur, group= Resp.Sex, color = Resp.Sex))+
  geom_line(aes(x = call_num_grp,y= avg_phone_dur, group= Resp.Sex, color = Resp.Sex)) +
  xlab('Call attempt order') +
  ylab('Average Phone Call Duration (mins)') +
  theme_bw()+
  coord_cartesian(ylim = c(0,20))  + 
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  scale_color_manual(values = c('#D2042D','#0671B7')) +
  labs(color='Respondent Sex') 

# Combine the plots
phone_dur_comb <- plot_grid(
  phonedur_mw_plot + theme(legend.position="none"),
  phonedur_drc_plot + theme(legend.position="none"),
  phonedur_bf_plot+ theme(legend.position="none"),
  labels = c("MW", "DRC", "BF"),
  label_size = 10,
  hjust = -0.25, vjust = 1.7)

# extract the legend from one of the plots
legend_phone_dur <- get_legend(
  phonedur_mw_plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 4),
          legend.key.size = unit(2, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15))
)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(phone_dur_comb, legend_phone_dur, ncol = 1, rel_heights = c(1, .1))

################################################################################
##  Age and sex distribution
################################################################################
#MW
age_sex_mw <- Consentedmw %>%
  group_by(Resp.Age.pyr, Resp.Sex) %>%
  dplyr::summarize(population = n()) %>%
  ungroup() %>%
  group_by(Resp.Sex) %>%
  dplyr::mutate(npersex = sum(population)) %>% # sum per sex
  ungroup() %>%
  mutate(relfreq = population/npersex*100, 
         country = "MW")

age_sex_mw_plot <- ggplot(data = age_sex_mw) +
  geom_bar(stat = "identity",aes(x=Resp.Age.pyr, y = relfreq, fill=Resp.Sex), position = "dodge2") + 
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  ylab('Relative Frequency of Respondents (%)') + xlab('Respondent Age') +
  scale_x_discrete(labels = c('15-19','20-29','30-39','40-49','50-59','60-64')) +
  theme_bw() +
  labs(fill='Respondent Sex') +
  coord_cartesian(ylim = c(0,50)) 
ggsave("age_sex_mw_plot.png", dpi=600, units = "in", )

# DRC
age_sex_drc <- Consented %>%
  group_by(Resp.Age.pyr, Resp.Sex) %>%
  dplyr::summarize(population = n()) %>%
  ungroup() %>%
  group_by(Resp.Sex) %>%
  dplyr::mutate(npersex = sum(population)) %>% # sum per sex
  ungroup() %>%
  mutate(relfreq = population/npersex*100, 
         country = "DRC")

age_sex_drc_plot<- ggplot(data = age_sex_drc) +
  geom_bar(stat = "identity",aes(x=Resp.Age.pyr, y = relfreq, fill=Resp.Sex), position = "dodge2") + 
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  ylab('Relative Frequency of Respondents (%)') + xlab('Respondent Age') +
  scale_x_discrete(labels = c('15-19','20-29','30-39','40-49','50-59','60-64')) +
  theme_bw() +
  labs(fill='Respondent Sex') +
  coord_cartesian(ylim = c(0,50)) 

# BF
age_sex_bf <- Consentedrdd %>%
  group_by(Resp.Age.pyr, Resp.Sex) %>%
  dplyr::summarize(population = n()) %>%
  ungroup() %>%
  group_by(Resp.Sex) %>%
  dplyr::mutate(npersex = sum(population)) %>% # sum per sex
  ungroup() %>%
  mutate(relfreq = population/npersex*100, 
         country = "BF")%>%
  na.omit(age_sex_bf) # Remove rows with NA

age_sex_bf_plot <- ggplot(data = age_sex_bf) +
  geom_bar(stat = "identity",aes(x=Resp.Age.pyr, y = relfreq, fill=Resp.Sex), position = "dodge2") + 
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  ylab('Relative Frequency of Respondents (%)') + xlab('Respondent Age') +
  scale_x_discrete(labels = c('15-19','20-29','30-39','40-49','50-59','60-64')) +
  theme_bw() +
  labs(fill='Respondent Sex') +
  coord_cartesian(ylim = c(0,50)) 

age_sex_df <- bind_rows(age_sex_mw, age_sex_drc, age_sex_bf)

ggsave("age_sex_bf_plot.png")

# Combine the plots
age_sex_comb <- plot_grid(
  age_sex_mw_plot + theme(legend.position="none"),
  age_sex_drc_plot + theme(legend.position="none"),
  age_sex_bf_plot+ theme(legend.position="none"),
  labels = c("MW", "DRC", "BF"),
  label_size = 10,
  hjust = -0.25, vjust = 1.7)

# extract the legend from one of the plots
legend_age_sex <- get_legend(
  age_sex_mw_plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.60, 4),
          legend.key.size = unit(2, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15))
)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(age_sex_comb, legend_age_sex, ncol = 1, rel_heights = c(1, .1))

################################################################################
##  Residence and sex distribution
################################################################################

unique(Consentedrdd$urban_rural)
Consentedrdd %>%
  filter(Outcome == 'COMP') %>%
  select(urban_rural) %>%
  group_by(urban_rural) %>%
  dplyr::summarise(n = n())

# MW
urban_sex_mw <- Consentedmw %>%
  group_by(RuralUrban, Resp.Sex) %>%
  dplyr::summarize(population = n()) %>%
  ungroup() %>%
  group_by(Resp.Sex) %>%
  dplyr::mutate(npersex = sum(population)) %>% # sum per sex
  ungroup() %>%
  mutate(relfreq = population/npersex*100, 
         Group = "Total")

urban_sex_mw_plot <- ggplot(urban_sex_mw) +
  geom_bar(aes(x=RuralUrban, y=relfreq, fill=Resp.Sex), stat = "identity",position = "dodge2")+ 
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  ylab('Relative Frequency of Respondents (%)') + xlab(' ') +
  theme_bw() +
  labs(fill='Respondent Sex') +
  coord_cartesian(ylim = c(0,100)) 

# DRC
urban_sex_drc <- Consented %>%
  group_by(E4a_lab, Resp.Sex) %>%
  dplyr::summarize(population = n()) %>%
  ungroup() %>%
  group_by(Resp.Sex) %>%
  dplyr::mutate(npersex = sum(population)) %>% # sum per sex
  ungroup() %>%
  mutate(relfreq = population/npersex*100, 
         Group = "Total")%>%
  na.omit(urban_sex_drc) # Remove rows with NA

urban_sex_drc_plot <- ggplot(urban_sex_drc) +
  geom_bar(aes(x=E4a_lab, y=relfreq, fill=Resp.Sex), stat = "identity",position = "dodge2") + 
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  ylab('Relative Frequency of Respondents (%)') + xlab(' ') +
  theme_bw() +
  labs(fill='Respondent Sex') +
  coord_cartesian(ylim = c(0,100)) 

# BF
urban_sex_bf <- Consentedrdd %>%
  filter(Outcome == "COMP") %>%
  group_by(urban_rural, Resp.Sex) %>%
  dplyr::summarize(population = n()) %>%
  ungroup() %>%
  group_by(Resp.Sex) %>%
  dplyr::mutate(npersex = sum(population)) %>% # sum per sex
  ungroup() %>%
  mutate(relfreq = population/npersex*100, 
         Group = "Total")%>%
  na.omit(urban_sex_bf) # Remove rows with NA

urban_sex_bf_plot <- ggplot(urban_sex_bf) +
  geom_bar(aes(x=urban_rural, y=relfreq, fill=Resp.Sex), stat = "identity",position = "dodge2")+ 
  scale_fill_manual(values = c('#D2042D','#0671B7')) +
  ylab('Relative Frequency of Respondents (%)') + xlab(' ') +
  theme_bw() +
  labs(fill='Respondent Sex') +
  coord_cartesian(ylim = c(0,100)) 

# Combine the plots
age_urban_comb <- plot_grid(
  urban_sex_mw_plot + theme(legend.position="none"),
  urban_sex_drc_plot + theme(legend.position="none"),
  urban_sex_bf_plot+ theme(legend.position="none"),
  labels = c("MW", "DRC", "BF"),
  label_size = 10,
  hjust = -0.25, vjust = 1.7)

# extract the legend from one of the plots
legend_urban_sex <- get_legend(
  urban_sex_mw_plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.60, 4),
          legend.key.size = unit(2, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15))
)
# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(age_urban_comb, legend_urban_sex, ncol = 1, rel_heights = c(1, .1))

################################################################################
## Success of referrals
################################################################################

caseid_refer <- grep("-Refer",Consentedmw$caseid, value = TRUE) 

grep("DEFER",dat.widemw$Outcome.FINAL, value = TRUE) 

# select caseid of those who were referred
# get data from the calls df
caseid_refer <- datamw %>%
  filter(Outcome2 == "REFER") %>%
  select(caseid)
# get the refer caseids from the CATIs df
catis_refer <- dat.widemw[dat.widemw$caseid %in% caseid_refer$caseid,]

# merge(x=caseid_refer,y=dat.widemw,by="caseid",all.x=TRUE)
# 
# dat.widemw %>%
#   filter(caseid %in% caseid_refer$caseid)
# 
# subset(dat.widemw, gene_ID %in% accessions40$V1)
# 
# duplicated(dat.widemw$caseid)
# dat.widemw$caseid[duplicated(dat.widemw$caseid)]


catis_refer_res <-catis_refer %>% 
  select(Outcome.FINAL) %>%
  mutate(total = n()) %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(sum = n(),
                   percent = (sum/total)*100)
# Collapsing df based on CATI outcome
aggregate(cbind(catis_refer_res$sum, catis_refer_res$percent), list(catis_refer_res$Outcome.FINAL), mean)


consented_refer <- Consentedmw[Consentedmw$caseid %in% caseid_refer$caseid,]

consented_refer %>% 
  select(Outcome2) %>%
  group_by(Outcome2) %>%
  dplyr::summarise(sum = n())


Consentedmw %>%
  filter(Outcome2 == "REFER") %>%
  select(caseid)

nrow(consented_refer)
nrow(Consentedmw)
Consentedmw$Outcome2


# MW
# based on the completed CATIs df, there are 72 referral cases
referred.caseid.mw <- dat.widemw[grep("-Refer", dat.widemw$caseid), ]

referred.caseid.df.mw <- referred.caseid.mw %>%
  select(Outcome.FINAL) %>%
  mutate(total = n()) %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(sum = n(),
                  percent = (sum/total)*100)
# 29 of the referral cases resulted in compelted interviews
# aggregate both the sum and percentage
aggregate(cbind(referred.caseid.df.mw$sum, referred.caseid.df.mw$percent), list(referred.caseid.df.mw$Outcome.FINAL), mean)
# aggregate just the sum
grouped.caseid.mw <- aggregate(referred.caseid.df.mw$sum, list(referred.caseid.df.mw$Outcome.FINAL), mean)
# rename the columns
names(grouped.caseid.mw)[names(grouped.caseid.mw) == 'Group.1'] <- 'Outcome'
names(grouped.caseid.mw)[names(grouped.caseid.mw) == 'x'] <- 'Count'
# insert country column
referred.caseid.df.mw <- referred.caseid.df.mw %>%
  mutate(country = "MW")

sum(referred.caseid.df.mw$sum)

# DRC
referred.caseid.drc <- dat.wide[grep("-Refer", dat.wide$caseid), ]
referred.caseid.df.drc <- referred.caseid.drc %>%
  select(Outcome.FINAL) %>%
  mutate(total = n()) %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(sum = n(),
                   percent = (sum/total)*100)
# aggregate both the sum and percentage
aggregate(cbind(referred.caseid.df.drc$sum, referred.caseid.df.drc$percent), list(referred.caseid.df.drc$Outcome.FINAL), mean)
# aggregate just the sum
grouped.caseid.drc <- aggregate(referred.caseid.df.drc$sum, list(referred.caseid.df.drc$Outcome.FINAL), mean)
# rename the columns
names(grouped.caseid.drc)[names(grouped.caseid.drc) == 'Group.1'] <- 'Outcome'
names(grouped.caseid.drc)[names(grouped.caseid.drc) == 'x'] <- 'Count'
# insert country column
referred.caseid.df.drc <- referred.caseid.df.drc %>%
  mutate(country = "DRC")

# total <-  bind_rows(grouped.caseid.mw, grouped.caseid.drc)
total <-  bind_rows(referred.caseid.df.mw, referred.caseid.df.drc)
names(total)[names(total) == 'Outcome.FINAL'] <- 'Outcome'


# total<- total %>%
#   mutate(country = case_when(is.na(country) ~ "DRC", TRUE ~ as.character(country)))

# create table
library(table1)
table1(~ factor(Outcome)  | country,data = total)


#######################
## Check the dates
library(lubridate)
# sorting the format of the dates
referred.caseid.mw[order(as.Date(referred.caseid.mw$time, format="%Y/%m/%d")),]
# sort from least recent to most recent
referred.caseid.mw <- referred.caseid.mw %>%
  arrange(desc(ymd(referred.caseid.mw$time)))
referred.caseid.mw$time
  
# E10
# Is there someone in your household with these characteristics that we could talk to? 
# MW
unique(dat.widemw$E12_1)

datamw %>%
  select(E10_1) %>%
  drop_na(E10_1)  %>%
  dplyr::mutate(E10_1 = case_when(E10_1 == 1 ~ "Yes",
                          E10_1 == 2 ~ "No",
                          E10_1 == 3 ~ "Don't know",
                          E10_1 == 4 ~ "Refuse")) %>%
  group_by(E10_1) %>%
  dplyr::summarise(n = n()) %>% # 87 phone call said yes there is eligible person
  mutate(freq = n/sum(n)*100)

unique(dat.widemw$E12_1)

dat.widemw %>%
  select(E10_1) %>%
  dplyr::mutate(E10_1 = case_when(E10_1 == 1 ~ "known",
                                  E10_1 == 2 ~ "known",
                                  E10_1 == 3 ~ "known",
                                  E10_1 == 4 ~ "known")) %>%
  group_by(E10_1) %>%
  dplyr::summarise(n = n()) %>% # 87 phone call said yes there is eligible person
  mutate(freq = n/sum(n)*100)

# DRC
unique(dat.wide$inel.age)

dat.wide %>%
  select(inel.age) %>%
  drop_na(inel.age)  %>%
  dplyr::mutate(inel.age = case_when(inel.age == 1 ~ "Yes",
                                     inel.age == 2 ~ "No",
                                     inel.age == 3 ~ "Don't know",
                                     inel.age == 4 ~ "Refuse")) %>%
  group_by(inel.age) %>%
  dplyr::summarise(n = n()) %>% # 38 phone call said yes there is eligible person
  mutate(freq = n/sum(n)*100)

unique(dat.wide$inel.age)

rdd.dat.wide %>%
  select(e8) %>%
  drop_na(e8)  %>%
  dplyr::mutate(e8 = case_when(e8 == 0 ~ "No",
                                     e8 == 1 ~ "Yes",
                                     e8 == 8 ~ "Don't know")) %>%
  group_by(e8) %>%
  dplyr::summarise(n = n()) %>% # 38 phone call said yes there is eligible person
  mutate(freq = n/sum(n)*100)

# E12
# Can this person come to the phone right now, or is it better to organize a call-back?
# MW
dat.widemw %>%
  select(E12_1) %>%
  drop_na() %>%
  dplyr::mutate(E12_1 = case_when(E12_1 == 1 ~ "Now",
                                  E12_1 == 2 ~ "Call Back")) %>%
  group_by(E12_1) %>%
  dplyr::summarise(n = n())%>%
  mutate(freq = n/sum(n)*100)

# DRC
dat.wide %>%
  select(E12) %>%
  drop_na() %>%
  dplyr::mutate(E12 = case_when(E12 == 1 ~ "Now",
                                  E12 == 2 ~ "Call Back")) %>%
  group_by(E12) %>%
  dplyr::summarise(n = n())%>%
  mutate(freq = n/sum(n)*100)

# BF
rdd.dat.wide %>%
  select(e10) %>%
  drop_na() %>%
  dplyr::mutate(e10 = case_when(e10 == 1 ~ "Now",
                                e10 == 2 ~ "Call Back")) %>%
  group_by(e10) %>%
  dplyr::summarise(n = n())%>%
  mutate(freq = n/sum(n)*100)

# D5
# How upset were you from these questions?
Consentedmw %>%
  group_by(d5_1) %>%
  drop_na(d5_1) %>%
  mutate(d5_1 = case_when(d5_1 == 1 ~ "Slightly upset",
                          d5_1 == 2 ~ "Moderately upset",
                          d5_1 == 3 ~ "Very upset")) %>%
  dplyr::summarise(n = n())%>%
  mutate(freq = n/sum(n)*100)


# D4
# whether the questions I’ve asked during the interview upset you? 
Consentedmw %>% 
  group_by(d4_1) %>%
  mutate(d4_1 = case_when(d4_1 == 0 ~ "No",
                          d4_1 == 1 ~ "Yes")) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)



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
       width = 1.3]  
       
  Contact   [label = 'Contact'] 
  Match [label = 'Match\nLanguage'] 
  Mismatch  [label = 'Mismatch\nLanguage'] 
  Ineligibleg [label = 'Ineligible\n(geography)'] 
  Eligible [label = 'Eligible']
  Ineligibled [label = 'Ineligible\n(demography)']
  Reallocate [label = 'Reallocate']
  Deferral [label = 'Deferral']
  Consent [label = 'Consent']
  Referral [label = 'Referral']
  
  # edges

  #######
  Contact -> Match
  Mismatch -> Reallocate
  Match -> Ineligibleg
  Ineligibleg -> Deferral
  Match -> Eligible
  Eligible -> Consent
  Match -> Ineligibled
  Ineligibled -> Referral
  Contact -> Mismatch
}
")

################################################################################
## Interview assessment
################################################################################

# Technical difficulties
# MW
tech_diff <- Consentedmw %>%
  group_by(tech_difficulties) %>%
  drop_na(tech_difficulties) %>%
  dplyr::summarise(n=n()) %>%
  mutate(freq = n/sum(n)*100)

tech_diff_sex <- Consentedmw %>%
  filter(Resp.Sex=="Male") %>% # change 'Male' to 'Female'
  group_by(tech_difficulties) %>% # change RuralUrban to Resp.Sex to get sex
  drop_na(tech_difficulties) %>%
  dplyr::summarise(n=n()) %>%
  mutate(freq = n/sum(n)*100)

# DRC
DRC_MALE <- Consented %>%
  filter(RuralUrban=="Rural") 

Freq <- data.frame(Response=c("No Difficulties", "Background noise",
                              "Poor reception", "Mismatched languages", "Other"), 
                   Freq = c(sum(DRC_MALE$call_feedback_1, na.rm=T),
         sum(DRC_MALE$call_feedback_2, na.rm=T),
         sum(DRC_MALE$call_feedback_3, na.rm=T),
         sum(DRC_MALE$call_feedback_4, na.rm=T),
         sum(DRC_MALE$call_feedback_5, na.rm=T)))
Freq %>%
  mutate(per = Freq/sum(Freq)*100)

# Respondent cooperation
# MW
resp_cooperation <- Consentedmw %>%
  group_by(resp_coop) %>%
  drop_na(resp_coop) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100) 

resp_cooperation_sex <- Consentedmw %>%
  filter(Resp.Sex=="Male") %>%
  group_by(resp_coop) %>%
  drop_na(resp_coop) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100) 

# DRC
Consented %>%
  mutate(Respondent_cooperation = case_when(Respondent_cooperation == 1 ~ 'Good',
                                            Respondent_cooperation == 2 ~ 'hesitant',
                                            Respondent_cooperation == 3 ~ 'distracted',
                                            Respondent_cooperation == 4 ~ 'not cooperative')) %>%
  group_by(Respondent_cooperation) %>%
  drop_na(Respondent_cooperation) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100) 

Consented %>%
  filter(RuralUrban=="Rural") %>%
  mutate(Respondent_cooperation = case_when(Respondent_cooperation == 1 ~ 'Good',
                                            Respondent_cooperation == 2 ~ 'hesitant',
                                            Respondent_cooperation == 3 ~ 'distracted',
                                            Respondent_cooperation == 4 ~ 'not cooperative')) %>%
  group_by(Respondent_cooperation) %>%
  drop_na(Respondent_cooperation) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100) 

# How upset were you from these questions?
Consentedmw %>%
  group_by(d5_1) %>%
  drop_na(d5_1) %>%
  mutate(d5_1 = case_when(d5_1 == 1 ~ "Slightly upset",
                          d5_1 == 2 ~ "Moderately upset",
                          d5_1 == 3 ~ "Very upset")) %>%
  dplyr::summarise(n = n())%>%
  mutate(freq = n/sum(n)*100)

upset_sex <- Consentedmw %>%
  filter(Resp.Sex=="Male") %>%
  group_by(d5_1) %>%
  drop_na(d5_1) %>%
  mutate(d5_1 = case_when(d5_1 == 1 ~ "Slightly upset",
                          d5_1 == 2 ~ "Moderately upset",
                          d5_1 == 3 ~ "Very upset")) %>%
  dplyr::summarise(n = n())%>%
  mutate(freq = n/sum(n)*100)


datamw %>%
  group_by(call_status) %>%
  dplyr::summarise(n=n()) %>%
  mutate(freq = n/sum(n)*100)

# Where respondent is taking their call?
# MW
Consentedmw %>%
  group_by(call_location) %>%
  drop_na(call_location) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

Consentedmw %>%
  filter(RuralUrban=="Rural") %>%
  group_by(call_location) %>%
  drop_na(call_location) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

# DRC
Consented %>%
  group_by(Location_call) %>%
  drop_na(Location_call) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

Consented %>%
  filter(Resp.Sex=="Male") %>%
  group_by(Location_call) %>%
  drop_na(Location_call) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

# BF
# Info is only present in T1

rddt1 <- read_dta(paste0(dir.input, 'base_appel_rdd_wide_T1.dta'))
rddt1$i1
bf_call_place <- rddt1 %>%
  mutate(i1 = case_when(i1 == 1 ~ 'Home',
                   i1 == 2 ~ 'Workplace',
                   i1 == 3 ~ 'Public Place',
                   i1 == 4 ~ 'Public Place',
                   i1 == 5 ~ 'Other',
                   i1 == 6 ~ 'Other')) %>%
  group_by(i1) %>%
  drop_na(i1) %>%
  summarise(n=n()) %>%
  mutate(n/sum(n)*100)
# Are the respondents on the speaker?
# MW
Consentedmw %>%
  group_by(on_speaker) %>%
  drop_na(on_speaker) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

Consentedmw %>%
  filter(RuralUrban=="Rural") %>%
  group_by(on_speaker) %>%
  drop_na(on_speaker) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

# DRC
Consented %>%
  mutate(SpeakerPhone = case_when(SpeakerPhone == 0 ~ 'No',
                                  SpeakerPhone == 1 ~ 'Yes',
                                  SpeakerPhone == 3 ~ 'Refuse')) %>%
  group_by(SpeakerPhone) %>%
  drop_na(SpeakerPhone) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

Consented %>%
  filter(Resp.Sex=="Female") %>%
  mutate(SpeakerPhone = case_when(SpeakerPhone == 0 ~ 'No',
                                  SpeakerPhone == 1 ~ 'Yes',
                                  SpeakerPhone == 3 ~ 'Refuse')) %>%
  group_by(SpeakerPhone) %>%
  drop_na(SpeakerPhone) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)


# Phone ownership
# MW
Consentedmw %>%
  group_by(phone_ownership) %>%
  drop_na(phone_ownership) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

Consentedmw %>%
  filter(RuralUrban=="Rural") %>%
  group_by(phone_ownership) %>%
  drop_na(phone_ownership) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

# DRC
Consented %>%
  group_by(OwnPhone) %>%
  drop_na(OwnPhone) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

Consented %>%
  filter(Resp.Sex=="Male") %>%
  group_by(OwnPhone) %>%
  drop_na(OwnPhone) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100)

################################################################################
## Call outcome by time of day
################################################################################

# Convert from 'Jan 31, 2022 8:50:52 PM' format to '2022-01-31 08:37:31' format
# Then split it into seperate Date and Time variables
stuff <- Consentedmw %>%
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))

# Create df with just the info needed
time_mw<- stuff

# Creating the time categories
# create breaks
breaks <- hour(hm("00:00", "4:00", "9:00",  "13:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night","Early Morning",  "Late Morning", "Afternoon", "Evening")
# Creating new column with the time categories
time_mw$Time_of_day <- cut(x=hour(time_mw$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
time_mw

# create df grouped by Time_of_day
time_mw_comp <- time_mw %>%
  dplyr::group_by(Time_of_day, Outcome2) %>%
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome2 == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'MW') 

time_mw %>%
  select(Time, Time_of_day) %>%
  View()

# get the number of calls from datamw dataset
datamw<- datamw %>% 
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))
datamw$Time_of_day <- cut(x=hour(datamw$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
time_mw_comp$Calls <- c(table(datamw$Time_of_day))

# datamw %>%
#   group_by(Time_of_day) %>%
#   summarise(n=n())

# create df to order the bars
days_ordered <- c("Early Morning", "Late Morning", "Afternoon", "Evening", "Night")

# change Time_of_day from factor to character format
time_mw_comp$Time_of_day <- as.character(time_mw_comp$Time_of_day)

# create plot
time_mw_plot <- ggplot(data = subset(time_mw_comp)) +
  geom_bar(aes(x=Time_of_day, y=Calls, fill=Time_of_day),stat = 'identity') +
  scale_x_discrete(limits = days_ordered) +
  labs(x="Time", y="Number of calls", fill='Time') +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 75000)) +
  scale_fill_brewer(palette = "Paired") 
time_mw_plot

# DRC
time_sort_drc <- Consented %>%
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))

time_drc <- time_sort_drc

# Creating new column with the time categories
time_drc$Time_of_day <- cut(x=hour(time_drc$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
time_drc

# create df grouped by Time_of_day
time_drc_comp <- time_drc %>%
  dplyr::group_by(Time_of_day, Outcome2) %>%
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome2 == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'DRC') 

# get the number of calls from datamw dataset
datadrc<- data %>% 
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))
datadrc$Time_of_day <- cut(x=hour(datadrc$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
time_drc_comp$Calls <- c(table(datadrc$Time_of_day))

str(time_drc_comp)
# change Time_of_day from factor to character format
time_drc_comp$Time_of_day <- as.character(time_drc_comp$Time_of_day)

# create plot
time_drc_plot <- ggplot(data = time_drc_comp) +
  geom_bar(aes(x=Time_of_day, y= Calls,fill=Time_of_day),stat = 'identity') +
  scale_x_discrete(limits = days_ordered) +
  labs(x="Time", y="Number of calls") +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 75000)) +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position="none")
time_drc_plot



# BF
#Split date and time into different columns
Consentedrdd$Date <- as.Date(Consentedrdd$instance_time) 
Consentedrdd$Time <- format(as.POSIXct(Consentedrdd$instance_time), format = "%H:%M:%S") 

time_bf <- Consentedrdd

# Creating new column with the time categories
time_bf$Time_of_day <- cut(x=hour(time_bf$instance_time), breaks = breaks, labels = labels, include.lowest=TRUE)


# create df grouped by Time_of_day
time_bf_comp <- time_bf %>%
  dplyr::group_by(Time_of_day, Outcome) %>%
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'BF') 

# get the number of calls from datamw dataset
databf<- cut(x=hour(bf.data$instance_time), breaks = breaks, labels = labels, include.lowest=TRUE)
time_bf_comp$Calls <- c(table(databf))

str(time_bf_comp)
# change Time_of_day from factor to character format
time_bf_comp$Time_of_day <- as.character(time_bf_comp$Time_of_day)

# create plot
time_bf_plot <- ggplot(data = time_bf_comp) +
  geom_bar(aes(x=Time_of_day, y=Calls, fill=Time_of_day),stat = 'identity') +
  scale_x_discrete(limits = days_ordered) +
  labs(x="Time", y="Number of calls") +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 75000)) +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position="none")
time_bf_plot

# arrange the three plots into 4 quadrants
plots_time <- plot_grid(time_mw_plot + theme(legend.position="none", axis.text=element_text(size=12)), #remove legends first before combining legends
                time_drc_plot+ theme(legend.position="none", axis.text=element_text(size=12)),
                time_bf_plot + theme(legend.position="none", axis.text=element_text(size=12)), 
                labels = c("MW", "DRC", "BF"), #Title for each graph
                label_size = 10
)

# extract the legend from Malawi plots
legend_time_mw_plot <- get_legend(
  time_mw_plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 5),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15))
)

# Adding subtitle with the eligibility unknown outcomes
sublegend_time_mw_plot<- add_sub(plots_time, "Early Morning = 5:00 - 8:59 \n Late Morning = 9:00 - 11:59 \n Afternoon = 12:00 - 18:59 \n Evening = 19:00 - 23:59 \n Night = 00:00 - 4:59", x = 0.85, y=3, size=11)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(sublegend_time_mw_plot, legend_time_mw_plot, ncol = 1, rel_heights = c(1, .1))

# Based on sex and rural vs urban
# MW

# create df grouped by Time_of_day
time_mw_sex <- time_mw %>%
  dplyr::group_by(Time_of_day, Outcome2, Resp.Sex, .drop = FALSE) %>%
  dplyr::summarize(n = n()) 

# new_df <- data.frame(Time_of_day  = 'Night',
#                      Outcome2 = 'COMP',
#                      Resp.Sex = 'Male',
#                      n = 0)
# 
# time_mw_sex <- rbind(time_mw_sex, new_df)

time_mw_sex %>%
  filter(Outcome2 == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'MW')

# get the number of calls from datamw dataset
datamw_sex <- datamw %>% 
  select(Resp.Sex, starttime)  %>%
  drop_na(Resp.Sex, starttime) %>%
  dplyr::group_by(Resp.Sex) %>%
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))
datamw_sex$Time_of_day <- cut(x=hour(datamw_sex$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
time_mw_sex$Calls <- c(table(datamw_sex$Time_of_day, datamw_sex$Resp.Sex))

yay <- datamw_sex %>%
  group_by(Resp.Sex, Time_of_day)  %>%
  summarise(n=n())

mw_time_gender <- left_join(time_mw_sex, yay)

# hey <- datamw_sex %>%
#   group_by(Resp.Sex, Time_of_day)  %>%
#   summarise(n=n())
# 
# left_join(time_mw_sex, hey)

calls.time.sex.mw <- ggplot(data = mw_time_gender) +
  geom_bar(aes(x=Time_of_day, y=Calls, fill=Resp.Sex ),stat = 'identity', position = 'dodge') +
  scale_x_discrete(limits = days_ordered) +
  labs(x="Time", y="Number of calls", fill='Respondent Sex') +
  theme_bw() + 
  scale_fill_brewer(palette = "Paired") 
calls.time.sex.mw

# DRC
# create df grouped by Time_of_day
time_drc_sex <- time_drc %>%
  dplyr::group_by(Time_of_day, Outcome2, Resp.Sex) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::mutate(
                country = 'DRC') 

new_df <- data.frame(Time_of_day  = 'Night',
                     Outcome2 = 'COMP',
                     Resp.Sex = 'Male',
                     n = 0,
                     country = 'DRC')

time_drc_sex <- rbind(time_drc_sex, new_df)

# get the number of calls from datamw dataset
hey <- datadrc %>%
  group_by(Resp.Sex, Time_of_day) %>%
  drop_na(Resp.Sex) %>%
  summarise(Calls=n())

time_drc_sex <- left_join(time_drc_sex, hey)
ggplot(data = time_drc_sex) +
  geom_bar(aes(x=Time_of_day, y=Calls, fill=Resp.Sex ),stat = 'identity', position = 'dodge') +
  scale_x_discrete(limits = days_ordered) +
  labs(x="Time", y="Number of calls", fill='Respondent Sex') +
  theme_bw() + 
  scale_fill_brewer(palette = "Paired") 

# BF

# create df grouped by Time_of_day
time_bf_sex <- time_bf %>%
  dplyr::group_by(Time_of_day, Outcome, gender) %>%
  filter(Outcome == 'COMP')  %>%
  dplyr::summarize(n = n()) %>%
  dplyr::mutate(country = 'BF') 

# get the number of calls from datamw dataset


idk <- bf.data %>% 
  mutate(starttime = instance_time)
idk$Time_of_day <- cut(x=hour(idk$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)


why <- bf.data %>% 
  select(gender, instance_time)  %>%
  drop_na(gender, instance_time) %>%
  dplyr::group_by(gender) %>%
  mutate(starttime = instance_time,
         gender = case_when(gender == 1 ~ 'Male',
                            gender == 2 ~ 'Female'))
why$Time_of_day <- cut(x=hour(why$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
time_bf_sex$Calls <- c(table(why$Time_of_day, why$gender))

why %>%
  group_by(gender, Time_of_day) %>%
  summarise(n=n())

great <- why %>%
  group_by(gender, Time_of_day)  %>%
  summarise(Calls=n())

bf_time_gender <- left_join(time_bf_sex, great)

ggplot(data = bf_time_gender) +
  geom_bar(aes(x=Time_of_day, y=Calls, fill=gender ),stat = 'identity', position = 'dodge') +
  scale_x_discrete(limits = days_ordered) +
  labs(x="Time", y="Number of calls", fill='Respondent Sex') +
  theme_bw() + 
  scale_fill_brewer(palette = "Paired") 

################################################################################
## Urban vs rural 

# DRC
# create df grouped by Time_of_day
time_drc_urban <- time_drc %>%
  dplyr::group_by(Time_of_day, Outcome2, RuralUrban) %>%
  drop_na(RuralUrban)%>%
  dplyr::summarize(n = n()) %>%
  dplyr::mutate(
    country = 'DRC') 

# get the number of calls from datamw dataset
calls_rural <- datadrc %>%
  group_by(RuralUrban, Time_of_day) %>%
  drop_na(RuralUrban) %>%
  summarise(Calls=n())

time_drc_urbanrural <- left_join(time_drc_urban, calls_rural)
ggplot(data = time_drc_urbanrural) +
  geom_bar(aes(x=Time_of_day, y=Calls, fill=RuralUrban ),stat = 'identity', position = 'dodge') +
  scale_x_discrete(limits = days_ordered) +
  labs(x="Time", y="Number of calls", fill='Place of residence') +
  theme_bw() + 
  scale_fill_brewer(palette = "Paired") 


################################################################################
## Days of the week
################################################################################

datamw %>%
  # mutate(L2_1 = case_when(L2_1 == 1 ~ 'No preference',
  #                         L2_1 == 2 ~ 'Monday',
  #                         L2_1 == 3 ~ 'Tuesday',
  #                         L2_1 == 4 ~ 'Wednesday',
  #                         L2_1 == 5 ~ 'Thursday',
  #                         L2_1 == 6 ~ 'Friday',
  #                         L2_1 == 7 ~ 'Saturday',
  #                         L2_1 == 8 ~ 'Sunday',
  #                         L2_1 == 99 ~ 'Refuse Callback')) %>%
  group_by(l7_1) %>%
  dplyr::summarise(n=n())
  

  # dplyr::group_by(, Outcome.FINAL) %>%
  # dplyr::summarize(n = n()) %>%
  # ungroup() %>%
  # filter(Outcome.FINAL == 'COMP') %>%
  # dplyr::mutate(totalcomp = sum(n),
  #               totalnums = nrow(dat.widemw),
  #               perc = round(n/totalnums*100,2),
  #               perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  # ungroup() %>%
  # dplyr::mutate(perccum = cumsum(perc),
  #               country = 'MW') 
  # 
  # 

############
# Time preference
dat.widemw %>%
  group_by(i4_1) %>%
  summarise(n=n())

# DRC
dat.wide %>%
  group_by(L3) %>%
  summarise(n=n())

mwdata_raw %>%
  group_by( call_status) %>%
  filter(call_status == '17') %>%
  summarise(n=n())  %>%
  View()

################################################################################
## Referrals

# get the caseid and Outcomes that are referrals
datamw %>%
  group_by(caseid,Outcome2, E10_1) %>%
  filter(Outcome2 == 'REFER') %>%
  dplyr::summarise(n=n()) %>%
  View()
# caseid = 10764
# find the final CATI outcomes of the referrals
dat.widemw %>%
  filter(caseid == 10764)  %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(n=n())

# Call E10 answers
datamw %>%
  group_by(E10_1) %>%
  dplyr::summarise(n=n()) %>%
  View()
# Call outcome by enumerator
datamw %>%
  group_by(call_status) %>%
  filter(call_status == '9') %>%
  dplyr::summarise(n=n())  %>%
  View()

# Find out whether all the caseids in E10 are in Outcome2=Refer
calloutcome_id <- datamw %>%
  filter(Outcome2 == 'REFER') %>%
  distinct(caseid) %>%
  select(caseid)

E10_id <- datamw %>%
  filter(E10_1 == 1) %>%
  distinct(caseid) %>%
  select(caseid) 

E10_id$caseid %in% calloutcome_id$caseid

# Check whether the caseid of E10 is present in the Referral outcome caseid.
# Then counting the number of True cases.
# Differences in caseid could be due to different numbers being called, resulting
# in new caseid.
E10_id %>%
  mutate(present = caseid %in% calloutcome_id$caseid) %>%
  dplyr::summarize(present_sum = sum(present))

# Check the call outcomes of E10 yes
# ie there is an eligible person in the household
datamw %>%
  filter(E10_1 == 1) %>%
  group_by(Outcome2) %>%
  dplyr::summarise(n=n())

# get the caseid of the E10 yes which have call outcome as refer
e10_refer<- datamw %>%
  filter(E10_1 == 1) %>%
  group_by(Outcome2, caseid) %>%
  dplyr::summarise(n=n())
# get just when the call outcome is refer
e10_refer_caseid <- e10_refer %>%
  filter(Outcome2 == 'REFER')
# Check the final CATI outcomes
dat.widemw %>%
  filter(caseid %in% unique(e10_refer_caseid$caseid)) %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(n=n())

# Check the final CATI outcomes of the referral call outcomes
dat.widemw %>%
  filter(caseid %in% unique(calloutcome_id$caseid)) %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(n=n())
# Check final CATI outcomes of 92 E10 calls
dat.widemw %>%
  filter(caseid %in% unique(E10_id$caseid)) %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(n=n())


# Check E10 no eligible person
unique(datamw$E10_1)
E10_id_no <- datamw %>%
  filter(E10_1 == 2) %>%
  distinct(caseid) %>%
  select(caseid) 
# Check the final CATI outcomes of the no referral call outcomes
dat.widemw %>%
  filter(caseid %in% unique(E10_id_no$caseid)) %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(n=n())

datamw %>%
  filter(E10_1 == 2) %>%
  dplyr::summarise(n=n())

mwdata_raw %>%
  select(E14_1) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  group_by(E14_1) %>%
  dplyr::summarise(n=n())

#################
# Refusals
datamw %>%
  filter(Outcome2 == 'REFU') %>%
  group_by(C1_1) %>%
  dplyr::summarise(n=n()) 

datamw %>%
  group_by(c2_1) %>%
  dplyr::summarise(n=n()) 

# Those that refused to give name should be classed as refusal
# but caseids 16570 and 800 are not
datamw %>%
  filter(!is.na(L4_1)) %>%
  group_by(caseid, L4_1) %>%
  dplyr::summarise(n=n()) %>%
  View()
datamw %>%
  filter(caseid == 800) %>%
  group_by(call_num, Outcome2, L4_1) %>%
  dplyr::summarise(n=n())

datamw %>%
  group_by(caseid, Outcome2, call_num) %>%
  filter(Outcome2 == 'REFU') %>%
  filter(call_num == 6) %>%
  dplyr::summarise(n=n()) %>%
  View() 

a<-datamw %>%
  filter(Outcome2 == 'REFU') %>%
  select(caseid, call_num, Outcome2) 
  summarise(count = n_distinct(caseid))
 
# get list of repeated caseids
refu.repeat.id <- datamw %>%
  filter(Outcome2 == 'REFU') %>%
  select(caseid, call_num, Outcome2) %>%
  group_by(caseid) %>%
  filter(n()>1) 
# find number of repreated caseids
unique(refu.repeat.id$caseid)


# start from 'REFU' calloutcome
datamw %>%
  filter(Outcome2 == 'REFU') %>%
  group_by(c2_1)  %>%
  dplyr::summarise(n=n())

# Look through comments of enuerators to see whether calls hung up
comments_mw <- unique(datamw$comments)
grep("hung", comments_mw, value = TRUE) 

# find number of Reassigned incomplete cases witha refusal for the initial language question to refuse
datamw %>%
  filter(call_status_label == "Incomplete (callback)" & Resp.Language =="Refuse")%>%
  dplyr::summarise(n=n())

# find Reassign Truly ineligible cases which have not been marked as such appropriately  
datamw %>%
  filter(call_status == 111 & !is.na(E10_1))%>%
  dplyr::summarise(n=n())

# Check whether the caseid of E10 is present in the Referral outcome caseid.
# Then counting the number of True cases.
# Differences in caseid could be due to different numbers being called, resulting
# in new caseid.
# E10_id %>%
#   mutate(present = caseid %in% calloutcome_id$caseid) %>%
#   dplyr::summarize(present_sum = sum(present))

inel.e10 <- datamw %>%
  filter(Outcome2 == 'INEL' & E10_1 == 2)  %>%
  select(caseid)

inel.e11 <- datamw %>%
  filter(Outcome2 == 'INEL' & E11_1 == 0)  %>%
  select(caseid)

inel.caseid.repeat<-datamw %>%
  filter(Outcome2 == 'INEL' & E11_1 == 0 & E10_1 == 2)  %>%
  select(caseid) 


result3 <- subset(inel.e10, caseid %in% inel.e11$caseid)
result3

# Identify refusals base don 
datamw %>%
  # dplyr::select(Outcome2, i1_1:i4_1) %>%
  filter(Outcome2 == 'REFU') %>%
  dplyr::select(caseid, Outcome2, i1_1, i1_oth_1, i2_1, i3_1, i4_1, i5_1, i6_1, i7_1, b1_1) %>%
  drop_na(i6_1) 

datamw %>%
  filter(Outcome2 == 'REFU') %>%
  dplyr::select(caseid,Outcome2, b1_1:b8b_1) %>%
  drop_na(b1_1) 

unique(datamw$b8b_1)

datamw %>%
  filter(caseid == 14457) %>%
  group_by(call_num, Outcome2, hd9_1, ssh1a_1, ph1_1) %>%
  summarise(n=n())

datamw %>%
  filter(ph1_1==0) %>%
  group_by(caseid, Outcome2, ph1_1) %>%
  summarise(n=n()) %>%
  View()
################
# Ineligible
datamw %>%
  group_by(Outcome2) %>%
  dplyr::summarise(n=n()) %>%
  View()

datamw %>%
  group_by(E14_1) %>%
  dplyr::summarise(n=n()) 


# start from 'INEL' calloutcome
datamw %>%
  filter(Outcome2 == 'INEL') %>%
  filter(!is.na(E10_1))  %>%
  filter(!is.na(E11_1))%>%
  group_by(caseid, E10_1, E11_1) %>%
  dplyr::summarise(n=n()) %>%
  View()

mwdata_raw %>%
  group_by(E10_1) %>%
  dplyr::summarise(n=n()) 

datamw %>%
  select(phone_1, caseid, Outcome2) %>%
  View()

datamw %>%
  group_by(E14_1) %>%
  dplyr::summarise(n=n()) %>%
  View()

# get E9
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

# Get E9 for only INEL cases
inel_cases <- datamw %>%
  filter(Outcome2 == 'INEL')
E9_inel <- data.frame(X = c(inel_cases$e9_1_1, 
                           inel_cases$e9_2_1,
                           inel_cases$e9_3_1,
                           inel_cases$e9_4_1,
                           inel_cases$e9_5_1,
                           inel_cases$e9_6_1,
                           inel_cases$e9_7_1,
                           inel_cases$e9_8_1,
                           inel_cases$e9_9_1,
                           inel_cases$e9_10_1,
                           inel_cases$e9_11_1,
                           inel_cases$e9_12_1,
                           inel_cases$e9_13_1,
                           inel_cases$e9_14_1,
                           inel_cases$e9_15_1,
                           inel_cases$e9_16_1,
                           inel_cases$e9_17_1,
                           inel_cases$e9_18_1,
                           inel_cases$e9_19_1,
                           inel_cases$e9_20_1,
                           inel_cases$e9_21_1,
                           inel_cases$e9_22_1,
                           inel_cases$e9_23_1,
                           inel_cases$e9_24_1))
E9_inel %>%
  group_by(X) %>%
  dplyr::summarise(n=n())


dat1mw <- data.frame(call_num=datamw$call_num,
           phone_1 = datamw$phone_1,
           caseid = datamw$caseid,
           inel.age = datamw$E10_1,
           call_num_2 = data_clean3$call_num)

look <- pivot_wider(dat1mw, names_from = call_num,  #values_fn = list,#ifelse(Outcome=='COMP'),
                          values_from = c('inel.age', 'call_num_2'))








