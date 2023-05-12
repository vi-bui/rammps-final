#_______________________________________________________________________________
#  Response rates
#  This script contains information on response rates and CATI/Call outcomes
#  05/2023
#_______________________________________________________________________________

# Load Libraries ---------------------------------------------------------------
pkgs <- c('purrr','plyr','tidyverse', 'plotrix','lubridate','kableExtra','hrbrthemes','ggplot2','extrafont','float','reshape',
          'gridExtra','rsvg','png','devtools','readxl','date', 'ggpubr', 'tidyselect', 'httr', 'jsonlite', 'extrafont', 'colorspace',
          'ggrepel', 'forcats', 'ggpubr', 'readstata13', 'scales', 'cowplot')
lapply(pkgs, require, character.only = TRUE)

# Read in data -----------------------------------------------------------------

# Setting directories
r.functions.dir <- '/Users/lshvb5/Documents/rammps/'
# r.functions.dir <- 'C:/Users/Kelly McCain/Documents/LSHTM/rammps/'
dir.inputdata <- ('/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/UNIKIN/SurveyCTO Audits/')
# dir.inputdata <- "/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/UNIKIN/SurveyCTO Audits

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
bf.dat.wide <- listbf[[4]]
bf.data <- listbf[[3]]
Consentedrdd <- rdd.data %>%
  filter(Outcome == 'COMP') %>%
  dplyr::mutate(month.interview1 = lubridate::floor_date(as.Date(Date.Interview), 'month'),
                gender = case_when(is.na(gender) ~ 'Male', TRUE ~ as.character(gender)),
                Resp.Age.pyr = cut(as.numeric(age), c(14,19,29,39,49,59,64)))                                         
Consentedbf <- bf.data %>%
  filter(Outcome == 'COMP')

# Response Rate Calculations ---------------------------------------------------
# DRC 
ferkin <- dat.wide %>%
  filter(Source.prov.1 == 'Feroxus-Kin') %>%
  group_by(Outcome.FINAL) %>%
  tally()

fernk <- dat.wide %>%
  filter(Source.prov_1 == 'Feroxus-NK')%>%
  group_by(Outcome.FINAL) %>%
  tally()
ivrkin <- dat.wide %>%
  filter(Source.prov.1 == 'IVR group 1-Kin')%>%
  group_by(Outcome.FINAL) %>%
  tally()
ivrnk <- dat.wide %>%
  filter(Source.prov.1 == 'IVR group 1-NK')%>%
  group_by(Outcome.FINAL) %>%
  tally()
ivr <- dat.wide %>%
  filter(Source.prov.1=='IVR group 1-NK'|Source.prov.1=='IVR group 1-Kin'|Source.prov.1=='IVR group 2') %>%
  group_by(Outcome.FINAL) %>%
  tally()
fer <- dat.wide %>%
  filter(Source.prov.1=='Feroxus-Kin'|Source.prov.1=='Feroxus-NK')%>%
  group_by(Outcome.FINAL) %>%
  tally()
alldrc <- dat.wide%>%
  group_by(Outcome.FINAL) %>%
  tally()

# Malawi
allmw <- dat.widemw %>%
  group_by(Outcome.FINAL) %>%
  tally()

# BF
rddbf <- rdd.dat.wide %>%
  group_by(Outcome.FINAL) %>%
  tally()
ehcvmbf <- ehcvm.dat.wide %>%
  group_by(Outcome.FINAL) %>%
  tally()
allbf <- bf.dat.wide %>%
  group_by(Outcome.FINAL) %>%
  tally()


dfs <- list('alldrc'=alldrc, 'allmw' = allmw, 'rddbf' = rddbf)
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

# Work out total CATIs completed -----------------------------------------------
CATIs_drc <- rbind(
  Consented %>%
    group_by(Source.prov) %>%
    dplyr::summarise(catis = n())  %>%
    dplyr::rename(group = Source.prov) ,
  Consented %>% 
    group_by(source) %>%
    dplyr::summarise(catis = n()) %>%
    dplyr::rename(group = source),
  Consented %>%
    dplyr::summarise(catis=n()) %>%
    mutate(group = 'drc')
)

mwcatis <- nrow(Consentedmw) # total MW completed 
mwcatis

rddbfcatis <- nrow(rdd.dat.wide[rdd.dat.wide$Outcome.FINAL=='COMP',])
rddbfcatis

ehcvmbfcatis <- nrow(ehcvm.dat.wide[ehcvm.dat.wide$Outcome.FINAL=='COMP',])
ehcvmbfcatis

bfcatis <- nrow(bf.dat.wide[bf.dat.wide$Outcome.FINAL=='COMP',])
bfcatis

# calls placed + per completed
drccalls <- rbind(data %>%
                    group_by(Source.prov) %>%
                    dplyr::summarize(calls=n()) %>%
                    dplyr::rename(group = Source.prov) ,
                  data %>%
                    group_by(source) %>%
                    dplyr::summarize(calls=n()) %>%
                    dplyr::rename(group = source) ,
                  data %>% 
                    dplyr::summarize(calls=n()) %>%
                    mutate(group = 'drc') 
)

mwcalls <- datamw %>%
  dplyr::summarise(calls = n()) %>%
  mutate(group = 'MW')

rddcalls <- rdd.data %>%
  dplyr::summarise(calls = n()) %>%
  mutate(group = 'RDD BF')
ehcvmcalls <- ehcvm.data %>%
  dplyr::summarise(calls = n()) %>%
  mutate(group = 'EHCVM BF')
bfcalls <- bf.data %>%
  dplyr::summarise(calls = n()) %>%
  mutate(group = 'BF')

calls <- rbind(drccalls, mwcalls, rddcalls, ehcvmcalls, bfcalls)
calls

# Call attempts per completed CATI
drc <- left_join(drccalls, CATIs_drc) %>%
  mutate(attperCATI = calls/catis)
drc
mw <- mwcalls$calls/mwcatis
mw

rddatt <- rddcalls$calls/rddbfcatis
rddatt
ehcvmatt <- ehcvmcalls$calls/ehcvmbfcatis
ehcvmatt
bfatt <- bfcalls$calls/bfcatis
bfatt

# numbers w/ completed cati (CATIs/total #s per group)
drcnums <- rbind(dat.wide %>%
                   group_by(Source.prov.1) %>%
                   dplyr::summarize(nums = n()) %>%
                   dplyr::rename(group = Source.prov.1),
                 dat.wide %>%
                   group_by(source) %>%
                   dplyr::summarize(nums =n()) %>%
                   dplyr::rename(group = source) ,
                 dat.wide %>% ungroup() %>%
                   dplyr::summarize(nums = n()) %>%
                   mutate(group = 'drc'))

drcperc <- left_join(drc, drcnums) %>%
  mutate(percnumswCATI = catis/nums * 100)
drcperc

mwperc <- mwcatis/nrow(dat.widemw)*100#[dat.widemw$Outcome.FINAL=='COMP',])
mwperc

rddperc <- rddbfcatis/nrow(rdd.dat.wide)*100
ehcvmperc <- ehcvmbfcatis/nrow(ehcvm.dat.wide)*100
bfperc <- bfcatis/nrow(bf.dat.wide)*100
rddperc
ehcvmperc
bfperc

# CATI Completion rates by call order plot -------------------------------------

#DRC
outcomesdrc <- dat.wide %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
  dplyr::group_by(call_num_grp, Outcome.FINAL) %>%
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome.FINAL == 'COMP') %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'DRC') 

# MW
outcomesmw <- dat.widemw %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
  dplyr::group_by(call_num_grp, Outcome.FINAL) %>%
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
  dplyr::group_by(call_num_grp, Outcome.FINAL) %>%
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome.FINAL == 'COMP') %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(rdd.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'BF-RDD') 


# Combined df
data <- data %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num)))
datamw <- datamw %>% 
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num)))
rdd.data <- rdd.data %>% 
  mutate(call_num_grp = ifelse(nbr_appel > 5, '5+',as.character(nbr_appel)))

# outcomesdrc_join$Calls <- c(table(data$call_num_grp))
outcomesdrc$Calls <- c(table(data$call_num_grp))
outcomesmw$Calls <- c(table(datamw$call_num_grp))
outcomesrdd$Calls <- c(table(rdd.data$call_num_grp))

# Outcomes for rdd
outcomes_rdd <- rbind(outcomesdrc, outcomesmw, outcomesrdd) %>%
  mutate(country = forcats::fct_inorder(country))

# Call attempt order plots -----------------------------------------------------

##Separated by country

# Create ratio for second y-axis
ratio_cati <- max(outcomes_rdd$Calls)/max(outcomes_rdd$perccompletedpercallnum)

# DRC
phone_call_cati_drc <- ggplot(data = subset(outcomes_rdd, country == 'DRC')) + 
  geom_line(aes(x = call_num_grp, y = perccompletedpercallnum*ratio_cati, group=ordered(country, levels = 'DRC'), color = '#D95F02'), size = 0.8) + 
  geom_point(aes(x = call_num_grp, y =  perccompletedpercallnum*ratio_cati, group=ordered(country, levels = 'DRC'),color = '#D95F02'), size = 2) +
  geom_bar(aes(y = Calls, x = call_num_grp, group=ordered(country, levels = c('DRC')), fill = '#1B9E77'), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  scale_fill_manual(values = c('#1B9E77','#1B9E77')) +
  scale_color_manual(values = c('#D95F02','#D95F02')) +
  theme(axis.title.y = element_text(color = '#D95F02', size=12),
        axis.title.y.right = element_text(color = "#1B9E77", size=12),
        legend.position = 'none')+
  scale_y_continuous(
    name = "% of completed CATIs per call num",
    limits = c(0, 50000),
    sec.axis = sec_axis((~ . / ratio_cati), name ='Phone calls made'))
phone_call_cati_drc 

# MW
phone_call_cati_mw <- ggplot(data = subset(outcomes_rdd, country == 'MW')) + 
  geom_line(aes(x = call_num_grp, y = perccompletedpercallnum*ratio_cati, group=ordered(country, levels = 'MW'), color = '#D95F02'), size = 0.8) + 
  geom_point(aes(x = call_num_grp, y =  perccompletedpercallnum*ratio_cati, group=ordered(country, levels = 'MW'),color = '#D95F02'), size = 2) +
  geom_bar(aes(y = Calls, x = call_num_grp, group=ordered(country, levels = c('MW')), fill = '#1B9E77'), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  scale_fill_manual(values = c('#1B9E77','#1B9E77')) +
  scale_color_manual(values = c('#D95F02','#D95F02')) +
  theme(axis.title.y = element_text(color = '#D95F02', size=12),
        axis.title.y.right = element_text(color = "#1B9E77", size=12),
        legend.position = 'none')+
  scale_y_continuous(
    name = "% of completed CATIs per call num",
    limits = c(0, 50000),
    sec.axis = sec_axis((~ . / ratio_cati), name ='Phone calls made'))
phone_call_cati_mw 

# BF
phone_call_cati_bf <- ggplot(data = subset(outcomes_rdd, country == 'BF-RDD')) + 
  geom_line(aes(x = call_num_grp, y = perccompletedpercallnum*ratio_cati, group=ordered(country, levels = 'BF-RDD'), color = '#D95F02'), size = 0.8) + 
  geom_point(aes(x = call_num_grp, y =  perccompletedpercallnum*ratio_cati, group=ordered(country, levels = 'BF-RDD'),color = '#D95F02'), size = 2) +
  geom_bar(aes(y = Calls, x = call_num_grp, group=ordered(country, levels = c('BF-RDD')), fill = '#1B9E77'), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  scale_fill_manual(values = c('#1B9E77','#1B9E77')) +
  scale_color_manual(values = c('#D95F02','#D95F02')) +
  theme(axis.title.y = element_text(color = '#D95F02', size=12),
        axis.title.y.right = element_text(color = "#1B9E77", size=12),
        legend.position = 'none')+
  scale_y_continuous(
    name = "% of completed CATIs per call num",
    limits = c(0, 50000),
    sec.axis = sec_axis((~ . / ratio_cati), name ='Phone calls made'))
phone_call_cati_bf

# Combine graphs from each country
phone_call_cati_final <- plot_grid(phone_call_cati_mw, # keep the MW axis titles and ticks
                                   phone_call_cati_drc + theme(
                                     axis.ticks.y = element_blank(), # removes the x-axis scales
                                     axis.title.y = element_blank(), # removes the x-axis title
                                     axis.ticks.y.right = element_blank(), # removes the right x-axis scales
                                     axis.title.y.right = element_blank()), # removes the right x-axis title
                                   phone_call_cati_bf + theme(
                                     axis.ticks.y = element_blank(), 
                                     axis.title.y = element_blank(),
                                     axis.ticks.y.right = element_blank(),
                                     axis.title.y.right = element_blank()),
                                   labels = c("MW", "DRC", "BF"), #Title for each graph
                                   label_size = 10,
                                   hjust = -0.2, vjust = 1.5)
plot_grid(phone_call_cati_final,  ncol = 1, rel_heights = c(1, .1))

# Number of phone calls vs call attempt order ----------------------------------

# Ratio for second y-axis
ratio <- max(outcomes_rdd$Calls)/max(outcomes_rdd$perccum)

# DRC
cumulative_drc <- ggplot(data = subset(outcomes_rdd, country == 'DRC')) + 
  geom_line(aes(x = call_num_grp, y = perccum*ratio, group=ordered(country, levels = 'DRC'), color = '#D95F02'), size = 0.8) + 
  geom_point(aes(x = call_num_grp, y =  perccum*ratio, group=ordered(country, levels = 'DRC'),color = '#D95F02'), size = 2) +
  geom_bar(aes(y = Calls, x = call_num_grp, group=ordered(country, levels = c('DRC')), fill = '#1B9E77'), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  scale_fill_manual(values = c('#1B9E77','#1B9E77')) +
  scale_color_manual(values = c('#D95F02','#D95F02')) +
  theme(axis.title.y = element_text(color = "#1B9E77", size=12),
        axis.title.y.right = element_text(color = "#D95F02", size=12),
        legend.position = 'none')+
  scale_y_continuous(
    name = "Number of Phone Calls",
    limits = c(0, 50000),
    sec.axis = sec_axis((~ . / ratio), name ='Cumulative percentage of phone #s\n with completed CATI'))
cumulative_drc 

# MW
cumulative_mw <- ggplot(data = subset(outcomes_rdd, country == 'MW')) + 
  geom_line(aes(x = call_num_grp, y = perccum*ratio, group=ordered(country, levels = 'MW'), color = '#D95F02'), size = 0.8) + 
  geom_point(aes(x = call_num_grp, y =  perccum*ratio, group=ordered(country, levels = 'MW'),color = '#D95F02'), size = 2) +
  geom_bar(aes(y = Calls, x = call_num_grp, group=ordered(country, levels = c('MW')), fill = '#1B9E77'), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  scale_fill_manual(values = c('#1B9E77','#1B9E77')) +
  scale_color_manual(values = c('#D95F02','#D95F02')) +
  theme(axis.title.y = element_text(color = "#1B9E77", size=12),
        axis.title.y.right = element_text(color = "#D95F02", size=12),
        legend.position = 'none')+
  scale_y_continuous(
    name = "Number of Phone Calls",
    limits = c(0, 50000),
    sec.axis = sec_axis((~ . / ratio), name ='Cumulative percentage of phone #s\n with completed CATI'))
cumulative_mw 

# BF
cumulative_bf <- ggplot(data = subset(outcomes_rdd, country == 'BF-RDD')) + 
  geom_line(aes(x = call_num_grp, y = perccum*ratio, group=ordered(country, levels = 'BF-RDD'), color = '#D95F02'), size = 0.8) + 
  geom_point(aes(x = call_num_grp, y =  perccum*ratio, group=ordered(country, levels = 'BF-RDD'),color = '#D95F02'), size = 2) +
  geom_bar(aes(y = Calls, x = call_num_grp, group=ordered(country, levels = c('BF-RDD')), fill = '#1B9E77'), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  scale_fill_manual(values = c('#1B9E77','#1B9E77')) +
  scale_color_manual(values = c('#D95F02','#D95F02')) +
  theme(axis.title.y = element_text(color = "#1B9E77", size=12),
        axis.title.y.right = element_text(color = "#D95F02", size=12),
        legend.position = 'none')+
  scale_y_continuous(
    name = "Number of Phone Calls",
    limits = c(0, 50000),
    sec.axis = sec_axis((~ . / ratio), name ='Cumulative percentage of phone #s\n with completed CATI'))
cumulative_bf 

# Combine the plots into a panel
# Arrange the three plots into 4 quadrants
# Then plot the figures
cumulative_final <- plot_grid(cumulative_mw, # keep the MW axis titles and ticks
                              cumulative_drc + theme(
                                axis.ticks.y = element_blank(), # removes the x-axis scales
                                axis.title.y = element_blank(), # removes the x-axis title
                                axis.ticks.y.right = element_blank(), # removes the right x-axis scales
                                axis.title.y.right = element_blank()), # removes the right x-axis title
                              cumulative_bf + theme(
                                axis.ticks.y = element_blank(), 
                                axis.title.y = element_blank(),
                                axis.ticks.y.right = element_blank(),
                                axis.title.y.right = element_blank()),
                              labels = c("MW", "DRC", "BF"), #Title for each graph
                              label_size = 10,
                              hjust = -0.2, vjust = 1.5)
plot_grid(cumulative_final,  ncol = 1, rel_heights = c(1, .1))

# CATI outcomes plot -----------------------------------------------------------

drccati <- dat.wide %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'DRC')
mwcati <- dat.widemw %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'MW')
rddcati <- rdd.dat.wide %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'BF-RDD')

# Change 'NNA or NR' to 'NNA/NR'
rddcati[rddcati == 'NNA or NR'] <- 'NNA/NR'

# Create CATIs outcomes df
catis <- rbind(drccati, mwcati, rddcati) %>%
  mutate(Eligibility = case_when(Outcome.FINAL %in% c('COMP', 'PART') ~ 'Eligible',
                                 Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                 Outcome.FINAL %in% c('REFU', 'PEND', 'Other', 'NNA/NR','NNA', 'NR' ) ~ 'Eligibility unknown',
                                 Outcome.FINAL %in% c('LANG', 'REASS') ~ 'Other Eligibility Unknown'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Eligibility Unknown')),
         Outcome.FINAL = factor(Outcome.FINAL, levels = c('COMP','PART', 'REFU', 'NNA/NR','NNA', 'NR','NNU', 'DEFER', 'PEND', 'INEL','Other', 'Other Eligibility Unknown')),
         Outcome.FINAL = case_when(Eligibility == 'Other Eligibility Unknown' | is.na(Outcome.FINAL) ~ 'Other Eligibility Unknown', TRUE ~ as.character(Outcome.FINAL)),
         Outcome.FINAL = factor(Outcome.FINAL, levels = c('COMP','PART', 'REFU','NNA/NR','NNA', 'NR', 'NNU', 'DEFER', 'PEND', 'INEL', 'Other', 'Other Eligibility Unknown')),
         percperoutcome = case_when(Eligibility == 'Other Eligibility Unknown' ~ as.numeric(percperoutcome), TRUE ~ as.numeric(percperoutcome)))

catis <- catis %>% 
  mutate(
    Eligibility = case_when(Eligibility == 'Other Eligibility Unknown' ~ 'Eligibility unknown', TRUE ~ as.character(Eligibility)),
    Eligibility = case_when(Outcome.FINAL == 'Other Eligibility Unknown' & is.na(Eligibility) ~ 'Other Eligibility Unknown', TRUE ~ as.character(Eligibility)),
    Eligibility = case_when(Eligibility == 'Other Eligibility Unknown' ~ 'Eligibility unknown', TRUE ~ as.character(Eligibility)))

# Plot CATI outcomes against number of unique phone numbers
# Malawi
cati_plot_mw <- ggplot(catis[catis$group=='MW',], aes(y=percperoutcome , x=Outcome.FINAL, fill=Eligibility)) +
  geom_bar(stat="identity", width = 0.8) +
  labs(x="",y="Percent of unique phone numbers") +
  theme_light() +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(limits = c(0, 35)) +
  scale_fill_brewer(palette = "Pastel2")
cati_plot_mw <- cati_plot_mw + guides(fill=guide_legend(title="CATI Outcome"))

# DRC
cati_plot_drc <- ggplot(catis[catis$group=='DRC',], aes(y=percperoutcome , x=Outcome.FINAL, fill=Eligibility)) +
  geom_bar(stat="identity") +
  labs(x="",y="Percent of unique phone numbers") +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(limits = c(0, 35)) +
  scale_fill_brewer(palette = "Pastel2")
cati_plot_drc <- cati_plot_drc + guides(fill=guide_legend(title="CATI Outcome"))

# BF
cati_plot_bf <- ggplot(catis[catis$group=='BF-RDD',], aes(y=percperoutcome , x=Outcome.FINAL, fill=Eligibility)) +
  geom_bar(stat="identity") +
  labs(x="",y="Percent of unique phone numbers") +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(limits = c(0, 35)) +
  scale_fill_brewer(palette = "Pastel2")
cati_plot_bf <- cati_plot_bf + guides(fill=guide_legend(title="CATI Outcome"))

# arrange the three plots into 4 quadrants
p1 <- plot_grid(cati_plot_mw + theme(legend.position="none", axis.text=element_text(size=12)), #remove legends first before combining legends
                cati_plot_drc+ theme(legend.position="none", axis.text=element_text(size=12)), 
                cati_plot_bf + theme(legend.position="none", axis.text=element_text(size=12)), 
                labels = c("MW", "DRC", "BF"),
                label_size = 9,
                hjust = -0.2, vjust = 1.9
)

# Adding subtitle with the eligibility unknown outcomes
p1<- add_sub(p1, "Other eligibility unknown = LANG and REASS", x = 0.69, y=3, size=12)

# extract the legend from one of the plots
legend_b <- get_legend(
  cati_plot_mw + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 4),
          legend.key.size = unit(2, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15))
)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(p1, legend_b, ncol = 1, rel_heights = c(1, .1))

# Call outcomes plot -----------------------------------------------------------

# Create df with calls info
drccalls <- data %>%
  filter(!is.na(Outcome2)) %>%
  group_by(Outcome2) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'DRC')
mwcalls <- datamw %>%
  filter(!is.na(Outcome2)) %>%
  group_by(Outcome2) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'MW')
bfcalls <- bf.data %>%
  filter(!is.na(Outcome)) %>%
  group_by(Outcome) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'BF')%>%
  dplyr::rename(Outcome2 = Outcome)

# Change 'NNA or NR' to 'NNA/NR'
rddcalls[rddcalls == 'NNA or NR'] <- 'NNA/NR'

calls <- rbind(drccalls, mwcalls, rddcalls) %>%
  mutate(Eligibility = case_when(Outcome2 %in% c('COMP', 'INCO') ~ 'Eligible',
                                 Outcome2 %in% c('DEFER','NNU') ~ 'Ineligible',
                                 Outcome2 %in% c('REFU', 'NNA/NR') ~ 'Eligibility unknown',
                                 Outcome2 %in% c('INCOR', 'LANG','REASS', 'Other') ~ 'Other Eligibility Unknown',
                                 Outcome2 %in% c('INEL', 'REFER') ~ 'Other Ineligible'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Eligibility Unknown', 'Other Ineligible')),
         Outcome2 = factor(Outcome2, levels = c('COMP','INCO', 'REFU','DEFER', 'NNA/NR','NNU',  'PEND', 'Other Eligibility Unknown', 'Other Ineligible')),
         Outcome2 = case_when(Eligibility == 'Other Eligibility Unknown' & is.na(Outcome2) ~ 'Other Eligibility Unknown', TRUE ~ as.character(Outcome2)),
         Outcome2 = case_when(Eligibility == 'Other Ineligible' & is.na(Outcome2) ~ 'Other Ineligible', TRUE ~ as.character(Outcome2)),
         Outcome2 = case_when(is.na(Outcome2) ~ 'Other Eligibility Unknown', TRUE ~ as.character(Outcome2)),
         Outcome2 = factor(Outcome2, levels = c('COMP','INCO', 'REFU','DEFER', 'NNA/NR','NNU',  'PEND', 'Other Eligibility Unknown', 'Other Ineligible')))

# Restricting the number of colours to 3 to represent Eligible, Ineligible and
# Eligibility unknown.
# Change objects in Eligibility column to one of the categories.
# e.g. change 'Other Eligibility Unknown' to just 'Eligibility unknown'.
calls <- calls %>% 
  mutate(
    Eligibility = case_when(Eligibility == 'Other Eligibility Unknown' ~ 'Eligibility unknown', TRUE ~ as.character(Eligibility)),
    Eligibility = case_when(Eligibility == 'Other Ineligible' ~ 'Ineligible', TRUE ~ as.character(Eligibility)),
    Eligibility = case_when(is.na(Eligibility) ~ 'Eligibility unknown', TRUE ~ as.character(Eligibility))
  )

# Plot phone call outcomes

#MW
call_plot_mw <- ggplot(calls[calls$group=='MW',], aes(y=percperoutcome , x=Outcome2, fill=Eligibility)) +
  geom_bar(stat="identity") +
  labs(x="",y="Percent of phone calls") +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(15)) + #limits no characters on x-axis to 20
  scale_y_continuous(limits = c(0, 80)) + #limits scale of y-axis to 0-80 
  scale_fill_brewer(palette = "Pastel2")
call_plot_mw <- call_plot_mw + guides(fill=guide_legend(title="Call Outcome")) #Title for legend

#DRC
call_plot_drc <- ggplot(calls[calls$group=='DRC',], aes(y=percperoutcome , x=Outcome2, fill=Eligibility)) +
  geom_bar(stat="identity") +
  labs(x="",y="Percent of phone calls") +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_brewer(palette = "Pastel2")
call_plot_drc <- call_plot_drc + guides(fill=guide_legend(title="Call Outcome"))

#BF
call_plot_bf <- ggplot(calls[calls$group=='BF-RDD',], aes(y=percperoutcome , x=Outcome2, fill=Eligibility)) +
  geom_bar(stat="identity") +
  labs(x="",y="Percent of phone calls") +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_brewer(palette = "Pastel2")
call_plot_bf <- call_plot_bf + guides(fill=guide_legend(title="Call Outcome"))

# arrange the three plots into 4 quadrants
p2 <- plot_grid(call_plot_mw + theme(legend.position="none", axis.text=element_text(size=12)), #remove legends first before combining legends
                call_plot_drc+ theme(legend.position="none", axis.text=element_text(size=12)),
                call_plot_bf + theme(legend.position="none", axis.text=element_text(size=12)), 
                labels = c("MW", "DRC", "BF"), #Title for each graph
                label_size = 10
)

# Adding subtitle with the eligibility unknown outcomes
p2<- add_sub(p2, "Other eligibility unknown = INCOR, LANG, REASS, Other \n Other ineligible = INEL, REFER", x = 0.69, y=3, size=12, hjust = 0.4)

# extract the legend from Malawi plots
legend_call <- get_legend(
  call_plot_mw + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 5),
          legend.key.size = unit(2, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15))
)
# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(p2, legend_call, ncol = 1, rel_heights = c(1, .1))


