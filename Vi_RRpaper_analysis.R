################################################################################
## Response Rates Paper Code
## Vi Bui and Kelly McCaine 
################################################################################

################################################################################
## Load Libraries
################################################################################
pkgs <- c('purrr','plyr','tidyverse', 'plotrix','lubridate','kableExtra','hrbrthemes','ggplot2','extrafont','float','reshape',
          'gridExtra','rsvg','png','devtools','readxl','date', 'ggpubr', 'tidyselect', 'httr', 'jsonlite', 'extrafont', 'colorspace',
          'ggrepel', 'forcats', 'ggpubr', 'readstata13', 'scales')
lapply(pkgs, require, character.only = TRUE)

dark2pal <- c("#1B9E77", "#D95F02", "#7570B3", "#66A61E", "#E6AB02")

################################################################################
## Reading in data
################################################################################

# Setting directories
r.functions.dir <- '/Users/lshvb5/Documents/rammps/'
# r.functions.dir <- 'C:/Users/Kelly McCain/Documents/LSHTM/rammps/'
dir.inputdata <- ('/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/UNIKIN/SurveyCTO Audits/')
# dir.inputdata <- "/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/UNIKIN/SurveyCTO Audits


# DRC
Consented <- read.csv(paste0(dir.inputdata, "Clean data/Consented2022-08-23.csv"))%>%
  mutate(source = ifelse(grepl('Fer', Source.prov),'Feroxus',
                         ifelse(grepl('IVR', Source.prov),'IVR',NA)),
         Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2)))
         # Outcome2 = ifelse(Outcome2 =='INEL'| Outcome2 =='REFER', 'Other Ineligible', as.character(Outcome2)),
         # Outcome2 = ifelse(Outcome2 =='INCOR'| Outcome2 =='LANG' | Outcome2 == 'REASS' | Outcome2 == 'Other', 'Other Eligibility Unknown', as.character(Outcome2)))
         # Outcome2 = ifelse(Outcome2 =='LANG'| Outcome2 =='NNA' | Outcome2 == 'NR' | Outcome2 == 'REASS', 'CATI Other Eligibility Unknown', as.character(Outcome2)))
data <- read.csv(paste0(dir.inputdata, "Clean data/Data_Long2022-08-23.csv"))%>%
  mutate(source = ifelse(grepl('Fer', Source.prov),'Feroxus',
                         ifelse(grepl('IVR', Source.prov),'IVR',NA)),
         Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2)))
         # Outcome2 = ifelse(Outcome2 =='INEL'| Outcome2 =='REFER', 'Other Ineligible', as.charcter(Outcome2)),
         # Outcome2 = ifelse(Outcome2 =='INCOR'| Outcome2 =='LANG' | Outcome2 == 'REASS' | Outcome2 == 'Other', 'Other Eligibility Unknown', as.character(Outcome2)))
         # Outcome2 = ifelse(Outcome2 =='LANG'| Outcome2 =='NNA' | Outcome2 == 'NR' | Outcome2 == 'REASS', 'CATI Other Eligibility Unknown', as.character(Outcome2)))
dat.wide <- read.csv(paste0(dir.inputdata, "Clean data/Data_Wide2022-08-23.csv")) %>%
  mutate(source = ifelse(grepl('Fer',Source.prov.1),'Feroxus',
                         ifelse(grepl('IVR',Source.prov.1),'IVR', NA)),
         Eligibility = case_when(Outcome.FINAL %in% c('COMP', 'INCO', 'PART') ~ 'Eligible',
                                 Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                 Outcome.FINAL %in% c('REFU','NNA','NR','NNA/NR','LANG','PEND') ~ 'Eligibility unknown'),
                                 # Outcome.FINAL %in% c('Other Ineligible') ~ 'Other Ineligible',
                                 # Outcome.FINAL %in% c('Other Eligibility Unknown') ~ 'Other Eligibility Unknown'),
                                 # Outcome.FINAL %in% c('CATI Other Eligibility Unknown') ~ 'CATI Other Eligibility Unknown'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligible Unknown', 'CATI Other Eligibility Unknown')),
         Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)))
         # Outcome.FINAL = ifelse(Outcome.FINAL =='INEL'| Outcome.FINAL =='REFER', 'Other Ineligible', as.character(Outcome.FINAL)),
         # Outcome.FINAL = ifelse(Outcome.FINAL =='INCOR'| Outcome.FINAL =='LANG' | Outcome.FINAL == 'REASS' | Outcome.FINAL == 'Other', 'Other Eligibility Unknown', as.character(Outcome.FINAL)))
         # Outcome.FINAL = ifelse(Outcome.FINAL =='LANG'| Outcome.FINAL =='NNA' | Outcome.FINAL == 'NR' | Outcome.FINAL == 'REASS', 'CATI Other Eligibility Unknown', as.character(Outcome.FINAL)))
unique(Consented$call_feedback_other)
# Malawi
Consentedmw <- read.csv(paste0(r.functions.dir, 'RaMMPS_MWApp/ConsentedMW.csv')) %>%
  mutate(Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2))) #%>%
  # mutate(Outcome2 = ifelse(Outcome2 =='INEL'| Outcome2 =='REFER', 'Other Ineligible', as.character(Outcome2))) %>%
  # mutate(Outcome2 = ifelse(Outcome2 =='INCOR'| Outcome2 =='LANG' | Outcome2 == 'REASS' | Outcome2 == 'Other', 'Other Eligibility Unknown', as.character(Outcome2))) #%>%
  # mutate(Outcome2 = ifelse(Outcome2 =='LANG'| Outcome2 =='NNA' | Outcome2 == 'NR' | Outcome2 == 'REASS', 'CATI Other Eligibility Unknown', as.character(Outcome2)))
dat.widemw <- read.csv(paste0(r.functions.dir,'RaMMPS_MWApp/DatWide.csv'))%>%
  mutate(Eligibility = case_when(Outcome.FINAL %in% c('COMP', 'INCO', 'PART') ~ 'Eligible',
                               Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                               Outcome.FINAL %in% c('REFU','NNA','NR','NNA/NR','LANG','PEND') ~ 'Eligibility unknown'),
                               # Outcome.FINAL %in% c('Other Ineligible') ~ 'Other Ineligible',
                               # Outcome.FINAL %in% c('Other Eligibility Unknown') ~ 'Other Eligibility Unknown'),
                               # Outcome.FINAL %in% c('CATI Other Eligibility Unknown') ~ 'CATI Other Eligibility Unknown'),
       Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligible Unknown', 'CATI Other Eligibility Unknown')),
       Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)))
       # Outcome.FINAL = ifelse(Outcome.FINAL =='INEL'| Outcome.FINAL =='REFER', 'Other Ineligible', as.character(Outcome.FINAL)),
       # Outcome.FINAL = ifelse(Outcome.FINAL =='INCOR'| Outcome.FINAL =='LANG' | Outcome.FINAL == 'REASS' | Outcome.FINAL == 'Other', 'Other Eligibility Unknown', as.character(Outcome.FINAL)))
       # Outcome.FINAL = ifelse(Outcome.FINAL =='LANG'| Outcome.FINAL =='NNA' | Outcome.FINAL == 'NR' | Outcome.FINAL == 'REASS', 'CATI Other Eligibility Unknown', as.character(Outcome.FINAL)))

datamw <- read.csv(paste0(r.functions.dir,'RaMMPS_MWApp/DatLongMW.csv')) %>%
  # filter(call_num %in% c(1:5)) %>%
  dplyr::filter(!is.na(Outcome2)) %>%
  mutate(Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2))) #%>%
  # mutate(Outcome2 = ifelse(Outcome2 =='INEL'| Outcome2 =='REFER', 'Other Ineligible', as.character(Outcome2))) %>%
  # mutate(Outcome2 = ifelse(Outcome2 =='INCOR'| Outcome2 =='LANG' | Outcome2 == 'REASS' | Outcome2 == 'Other', 'Other Eligibility Unknown', as.character(Outcome2))) #%>%
  # mutate(Outcome2 = ifelse(Outcome2 =='LANG'| Outcome2 =='NNA' | Outcome2 == 'NR' | Outcome2 == 'REASS', 'CATI Other Eligibility Unknown', as.character(Outcome2)))

# Burkina Faso 
dir.input <- "/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/Burkina/Survey Data/"
# dir.input <- "C:/Users/Vi Bui/London School of Hygiene and Tropical Medicine/RAMMPS_LSHTM_Group - Documents/General/Partners/Burkina/Survey Data/"
eval(parse(paste0(r.functions.dir,'CleaningScript_func_BF.R'), encoding="UTF-8"))
listbf <- clean_bf() #clean BF data

rdd.data <- listbf[[1]] %>%
  mutate(
    Outcome = ifelse(Outcome =='NNA'| Outcome =='NR', 'NNA/NR', as.character(Outcome)),
    # Outcome = ifelse(Outcome =='INEL'| Outcome =='REFER', 'Other Ineligible', as.character(Outcome)),
    # Outcome = ifelse(Outcome =='INCOR'| Outcome =='LANG' | Outcome == 'REASS' | Outcome == 'Other', 'Other Eligibility Unknown', as.character(Outcome)),
    # Outcome = ifelse(Outcome =='LANG'| Outcome =='NNA' | Outcome == 'NR' | Outcome == 'REASS', 'CATI Other Eligibility Unknown', as.character(Outcome)),
    Eligibility = case_when(Outcome %in% c('COMP','PART') ~ 'Eligible',
                            Outcome %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                            Outcome %in% c('REFU','NNA','NR','LANG','PEND', 'NNA or NR', 'Other') ~ 'Eligibility unknown'),
                            # Outcome %in% c('Other Ineligible') ~ 'Other Ineligible',
                            # Outcome %in% c('Other Eligibility Unknown') ~ 'Other Eligibility Unknown'),
                            # Outcome.FINAL %in% c('CATI Other Eligibility Unknown') ~ 'CATI Other Eligibility Unknown'),
    Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligibility Unknown', 'CATI Other Eligibility Unknown')),
    Outcome = ifelse(Outcome =='NNA'| Outcome =='NR', 'NNA/NR', as.character(Outcome)))
    # Outcome = ifelse(Outcome =='INEL'| Outcome =='REFER', 'Other Ineligible', as.character(Outcome)),
    # Outcome = ifelse(Outcome =='INCOR'| Outcome =='LANG' | Outcome == 'REASS' | Outcome == 'Other', 'Other Eligibility Unknown', as.character(Outcome)))
    # Outcome = ifelse(Outcome =='LANG'| Outcome =='NNA' | Outcome == 'NR' | Outcome == 'REASS', 'CATI Other Eligibility Unknown', as.character(Outcome)))

ehcvm.data <- listbf[[2]]
rdd.dat.wide <- listbf[[3]] %>%
  mutate(
    Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)),
    # Outcome.FINAL = ifelse(Outcome.FINAL =='INEL'| Outcome.FINAL =='REFER', 'Other Ineligible', as.character(Outcome.FINAL)),
    # Outcome.FINAL = ifelse(Outcome.FINAL =='INCOR'| Outcome.FINAL =='LANG' | Outcome.FINAL == 'REASS' | Outcome.FINAL == 'Other', 'Other Eligibility Unknown', as.character(Outcome.FINAL)),
    # Outcome.FINAL = ifelse(Outcome.FINAL =='LANG'| Outcome.FINAL =='NNA' | Outcome.FINAL == 'NR' | Outcome.FINAL == 'REASS', 'CATI Other Eligibility Unknown', as.character(Outcome.FINAL)),
    Eligibility = case_when(Outcome.FINAL %in% c('COMP','PART') ~ 'Eligible',
                            Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                            Outcome.FINAL %in% c('REFU','NNA','NR','LANG','PEND', 'NNA or NR', 'Other') ~ 'Eligibility unknown'),
                            # Outcome.FINAL %in% c('Other Ineligible') ~ 'Other Ineligible',
                            # Outcome.FINAL %in% c('Other Eligibility Unknown') ~ 'Other Eligibility Unknown'),
                            # Outcome.FINAL %in% c('CATI Other Eligibility Unknown') ~ 'CATI Other Eligibility Unknown'),
    Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligibility Unknown', 'CATI Other Eligibility Unknown')),
    Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)))
    # Outcome.FINAL = ifelse(Outcome.FINAL =='INEL'| Outcome.FINAL =='REFER', 'Other Ineligible', as.character(Outcome.FINAL)),
    # Outcome.FINAL = ifelse(Outcome.FINAL =='INCOR'| Outcome.FINAL =='LANG' | Outcome.FINAL == 'REASS' | Outcome.FINAL == 'Other', 'Other Eligibility Unknown', as.character(Outcome.FINAL)))
    # Outcome.FINAL = ifelse(Outcome.FINAL =='LANG'| Outcome.FINAL =='NNA' | Outcome.FINAL == 'NR' | Outcome.FINAL == 'REASS', 'CATI Other Eligibility Unknown', as.character(Outcome.FINAL)))

ehcvm.dat.wide <- listbf[[4]]
bf.dat.wide <- listbf[[5]]
bf.data <- listbf[[6]]
Consentedrdd <- rdd.data %>%
  filter(Outcome == 'COMP') %>%
  dplyr::mutate(month.interview1 = lubridate::floor_date(as.Date(Date.Interview), 'month'))#month(Date.Interview),

Consentedehcvm <- ehcvm.data %>%
  filter(Outcome == 'COMP') %>%
  dplyr::mutate(month.interview1 = lubridate::floor_date(as.Date(Date.Interview), 'month'),#month(Date.Interview),
                month.interview = case_when(month.interview == '2022-01-01'~ 'Jan-22',
                                            month.interview == '2022-02-01'~ 'Feb-22',
                                            month.interview == '2022-03-01'~ 'Mar-22',
                                            month.interview == '2022-04-01'~'Apr-22',
                                            month.interview == '2022-05-01'~'May-22',
                                            month.interview == '2022-06-01'~'Jun-22',
                                            month.interview == '2022-07-01'~'Jul-22',
                                            month.interview == '2022-08-01'~'Aug-22',
                                            month.interview == '2022-09-01'~'Sep-22',
                                            month.interview == '2021-08-01'~'Aug-21',
                                            month.interview == '2021-09-01'~'Sep-21',
                                            month.interview == '2021-10-01'~'Oct-21',
                                            month.interview == '2021-11-01'~'Nov-21',
                                            month.interview == '2021-12-01'~'Dec-21'))
Consentedbf <- bf.data %>%
  filter(Outcome == 'COMP')
# beep()

################################################################################
## Response Rate Calculations
################################################################################
# response rate 1 COMP / [COMP+ PART+ REFU + NR (5 attempts) + NNA (5attempts)+LANG]
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


dfs <- list( #'ferkin'=ferkin,'fernk'=fernk,  'ivrkin'=ivrkin,  'ivrnk'=ivrnk,
            #'kin' = kin, 'nk'= nk,'northmw'=northmw, 'centralmw' = centralmw, 'southmw' = southmw
            # 'fer'=fer, 'ivr'=ivr, 
            'alldrc'=alldrc, 'allmw' = allmw, 'rddbf' = rddbf)
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

write.csv(rrs_df, paste0('/Users/lshvb5/Documents/rammps/response_rates_data-', Sys.Date(),'.csv'))

################################################################################
## Response rates plots
################################################################################

# # read in the csv file with the response rate and refusal rate
# res_data <- read.csv('/Users/lshvb5/Documents/rammps_data/2022-11-28.csv')
# 
# # Change column name X to Statistics
# names(res_data)[names(res_data) == 'X'] <- 'Statistics'
# 
# # 
# # ha <- res_data %>%
# #   select(Statistics, alldrc, allmw, rddbf) %>%
# #   filter((Statistics == 'RR1' | Statistics == 'RR1e' | Statistics == 'RR1_0' |
# #             Statistics == 'RR2' | Statistics == 'RR2e' | Statistics == 'RR2_0' |
# #             Statistics == 'RefR1' | Statistics == 'RefR1e' | Statistics == 'RefR1_0'))
# 
# # Create data with relevant data
# ha <- res_data %>%
#   select(Statistics, alldrc, allmw, rddbf) %>%
#   filter((Statistics == 'RR1' | Statistics == 'RR1e' | Statistics == 'RR1_0' |
#             Statistics == 'RR2' | Statistics == 'RR2e' | Statistics == 'RR2_0' |
#             Statistics == 'RefR1' | Statistics == 'RefR1e' | Statistics == 'RefR1_0')) %>%
#   mutate(Statistic = case_when(Statistics %in% c('RR1', 'RR1e', 'RR1_0') ~ 'Response Rate 1',
#         Statistics %in% c('RR2', 'RR2e', 'RR2_0') ~ 'Response Rate 2',
#         Statistics %in% c('RefR1', 'RefR1e', 'RefR1_0') ~ 'Refusal Rate')) 
# 
# # create long version of df to plot
# ha <- melt(ha, value.name =Statistic)
# 
# # Change 'variable' column name to 'country'
# names(ha)[names(ha) == 'variable'] <- 'Country'
# 
# 
# ha <- ha %>%
#   mutate(Country = case_when(Country == 'alldrc' ~ 'DRC', TRUE ~ as.character(Country)),
#          Country = case_when(Country == 'allmw' ~ 'MW', TRUE ~ as.character(Country)),
#          Country = case_when(Country == 'rddbf' ~ 'BF', TRUE ~ as.character(Country)))
# 
# # 
# # 
# # # Plot response and refusal rates
# # plt <- ggplot(data = ha, aes(x = Statistic, y = value, color=variable))
# # plt + geom_boxplot() + theme_bw() + labs(x = "Statistics", y = "",
# #       color = "Country") + scale_color_manual(labels = c("DRC", "MW", "BF") , values = c("blue", "red", "green"))
# 
# # Plot the with Country col
# plt <- ggplot(data = ha, aes(x = Statistic, y = value, fill=Country))
# plt + geom_boxplot() + theme_bw() + labs(x = "Statistics", y = "") +
#   scale_fill_brewer(palette="Set1")
# 
# plt = plt + scale_color_npg()
# plt = plt + scale_fill_npg()
# 

################################################################################
## Response rates per characteristics
################################################################################

# #drc data
# #combining the characteristics with the response data
# combined_char <- dat.wide %>%
#   inner_join(data, na.rm=True)
# 
# #checking whether sex is in this?
# combined_char$Resp.Sex
# 
# # getting the outcomes and the sex
# alldrccomb <- combined_char%>%
#   group_by(Outcome.FINAL, Resp.Sex) %>%
#   tally()
# 
# # Split df based on sex
# alldrccomb_div<-split(alldrccomb, alldrccomb$Resp.Sex)
# drc_females <- alldrccomb_div[[1]] # used to access either characteristic
# drc_males <- alldrccomb_div[[2]]
# str(alldrccomb_div)
# 
# # mw
# combined_charmw <- dat.widemw %>%
#   inner_join(datamw,
#              by = c('phone_1'='phone_1'))
# 
# allmwcomb <- combined_charmw %>%
#   group_by(Outcome.FINAL, Resp.Sex) %>%
#   tally()
# 
# allmwcomb_div<-split(allmwcomb, allmwcomb$Resp.Sex)
# mw_females <- allmwcomb_div[[1]] # used to access either characteristic
# mw_males <- allmwcomb_div[[2]]
# 
# # bf
# combined_charbf <- rdd.dat.wide %>%
#   inner_join(rdd.data)
# 
# 
# allbfcomb <- combined_charbf %>%
#   group_by(Outcome.FINAL, gender) %>%
#   mutate(gender = case_when(gender==1 ~ "Male",
#                          gender==2 ~ "Female",
#                          is.na(gender)~ "Male")) %>%
#   tally()
# 
# allbfcomb_div<-split(allbfcomb, allbfcomb$gender)
# bf_females <- allbfcomb_div[[1]] # used to access either characteristic
# bf_males <- allbfcomb_div[[2]]
# 
# dfscomb <- list('drc_females'=drc_females, 'drc_males'=drc_males, 'mw_females'=mw_females, 'mw_males'=mw_males,
#                 'bf_females'=bf_females, 'bf_males'=bf_males)
# 
# # working out the response rate among those contacted
# # objects are different sizes
# ########################################
# # Response rate among only those who have been contacted
# response_rate_func <- function(df, e){
#   rr2 <- c()
#   unk_inel1 <- unk_inel(df)
#   rr2 <- round(df[df$Outcome.FINAL=='COMP',]$n / 
#                  sum(df[df$Outcome.FINAL=='COMP',]$n, df[df$Outcome.FINAL=='PART',]$n, df[df$Outcome.FINAL=='LANG',]$n, df[df$Outcome.FINAL=='REFU',]$n,  na.rm = T) *100,2)
#   rr2 <- c('RR2'=rr2, "p_elig"=e)
#   return(rr2)
# }
# 
# # rr2s <- rr13_func(dfs, e= 1) 
# response_rate_res <- list()
# for(i in 1:length(dfscomb)){
#   response_rate_res[[i]] <- response_rate_func(dfscomb[[i]], e = 1)
# }
# 
# # Make tables
# list_res <- list('response_rate_res' = response_rate_res)
# list_res_df <- purrr::map_df(list_res, bind_cols) %>% as.data.frame() 
# list_res_df <- list_res_df[-c(2),]
# rownames(list_res_df) <- c('RR5')
# colnames(list_res_df) <- names(dfscomb)
# 
# write.csv(list_res_df, paste0('/Users/vibui/Documents/response_data-', Sys.Date(),'.csv'))
# 
# # Read in the data to produce plots
# 
# resp_sex <- read.csv("/Users/vibui/Documents/response_data-2022-12-06.csv")
# 
# # create long version of df to plot
# resp_melt <- melt(resp_sex)
# 
# # Create sex categories to colour code plot
# resp_melt <- resp_melt %>%
#   mutate(
#     Sex = case_when(variable %in% c('drc_females', 'mw_females', 'bf_females') ~ "Females",
#                     variable %in% c('drc_males', 'mw_males', 'bf_males') ~ "Males"),
#     Country = case_when(variable %in% c('drc_females', 'drc_males') ~ 'DRC',
#                           variable %in% c('mw_females', 'mw_males') ~ 'MW',
#                         variable %in% c('bf_females', 'bf_males') ~ 'BF'))
# 
# # Create plot
# resp_plot <- ggplot(data = resp_melt, aes(x = Country, y = value, fill=Sex))
# resp_plot + geom_bar(stat='identity', position = "dodge", width = 0.6) +
#   labs(x = "", y = "Response rate among those contacted (%)") +
#   scale_fill_brewer(palette="Set1") +
#   theme_bw() 
# 
#     compbycall <- ggplot(data = outcomes) + 
#       geom_line(aes(x = call_num_grp, y = perccum, group=ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD')), 
#                     color = ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD'))), size = 0.8) + 
#       geom_point(aes(x = call_num_grp, y = perccum, group=ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD')), 
#                      color = ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD'))), size = 2) + 
# 
# test <- Consentedmw %>%
#   group_by(Outcome2, Resp.Sex) %>%
#   tally() 
# 
# testMW<-split(test, test$Resp.Sex)
# MW_females <- allmwcomb_div[[1]] # used to access either characteristic
# MW_males <- allmwcomb_div[[2]]


# ## Age plots
# 
# # MW    
# MWage <- combined_charmw %>%
#   group_by(Outcome.FINAL, Resp.Age.pyr) %>%
#   tally()
# 
# MWage_div <- split(MWage, MWage$Resp.Age.pyr) #Split based on Age categories
# MWage_14_19 <- MWage_div[1] #Get each age category into separate variable
# MWage_19_29 <- MWage_div[2]
# MWage_29_39 <- MWage_div[3]
# MWage_39_49 <- MWage_div[4]
# MWage_49_59 <- MWage_div[5]
# MWage_59_64 <- MWage_div[6]
# 
# # DRC
# combined_char$Resp.Age.pyr
# 
# DRCage <- combined_char %>%
#   group_by(Outcome.FINAL, Resp.Age.pyr) %>%
#   tally()
# 
# DRCage_div <- split(DRCage, DRCage$Resp.Age.pyr)
# DRCage_14_19 <- DRCage_div[1] 
# DRCage_19_29 <- DRCage_div[2]
# DRCage_29_39 <- DRCage_div[3]
# DRCage_39_49 <- DRCage_div[4]
# DRCage_49_59 <- DRCage_div[5]
# DRCage_59_64 <- DRCage_div[6]
# 
# # BF
# combined_charbf$Outcome.FINAL['COMP']
# 
# BFage <- combined_charbf %>%
#   group_by(Outcome.FINAL, age) %>%
#   tally()
# 
# BFage <- combined_charbf %>%
#   group_by(Outcome.FINAL, age) %>%
#   mutate(age = case_when(age <= 19 ~ "14-19",
#                         (age > 19 && age <=29) ~ "19-29",
#                         (age > 29 && age <=39) ~ "29-39",
#                         (age > 39 && age <=49) ~ "39-49",
#                         (age > 49 && age <=59) ~ "49-59",
#                         (age > 59) ~ "59-64")) %>%
#   tally()
# 
# BFage[1]
# mutate(gender = case_when(gender==1 ~ "Male",
#                           gender==2 ~ "Female",
#                           is.na(gender)~ "Male")) %>%
#   tally()
# 
# BFage_div <- split(BFage, BFage$age)
# BFage_14_19 <- BFage_div[1] 
# BFage_19_29 <- BFage_div[2]
# BFage_29_39 <- BFage_div[3]
# BFage_39_49 <- BFage_div[4]
# BFage_49_59 <- BFage_div[5]
# BFage_59_64 <- BFage_div[6]
# 
# # Combine ages from different countries
# dfscombAge <- list('MWage_14_19' = MWage_14_19, 
#                    'MWage_19_29' = MWage_19_29,
#                    'MWage_29_39' = MWage_29_39,
#                    'MWage_49_59' = MWage_49_59,
#                    'MWage_59_64' = MWage_59_64,
#                    'DRCage_14_19' = DRCage_14_19, 
#                    'DRCage_19_29' = DRCage_19_29,
#                    'DRCage_29_39' = DRCage_29_39,
#                    'DRCage_49_59' = DRCage_49_59,
#                    'DRCage_59_64' = DRCage_59_64,
#                    'BFage_14_19' = BFage_14_19, 
#                    'BFage_19_29' = BFage_19_29,
#                    'BFage_29_39' = BFage_29_39,
#                    'BFage_49_59' = BFage_49_59,
#                    'BFage_59_64' = BFage_59_64)
# 
# 
# # Calculate response rate among only those who have been contacted
# 
# response_rate_func <- function(df, e){
#   rr2 <- c()
#   unk_inel1 <- unk_inel(df)
#   rr2 <- round(df[df$Outcome.FINAL=='COMP',]$n / 
#                  sum(df[df$Outcome.FINAL=='COMP',]$n, df[df$Outcome.FINAL=='PART',]$n, df[df$Outcome.FINAL=='LANG',]$n, df[df$Outcome.FINAL=='REFU',]$n,  na.rm = T) *100,2)
#   rr2 <- c('RR2'=rr2, "p_elig"=e)
#   return(rr2)
# }
# 
# response_rate_res_age <- list()
# for(i in 1:length(dfscombAge)){
#   response_rate_res_age[[i]] <- response_rate_func(dfscombAge[[i]], e = 1)
# }
# 
# # Make tables
# list_res <- list('response_rate_res_age' = response_rate_res_age)
# list_res_df <- purrr::map_df(list_res, bind_cols) %>% as.data.frame() 
# list_res_df <- list_res_df[-c(2),]
# rownames(list_res_df) <- c('RR5')
# colnames(list_res_df) <- names(dfscombAge)
# 
# write.csv(list_res_df, paste0('/Users/vibui/Documents/response_data-', Sys.Date(),'.csv'))
# 

########################################
# Total CATIs completed
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

### CATI Completion rates by call order plot 
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


# outcomesdrc <- dat.wide %>%
#   select(call_num, Outcome.FINAL, caseid) %>%
#   mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
#   dplyr::group_by(call_num_grp, Outcome.FINAL, caseid) %>%
#   dplyr::summarize(n = n()) %>%
#   ungroup() %>%
#   filter(Outcome.FINAL == 'COMP') %>%
#   dplyr::mutate(totalcomp = sum(n),
#                 totalnums = nrow(dat.wide),
#                 perc = round(n/totalnums*100,2), #percent of completed calls
#                 perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
#   ungroup() %>%
#   dplyr::mutate(perccum = cumsum(perc),
#                 country = 'DRC') 
# 
# select_drc <- Consented %>%
#   select(caseid, phone_call_duration)
# 
# outcomesdrc_join <- left_join(outcomesdrc, select_drc, by="caseid", na.rm=True)
#MW
# outcomesmw <- dat.widemw %>%
#   mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
#   dplyr::group_by(call_num_grp, Outcome.FINAL, phone_call_duration, caseid) %>%
#   dplyr::summarize(n = n()) %>%
#   ungroup() %>%
#   filter(Outcome.FINAL == 'COMP') %>%
#   dplyr::mutate(totalcomp = sum(n),
#                 totalnums = nrow(dat.widemw),
#                 perc = round(n/totalnums*100,2),
#                 perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
#   ungroup() %>%
#   dplyr::mutate(perccum = cumsum(perc),
#                 country = 'MW') 

# outcomesmw <- dat.widemw %>%
#   mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
#   dplyr::group_by(call_num_grp, Outcome.FINAL, phone_call_duration, region) %>%
#   dplyr::summarize(n = n()) %>%
#   ungroup() %>%
#   filter(Outcome.FINAL == 'COMP') %>%
#   dplyr::mutate(totalcomp = sum(n),
#                 totalnums = nrow(dat.widemw),
#                 perc = round(n/totalnums*100,2),
#                 perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
#   ungroup() %>%
#   dplyr::mutate(perccum = cumsum(perc),
#                 country = 'MW') 

######## Code that results in no perc


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
#BF - EHCVM
outcomesehcvm <- ehcvm.dat.wide %>%
  mutate(call_num_grp = ifelse(nbr_appel > 5, '5+',as.character(nbr_appel))) %>%
  dplyr::group_by(call_num_grp, Outcome.FINAL) %>%
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome.FINAL == 'COMP') %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(ehcvm.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'BF-EHCVM') 

#BF
outcomesbf <- bf.dat.wide %>%
  mutate(call_num_grp = ifelse(nbr_appel > 5, '5+',as.character(nbr_appel))) %>%
  dplyr::group_by(call_num_grp, Outcome.FINAL) %>%
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome.FINAL == 'COMP') %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  ungroup() %>%
  dplyr::mutate(perccum = cumsum(perc),
                country = 'BF') 

# Combined df
data <- data %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num)))
datamw <- datamw %>% 
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num)))
rdd.data <- rdd.data %>% 
  mutate(call_num_grp = ifelse(nbr_appel > 5, '5+',as.character(nbr_appel)))
ehcvm.data <- ehcvm.data %>% 
  mutate(call_num_grp = ifelse(nbr_appel > 5, '5+',as.character(nbr_appel)))
bf.data <- bf.data %>% 
  mutate(call_num_grp = ifelse(nbr_appel > 5, '5+',as.character(nbr_appel)))

# outcomesdrc_join$Calls <- c(table(data$call_num_grp))
outcomesdrc$Calls <- c(table(data$call_num_grp))
outcomesmw$Calls <- c(table(datamw$call_num_grp))
outcomesrdd$Calls <- c(table(rdd.data$call_num_grp))
outcomesehcvm$Calls <- c(table(ehcvm.data$call_num_grp))
outcomesbf$Calls <- c(table(bf.data$call_num_grp))
outcomes <- rbind(outcomesdrc, outcomesmw, outcomesrdd, outcomesehcvm, outcomesbf) %>%
  mutate(country = forcats::fct_inorder(country))

# #binding the columns
# binding <- rbind(outcomesdrc_join, outcomesmw)
# ggplot(binding) +
#   geom_boxplot(aes(x=call_num_grp, y=phone_call_duration/60, fill=country)) +
#   labs(x="Call Attempt", y="Phone call duration")
# 
# binding <- rbind(outcomesdrc_join, outcomesmw)
# ggplot(binding) +
#   geom_line(aes(x=call_num_grp, 
#                 y=phone_call_duration/60, 
#                 group=ordered(country, levels = c('DRC','MW')),
#                 color = ordered(country, levels = c('DRC','MW')), size = 1)) +
#   labs(x="Call Attempt", y="Phone call duration")
# 
# 
# 
# # Working out the median of phone_call_response based on region
# unique(outcomesmw$call_num_grp)
# as.data.frame(outcomesmw)
# dur_attemp_mw <- outcomesmw %>%
#   group_by(call_num_grp, region) %>%
#   summarise(ph_dur = median(as.numeric(phone_call_duration/60))) %>%
#   mutate(group = 'MW')
# dur_attemp_fin_mw <- dur_attemp_mw %>%
#   mutate(call_num_grp = factor(call_num_grp, levels = c("1",  "2",  "3",  "4" , "5",  "5+")))
# # Plotting the phone_call_attempt against the call attempt
# ggplot(dur_attemp_fin_mw) +
#   geom_boxplot(aes(x=call_num_grp, y=ph_dur)) +
#   labs(x="Call Attempt", y="Median Phone call duration")
# 
# 
# Outcomes for rdd
outcomes_rdd <- rbind(outcomesdrc, outcomesmw, outcomesrdd) %>%
  mutate(country = forcats::fct_inorder(country))

write.csv(outcomes_rdd, "outcomes_rdd.csv")

#Plot
comprate <- ggplot(data = outcomes ) +
  geom_line(aes(x = call_num_grp, y = perccompletedpercallnum, group=ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD')), 
                                                                             color = ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD'))),
                                                                             size = 1) +#linetype = country), color = 'grey50',
  geom_point(aes(x = call_num_grp, y = perccompletedpercallnum, group=country,color = country ), size = 2) +#
  geom_text_repel(#data = filter(outcomes, call_num_grp =='1'|call_num_grp=='2'|call_num_grp=='3'),
                  aes(x = call_num_grp, y = perccompletedpercallnum, label = paste0(perccompletedpercallnum,'%'),
                  color = stage(country, after_scale = colorspace::darken(color, .25))), 
                  fontface = 'bold',  box.padding = .6, min.segment.length = 0) +
  xlab('Call attempt order') +
  ylab('% of completed CATIs per call num') +
  scale_color_manual(values = c("#1B9E77", "#D95F02","#7570B3" ,"#E6AB02", "#66A61E")) +
  scale_y_continuous(breaks = seq(0,100, by = 5), limits = c(0,90))+
  labs( color = 'Country', linetype = 'Country') +
  theme_minimal()+
  theme(legend.position = c(0.7, 0.7)) + 
  # theme_minimal() + 
  coord_cartesian(ylim = c(0,85)) 
comprate

### Cumulative completed CATIs with phone calls made by call order
compbycall <- ggplot(data = outcomes) + 
  geom_line(aes(x = call_num_grp, y = perccum, group=ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD')), 
                color = ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD'))), size = 0.8) + 
  geom_point(aes(x = call_num_grp, y = perccum, group=ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD')), 
                 color = ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD'))), size = 2) + 
  # geom_bar(aes(y = log(Calls)*1.1, x = call_num_grp, group=country, fill = country), position= 'dodge',alpha = 0.8,stat = 'identity') +
  # geom_bar(aes(y = log(Calls)*1.1, x = call_num_grp, group=country, fill = country), position= 'dodge', alpha = 0,size = 1,stat = 'identity') +
  ggrepel::geom_text_repel(aes(x = call_num_grp, y = perccum, label = paste(perccum,'%'), group = ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD')),
                               color = stage(ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD')), after_scale = colorspace::darken(color, .4))), 
            fontface = 'bold',box.padding = .6, min.segment.length = 0) +
  # geom_text(aes(y = log(Calls)*1.1, x = call_num_grp, label = Calls, color = stage(country, after_scale = colorspace::darken(color, .4))), 
  #           fontface = 'bold', size = 2.8,vjust = 1) +
  xlab('Call attempt order') + 
  scale_fill_manual(values = dark2pal) +
  scale_color_manual(values = dark2pal) +
  scale_y_continuous(
    name = "Cumulative percentage of phone #s\n with completed CATI",
    breaks = seq(0,55, by = 5)
    # sec.axis = sec_axis( trans=~exp(.)/1.1, name="Phone calls made (log scale)", breaks = c(200,1000,10000))
    ) +
  labs(fill = 'Country',color = 'Country')+
  theme_minimal()+
  theme(legend.position = 'none')+#c(0.7, 0.55)) +
  coord_cartesian(ylim = c(0,52))
compbycall

### Phone calls made by call order
callsperorder <- ggplot(data = outcomes) + 
  geom_bar(aes(y = Calls, x = call_num_grp, group=ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD')),
               fill = ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD'))), position= 'dodge',alpha = 0.75,stat = 'identity') +
  # geom_bar(aes(y = Calls, x = call_num_grp, group=country, fill = country), position= 'dodge', alpha = 0,size = 1,stat = 'identity') +
  # ggrepel::geom_text_repel(aes(x = call_num_grp, y = perccum, label = paste(perccum,'%'), group = country, color = stage(country, after_scale = colorspace::darken(color, .4))), 
  #                          fontface = 'bold',box.padding = .6, min.segment.length = 0) +
  geom_text_repel(aes(y = Calls, x = call_num_grp, label = Calls, color = stage(ordered(country, levels = c('DRC','MW','BF','BF-EHCVM','BF-RDD')), 
                                                                                after_scale = colorspace::darken(color, .4))),
                  position = position_dodge(width = .9),
            fontface = 'bold', size = 2.8, min.segment.length = 0) +
  xlab('Call attempt order') + 
  scale_fill_manual(values = dark2pal) +
  scale_color_manual(values = dark2pal) +
  scale_y_continuous(trans='log10', name= "Phone calls made (log scale)", breaks = c(200,1000,10000)) +
  labs(fill = 'Country',color = 'Country') +
  theme_minimal()
callsperorder

library(patchwork)
(compbycall / callsperorder) + theme(legend.position = "bottom")

################################################################################
## Call attempt order updated plots
################################################################################

# Cumulative completed CATI plot
comprate <- ggplot(data = outcomes_rdd ) +
  geom_line(aes(x = call_num_grp, y = perccompletedpercallnum, group=ordered(country, levels = c('DRC','MW','BF-RDD')), 
                color = ordered(country, levels = c('DRC','MW','BF-RDD'))),
            size = 1, show.legend = FALSE) +
  xlab('Call attempt order') +
  ylab('% of completed CATIs per call num') +
  scale_color_manual(values = c("#1B9E77", "#D95F02","#7570B3"), labels = c("DRC", "MW", "BF")) +
  scale_y_continuous(breaks = seq(0,100, by = 5), limits = c(0,90))+
  labs( color = 'Country', linetype = 'Country') +
  theme_bw()+
  coord_cartesian(ylim = c(0,85)) 
comprate + theme(axis.text=element_text(size=15))
comprate

# Phone calls made
callsperorder_rr <- ggplot(data = outcomes_rdd) + 
  geom_bar(aes(y = Calls, x = call_num_grp, group=ordered(country, levels = c("DRC", "MW", "BF-RDD")),
               fill = ordered(country, levels = c("DRC", "MW", "BF-RDD"))), position= 'dodge',alpha = 0.75,stat = 'identity') +
  # geom_bar(aes(y = Calls, x = call_num_grp, group=country, fill = country), position= 'dodge', alpha = 0,size = 1,stat = 'identity') +
  # ggrepel::geom_text_repel(aes(x = call_num_grp, y = perccum, label = paste(perccum,'%'), group = country, color = stage(country, after_scale = colorspace::darken(color, .4))), 
  #                          fontface = 'bold',box.padding = .6, min.segment.length = 0) +
  xlab('Call attempt order') + 
  scale_fill_manual(values = dark2pal, labels = c("DRC", "MW", "BF")) +
  scale_color_manual(values = dark2pal, labels = c("DRC", "MW", "BF")) +
  scale_y_continuous(trans='log10', name= "Phone calls made (log scale)") +
  labs(fill = 'Country',color = 'Country') +
  theme_bw() 
callsperorder_rr + theme(axis.text=element_text(size=15)) # Change axis labels size
callsperorder_rr

# Combine the plots
library(patchwork)
(comprate / callsperorder_rr) + theme(legend.position = "bottom",
                                      legend.key.size = unit(1, 'cm'))

### Separated by country

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
library(cowplot)
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

# MW

# write.csv(outcomesmw,  paste0("/Users/vibui/Documents/rammps_data/", "outcomesmw-",Sys.Date(),".csv"))
# 
# comprate_mw <- ggplot(data = outcomesmw, aes(x = call_num_grp)) +
#   geom_bar(aes(y = Calls),   position= 'dodge',alpha = 0.75,stat = 'identity') +
#   geom_line(aes( y = perccompletedpercallnum, group=("country"),
#                 color = ("country")),
#             size = 1, show.legend = FALSE) +
#   scale_y_continuous(
#     
#     # Features of the first axis
#     trans='log10', name= "Phone calls made (log scale)",
#     
#     # Add a second axis and specify its features
#     sec.axis = sec_axis(~. *0.01 ,breaks = seq(0,100),  labels = scales::label_percent(), name="% of completed CATIs per call num"))
#    +
#   
#   xlab('Call attempt order') +
#   ylab('% of completed CATIs per call num') +
#   scale_color_manual(values = c("#1B9E77", "#D95F02","#7570B3"), labels = c("MW")) +
#   scale_y_continuous(breaks = seq(0,100, by = 5), limits = c(0,90))+
#   labs( color = 'Country', linetype = 'Country') +
#   theme_bw()+
#   coord_cartesian(ylim = c(0,85)) 
# comprate_mw + theme(axis.text=element_text(size=15))
# comprate_mw
# 
# 
# ###
# log(outcomesmw$Calls)
# Var2= c(4,16,39,2,22,39,41,10,3)
# log(Var2)
# outcomesmw$perccompletedpercallnum <- (outcomesmw$perccompletedpercallnum)/100
# 
# # Max of secondary divided by max of primary
# upper <- log10(3e6) / 60
# 
# breakfun <- function(x) {
#   10^scales::extended_breaks()(log10(x))
# }
# 
# ## https://stackoverflow.com/questions/71049931/how-to-make-log10-only-first-y-axis-not-secondary-y-axis-in-ggplot-in-r
# library(scales)
# comprate_mw <- ggplot(data = outcomesmw) +
#   geom_line(aes(x= call_num_grp, y = perccompletedpercallnum, group=("country"),
#                  color = ("country")),
#             size = 1, show.legend = FALSE) +
#   geom_bar(aes(x= call_num_grp ,y = log(Calls)),   position= 'dodge',alpha = 0.75,stat = 'identity') +
#   scale_y_continuous(
#     position = "right",
#     name = "Calls",
#     sec.axis = sec_axis(~10^ (. * upper), name= expression(paste("% of completed CATIs per call num")),
#                         breaks = breakfun))
#     
#   sec.axis = sec_axis(~.*10000,  name="% of completed CATIs per call num"))
#     
#     
#     
#     
#     # Features of the first axis
#     trans='log10', name= "% of completed CATIs per call num" ,
# 
#     # Add a second axis and specify its features
#    sec.axis = sec_axis(trans = c("log10", "reverse"),   name="Phone calls made (log scale)"))
# 
#   sec.axis = sec_axis(~.*-1, name = "wt", breaks = c(0,250, 500)))
# 
#     sec.axis = sec_axis(trans = ~exp(.) ,   name="% of completed CATIs per call num"))
# +
#   
#   xlab('Call attempt order') +
#   ylab('% of completed CATIs per call num') +
#   scale_color_manual(values = c("#1B9E77", "#D95F02","#7570B3"), labels = c("MW")) +
#   scale_y_continuous(breaks = seq(0,100, by = 5), limits = c(0,90))+
#   labs( color = 'Country', linetype = 'Country') +
#   theme_bw()+
#   coord_cartesian(ylim = c(0,85)) 
# comprate_mw + theme(axis.text=element_text(size=15))
# comprate_mw

callsperorder_mw <- ggplot(data = outcomesmw) + 
  geom_bar(aes(y = Calls, x = call_num_grp, group=ordered(country, levels = c("DRC", "MW", "BF-RDD")),
               fill = ordered(country, levels = c("DRC", "MW", "BF-RDD"))), position= 'dodge',alpha = 0.75,stat = 'identity') +
  # geom_bar(aes(y = Calls, x = call_num_grp, group=country, fill = country), position= 'dodge', alpha = 0,size = 1,stat = 'identity') +
  # ggrepel::geom_text_repel(aes(x = call_num_grp, y = perccum, label = paste(perccum,'%'), group = country, color = stage(country, after_scale = colorspace::darken(color, .4))), 
  #                          fontface = 'bold',box.padding = .6, min.segment.length = 0) +
  xlab('Call attempt order') + 
  scale_fill_manual(values = dark2pal, labels = c("DRC", "MW", "BF")) +
  scale_color_manual(values = dark2pal, labels = c("DRC", "MW", "BF")) +
  scale_y_continuous(trans='log10', name= "Phone calls made (log scale)") +
  labs(fill = 'Country',color = 'Country') +
  theme_bw() 
callsperorder_mw + theme(axis.text=element_text(size=15)) # Change axis labels size
callsperorder_mw
# Cumulative percentage of #s with completed CATI plot per country

# DRC
# cumulative_drc <- ggplot(data = subset(outcomes_rdd, country == 'DRC')) + 
#   geom_line(aes(x = call_num_grp, y = perccum, group=ordered(country, levels = 'DRC'), 
#                 color = '#D95F02'), size = 0.8) + 
#   geom_point(aes(x = call_num_grp, y = perccum, group=ordered(country, levels = 'DRC'),
#                  color = ordered(country, levels = 'DRC')), size = 2) +
#   geom_bar(aes(y = log(Calls), x = call_num_grp, group=ordered(country, levels = c('DRC')),
#                fill = '#1B9E77'), position= 'dodge',alpha = 0.75,stat = 'identity')+
#   xlab('Call attempt order') + 
#   scale_fill_manual(values = c('#1B9E77','#1B9E77')) +
#   scale_color_manual(values = c('#D95F02','#D95F02')) +
#   scale_y_continuous(
#     name = "Cumulative percentage of phone #s\n with completed CATI",
#     breaks = seq(0,30, by = 5),
#     sec.axis = sec_axis( trans=~exp(.), name="Phone calls made (log scale)", breaks = c(200,1000,10000))
#   ) +
#   labs(fill = 'Country',color = 'Country')+
#   theme(axis.title.y = element_text(color = '#D95F02', size=12),
#            axis.title.y.right = element_text(color = "#1B9E77", size=12))+
#   theme(legend.position = 'none')+#c(0.7, 0.55)) +
#   coord_cartesian(ylim = c(0,30))
# cumulative_drc

## Number of phone calls vs call attempt order
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
# cumulative_mw <- ggplot(data = subset(outcomes_rdd, country == 'MW')) + 
#   geom_line(aes(x = call_num_grp, y = perccum, group=ordered(country, levels = 'MW'), 
#                 color = '#D95F02'), size = 0.8) + 
#   geom_point(aes(x = call_num_grp, y = perccum, group=ordered(country, levels = 'NW'),
#                  color = ordered(country, levels = 'MW')), size = 2) +
#   geom_bar(aes(y = log(Calls), x = call_num_grp, group=ordered(country, levels = c('MW')),
#                fill = '#1B9E77'), position= 'dodge',alpha = 0.75,stat = 'identity')+
#   xlab('Call attempt order') + 
#   scale_fill_manual(values = c('#1B9E77','#1B9E77')) +
#   scale_color_manual(values = c('#D95F02','#D95F02')) +
#   scale_y_continuous(
#     name = "Cumulative percentage of phone #s\n with completed CATI",
#     breaks = seq(0,30, by = 5),
#     sec.axis = sec_axis( trans=~exp(.), name="Phone calls made (log scale)", breaks = c(200,1000,10000))
#   ) +
#   labs(fill = 'Country',color = 'Country')+
#   theme(axis.title.y = element_text(color = '#D95F02', size=12),
#         axis.title.y.right = element_text(color = "#1B9E77", size=12))+
#   theme(legend.position = 'none')+#c(0.7, 0.55)) +
#   coord_cartesian(ylim = c(0,30))
# cumulative_mw

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
# cumulative_bf <- ggplot(data = subset(outcomes_rdd, country == 'BF-RDD')) + 
#   geom_line(aes(x = call_num_grp, y = perccum, group=ordered(country, levels = 'BF-RDD'), 
#                 color = '#D95F02'), size = 0.8) + 
#   geom_point(aes(x = call_num_grp, y = perccum, group=ordered(country, levels = 'BF-RDD'),
#                  color = ordered(country, levels = 'BF-RDD')), size = 2) +
#   geom_bar(aes(y = log(Calls), x = call_num_grp, group=ordered(country, levels = c('BF-RDD')),
#                fill = '#1B9E77'), position= 'dodge',alpha = 0.75,stat = 'identity')+
#   xlab('Call attempt order') + 
#   scale_fill_manual(values = c('#1B9E77','#1B9E77')) +
#   scale_color_manual(values = c('#D95F02','#D95F02')) +
#   scale_y_continuous(
#     name = "Cumulative percentage of phone #s\n with completed CATI",
#     breaks = seq(0,30, by = 5),
#     sec.axis = sec_axis( trans=~exp(.), name="Phone calls made (log scale)", breaks = c(200,1000,10000))
#   ) +
#   labs(fill = 'Country',color = 'Country')+
#   theme(axis.title.y = element_text(color = '#D95F02', size=12),
#         axis.title.y.right = element_text(color = "#1B9E77", size=12))+
#   theme(legend.position = 'none')+#c(0.7, 0.55)) +
#   coord_cartesian(ylim = c(0,30))
# cumulative_bf

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

# cumulative_bf <- ggplot(data = subset(outcomes_rdd, country == 'BF-RDD')) + 
#   geom_line(aes(x = call_num_grp, y = perccum*max(outcomes_rdd$Calls), group=ordered(country, levels = 'BF-RDD')), size = 0.8) + 
#   geom_point(aes(x = call_num_grp, y = perccum*max(outcomes_rdd$Calls), group=ordered(country, levels = 'BF-RDD'),
#                  color = ordered(country, levels = 'BF-RDD')), size = 2) +
#   geom_bar(aes(y = (0.95*Calls), x = call_num_grp, group=ordered(country, levels = c('BF-RDD'))), position= 'dodge',alpha = 0.75,stat = 'identity')+
#   scale_y_continuous(
#     name = "Cumulative percentage of phone #s\n with completed CATI",
#     sec.axis = sec_axis(~./max(outcomes_rdd$Calls)))
# cumulative_bf 
# ### RIGHT ONEEE
# ratio <- max(outcomes_rdd$Calls)/max(outcomes_rdd$perccum)
# cumulative_bf <- ggplot(data = subset(outcomes_rdd, country == 'BF-RDD')) + 
#   geom_line(aes(x = call_num_grp, y = perccum*ratio, group=ordered(country, levels = 'BF-RDD'), color = '#D95F02'), size = 0.8) + 
#   geom_point(aes(x = call_num_grp, y =  perccum*ratio, group=ordered(country, levels = 'BF-RDD'),color = '#D95F02'), size = 2) +
#   geom_bar(aes(y = Calls, x = call_num_grp, group=ordered(country, levels = c('BF-RDD')), fill = '#1B9E77'), position= 'dodge',alpha = 0.75,stat = 'identity')+
#   xlab('Call attempt order') +
#   scale_fill_manual(values = c('#1B9E77','#1B9E77')) +
#   scale_color_manual(values = c('#D95F02','#D95F02')) +
#   theme(axis.title.y = element_text(color = '#D95F02', size=12),
#         axis.title.y.right = element_text(color = "#1B9E77", size=12))+
#   theme(legend.position = 'none')+#c(0.7, 0.55)) +
#   scale_y_continuous(
#     name = "Number of Phone Calls",
#     sec.axis = sec_axis((~ . / ratio), name ='Cumulative percentage of phone #s\n with completed CATI'))
# cumulative_bf 
#     
# 
# cumulative_bf  
#   xlab('Call attempt order') + 
#   scale_fill_manual(values = c('#1B9E77','#1B9E77')) +
#   scale_color_manual(values = c('#D95F02','#D95F02')) +
#   scale_y_continuous(
#     name = "Cumulative percentage of phone #s\n with completed CATI",
#     breaks = seq(0,30, by = 5),
#     sec.axis = sec_axis(~. *2000, name="Phone calls made (log scale)")
#   ) +
#   labs(fill = 'Country',color = 'Country')+
#   theme(axis.title.y = element_text(color = '#D95F02', size=12),
#         axis.title.y.right = element_text(color = "#1B9E77", size=12))+
#   theme(legend.position = 'none')+#c(0.7, 0.55)) +
#   coord_cartesian(ylim = c(0,30))
# cumulative_bf

# trying to combine the plots
# cumulative_bf <- ggplot(data = subset(outcomes_rdd, country == 'BF-RDD')) + 
#   geom_line(aes(x = call_num_grp, y = perccum, group=ordered(country, levels = 'BF-RDD'), 
#                 color = '#D95F02'), size = 0.8) + 
#   geom_col(aes(x = call_num_grp, y = (Calls/1000), group=ordered(country, levels = c('BF-RDD')),
#                fill = '#1B9E77'), position= 'dodge',alpha = 0.75)+
#   xlab('Call attempt order') + 
#   scale_fill_manual(values = c('#1B9E77','#1B9E77')) +
#   scale_color_manual(values = c('#D95F02','#D95F02')) +
#   scale_y_continuous(
#     name = "Phone calls made (log scale)",
#     sec.axis = sec_axis(~., name="Cumulative percentage of phone #s\n with completed CATI"))+
# 
#   labs(fill = 'Country',color = 'Country')+
#   theme(axis.title.y = element_text(color = '#1B9E77', size=12),
#         axis.title.y.right = element_text(color = "#D95F02", size=12))+
#   theme(legend.position = 'none')+#c(0.7, 0.55)) +
#   coord_cartesian(ylim = c(0,30))
# cumulative_bf


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

################################################################################
## Cumulative Phone Call Duration
################################################################################

# # MW
# phone_dur_mw <- dat.widemw %>%
#   mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
#   group_by(call_num_grp, Outcome.FINAL, phone_call_duration) %>%
#   summarize(n = n()) %>%
#   ungroup() %>%
#   filter(Outcome.FINAL == 'COMP') %>%
#   dplyr::mutate(totaldur = sum(n),
#                 dur_cum = cumsum(phone_call_duration))%>%
#   ungroup()
# # # Combining the phone_call_duration along with the call_num_grp so that call_num_grp
# # # is organised according to unique values.
# num <- aggregate(dur_cum~call_num_grp,phone_dur_mw,length)
# names(num)[2] <- 'num'
# # # Working out the total of phone_dur_drc
# totalB <- aggregate(dur_cum~call_num_grp,phone_dur_mw,sum)
# names(totalB)[2] <- 'totalB'
# # # Merging dfs
# phone_dur_mw_fin <- merge(num,totalB)
# phone_dur_mw_fin <- phone_dur_mw_fin %>%
#   mutate(csum = cumsum(totalB))
# #Plotting line graph
# ggplot(phone_dur_mw_fin) +
#   geom_point(aes(x=call_num_grp, y=csum)) +
#   geom_line(aes(x=call_num_grp, y=csum, group=1)) +
#   labs(x='Call Attempt Number', y='Cumulative Phone Call Duration')
# # # Plotting bar graph
# ggplot(phone_dur_mw_fin, aes(x=call_num_grp, y=csum)) +
#   geom_bar(stat='identity')+
#   labs(x='Call Attempt Number', y='Cumulative Phone Call Duration')

### think this one is correct
library(scales)
phonedur_mw <- dat.widemw %>%
  mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num))) %>%
  filter(Outcome.FINAL == 'COMP') %>%
  group_by(call_num_grp) %>%
  summarise(num = n(),
            totalB = sum((phone_call_duration/60)))
phonedur_mw_plot <- ggplot(phonedur_mw) +
  geom_point(aes(x=call_num_grp, y=totalB)) +
  geom_line(aes(x=call_num_grp, y=totalB, group=1)) +
  labs(x='Call Attempt Number', y='Phone Call Duration') +
  scale_y_continuous(limits = c(0, 100000),labels = label_comma()) +
  theme_bw() 
phonedur_mw_plot


# DRC
# # Creating variable for call numbers greater than 5
# phone_dur_drc <- dat.wide %>%
#    mutate(call_num_grp = ifelse(call_num > 5, '5+',as.character(call_num)))
# # Selecting the caseid and phone_call_duration from Consented
# select_drc <- Consented %>%
#   select(caseid, phone_call_duration)
# # Joining the phone_call_duration to the phone_dur_drc via caseid as the key
# phone_dur_drc <- left_join(phone_dur_drc, select_drc, by="caseid", na.rm=True)
# # phone_dur_drc <- phone_dur_drc[apply(phone_dur_drc!=0, 1, all),]
# 
# # Creating df with phone call duration
# phone_dur_drc <- phone_dur_drc %>%
#   group_by(call_num_grp, Outcome.FINAL, phone_call_duration) %>%
#   summarize(n = n()) %>%
#   ungroup() %>%
#   filter(Outcome.FINAL == 'COMP') %>%
#   dplyr::mutate(totaldur = sum(n),
#                 dur_cum = cumsum(phone_call_duration))%>%
#   ungroup()


phonedur_drc <- phone_dur_drc %>%
  group_by(call_num_grp) %>%
  summarise(num = n(),
            totalB = sum((phone_call_duration/60)))
phonedur_drc_plot <- ggplot(phonedur_drc) +
  geom_point(aes(x=call_num_grp, y=totalB)) +
  geom_line(aes(x=call_num_grp, y=totalB, group=1)) +
  labs(x='Call Attempt Number', y='Phone Call Duration') +
  scale_y_continuous(limits = c(0, 100000),labels = label_comma()) +
  theme_bw() 
phonedur_drc_plot

# BF
phone_dur_bf <- rdd.dat.wide


# rdd.data consists of phone_call_duration and call_num_grp (already have 5+) category
# Selecting phone_1, phone_call_duration and call_num_gr from Consented
select_bf <- rdd.data %>%
  select(phone_1, call_num_grp)
# select_bf <- select_bf[apply(select_bf!=0, 1, all),]

# Joining the phone_call_duration to the phone_dur_drc via caseid as the key
phone_dur_bf <- left_join(phone_dur_bf, select_bf, by="phone_1", na.rm=True)


# Creating df with phone call duration
# phone_dur_bf <- phone_dur_bf %>%
#   group_by(call_num_grp, Outcome.FINAL, phone_call_duration) %>%
#   summarize(n = n()) %>%
#   ungroup() %>%
#   filter(Outcome.FINAL == 'COMP') %>%
#   dplyr::mutate(totaldur = sum(n),
#                 dur_cum = cumsum(phone_call_duration))%>%
#   ungroup()

# Remove rows with zeros
# Some interviews were conducted outside the Survey CTO app (calls with duration=0) 
phone_dur_bf <- phone_dur_bf[apply(phone_dur_bf!=0, 1, all),]
phone_dur_bf <- phone_dur_bf %>%
  filter(complete.cases(.))

# change phone_call_duration to numeric in order to divide by 60
phone_dur_bf$phone_call_duration <- as.numeric(phone_dur_bf$phone_call_duration)
# subset data
phonedur_bf <- phone_dur_bf %>%
  filter(Outcome.FINAL == 'COMP') %>%
  group_by(call_num_grp) %>%
  summarise(num = n(),
            totalB = sum((phone_call_duration/60)))
# plot graph
phonedur_bf_plot <- ggplot(phonedur_bf) +
  geom_point(aes(x=call_num_grp, y=totalB)) +
  geom_line(aes(x=call_num_grp, y=totalB, group=1)) +
  labs(x='Call Attempt Number', y='Phone Call Duration') +
  scale_y_continuous(limits = c(0, 100000), labels = label_comma()) + #removes scentific notation on y-axis scale
  scale_fill_brewer(palette = "Pastel2")+
  theme_bw() 
phonedur_bf_plot

# arrange the three plots into 4 quadrants
plot_phone_dur <- plot_grid(phonedur_mw_plot + theme(legend.position="none", axis.text=element_text(size=12)), #remove legends first before combining legends
                phonedur_drc_plot+ theme(legend.position="none", axis.text=element_text(size=12)), 
                phonedur_bf_plot + theme(legend.position="none", axis.text=element_text(size=12)), 
                labels = c("MW", "DRC", "BF"),
                label_size = 9,
                hjust = -0.2, vjust = 1.9
)


# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(plot_phone_dur, ncol = 1, rel_heights = c(1, .1))


################################################################################
# CATI outcomes plot (overall and by trimester)
drccati <- dat.wide %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'DRC')
drccatibysource <- dat.wide %>%
  group_by(source, Outcome.FINAL) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1)) %>%
  dplyr::rename(group = source)
drccatibysourceprov <- dat.wide %>%
  group_by(Source.prov.1, Outcome.FINAL) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1)) %>%
  dplyr::rename(group = Source.prov.1)
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
ehcvmcati <- ehcvm.dat.wide %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'BF-EHCVM')
bfcati <- bf.dat.wide %>%
  group_by(Outcome.FINAL) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'BF')

# Change 'NNA or NR' to 'NNA/NR'
rddcati[rddcati == 'NNA or NR'] <- 'NNA/NR'

catis <- rbind(drccati, drccatibysource, drccatibysourceprov, mwcati, rddcati, ehcvmcati, bfcati) %>%
  # mutate(Eligibility = case_when(Outcome.FINAL %in% c('COMP','PART') ~ 'Eligible',
  #                                Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
  #                                Outcome.FINAL %in% c('REFU','NNA','NR','LANG','PEND', 'NNA/NR', 'Other', 'REASS') ~ 'Eligibility unknown'),
  #        Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
  #        Outcome.FINAL = factor(Outcome.FINAL, levels = c('COMP','PART','REFU','NNU','NNA','NNA/NR','NR','INEL','LANG','DEFER','PEND','REASS','Other')))

  # Outcome.FINAL = ifelse(Outcome.FINAL =='LANG'| Outcome.FINAL =='NNA' | Outcome.FINAL == 'NR' | Outcome.FINAL == 'REASS', 'CATI Other Eligibility Unknown', as.character(Outcome.FINAL)))

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
  # mutate(Outcome.FINAL = ifelse(is.na(Outcome.FINAL), 'CATI Other Eligibility Unknown', as.character(Outcome.FINAL)))
  # mutate(Outcome.Final = ifelse(is.na(Outcome.Final) & catis$Eligibility == 'CATI Other Eligibility Unknown', 'CATI Other Eligibility Unknown', as.character(Outcome.FINAL)))

write.csv(catis, "catis.csv")
library(plotly)

p <-ggplot(catis[catis$group=='MW'|catis$group=='DRC',]) +
  geom_bar(aes(fill=group, y=percperoutcome , x=Outcome.FINAL),position="dodge", stat="identity",alpha = 0.7) +
  # geom_bar(aes(color=group, y=percperoutcome , x=Outcome.FINAL),alpha = 0,size = 1,position="dodge", stat="identity" ) +
  geom_text(aes(x=Outcome.FINAL, y=percperoutcome , label=percperoutcome, color = stage(group, after_scale = colorspace::darken(color, .4))), 
                  size=3, vjust = -1, position = position_dodge(width = 1))+
  scale_fill_manual(values = dark2pal) +
  scale_color_manual(values = dark2pal) +
  theme_minimal() +
  labs(color = 'Country',fill = 'Country')+
  theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1, size = 9)) +
  xlab("")+ ylab("Percent of unique phone numbers")
ggplotly(p)

ggplot(catis[catis$group=='BF-RDD'|catis$group=='BF-EHCVM'|catis$group=='BF',]) +
  geom_bar(aes(fill=group, y=percperoutcome , x=Outcome.FINAL),position="dodge", stat="identity",alpha = 0.7) +
  # geom_bar(aes(color=group, y=percperoutcome , x=Outcome.FINAL),alpha = 0,size = 1,position="dodge", stat="identity" ) +
  geom_text(aes(x=Outcome.FINAL, y=percperoutcome , label=percperoutcome, color = stage(group, after_scale = colorspace::darken(color, .4))), 
            size=3, vjust = -1, position = position_dodge(width = 1))+
  scale_color_manual(values = c("#7570B3" ,"#E6AB02", "#66A61E")) +
  scale_fill_manual(values = c("#7570B3" ,"#E6AB02", "#66A61E")) +
  # scale_fill_brewer(palette = 'Dark2') +
  # scale_color_brewer(palette = 'Dark2') +
  theme_minimal() +
  labs(color = 'Country',fill = 'Country')+
  theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1, size = 9)) +
  xlab("")+ ylab("Percent of unique phone numbers")

################################################################################
## CATI Plots
################################################################################

par(mfrow=c(3,1))
library("ggsci")
library("scales")
library(RColorBrewer)

# Malawi
cati_plot_mw <- ggplot(catis[catis$group=='MW',], aes(y=percperoutcome , x=Outcome.FINAL, fill=Eligibility)) +
  geom_bar(stat="identity", width = 0.8) +
  labs(x="",y="Percent of unique phone numbers") +
  theme_light() +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(limits = c(0, 35)) +
  scale_fill_brewer(palette = "Pastel2")

  # geom_text(aes(x=Outcome.FINAL, y=percperoutcome , label=percperoutcome,), 
  #           size=3, vjust = -1, position = position_dodge(width = 1)) +
cati_plot_mw <- cati_plot_mw + guides(fill=guide_legend(title="CATI Outcome"))
# cati_plot_mw = cati_plot_mw + scale_fill_npg() #colour


# DRC
cati_plot_drc <- ggplot(catis[catis$group=='DRC',], aes(y=percperoutcome , x=Outcome.FINAL, fill=Eligibility)) +
  geom_bar(stat="identity") +
  labs(x="",y="Percent of unique phone numbers") +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(limits = c(0, 35)) +
  scale_fill_brewer(palette = "Pastel2")
  # geom_text(aes(x=Outcome.FINAL, y=percperoutcome , label=percperoutcome,), 
  #           size=3, vjust = -1, position = position_dodge(width = 1)) +
cati_plot_drc <- cati_plot_drc + guides(fill=guide_legend(title="CATI Outcome"))
# cati_plot_drc = cati_plot_drc + scale_fill_npg() #colour

# BF
cati_plot_bf <- ggplot(catis[catis$group=='BF-RDD',], aes(y=percperoutcome , x=Outcome.FINAL, fill=Eligibility)) +
  geom_bar(stat="identity") +
  labs(x="",y="Percent of unique phone numbers") +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(15)) +
  scale_y_continuous(limits = c(0, 35)) +
  scale_fill_brewer(palette = "Pastel2")
  # geom_text(aes(x=Outcome.FINAL, y=percperoutcome , label=percperoutcome,), 
  #           size=3, vjust = -1, position = position_dodge(width = 1)) +
  # scale_fill_hue(c = 40) #col+
cati_plot_bf <- cati_plot_bf + guides(fill=guide_legend(title="CATI Outcome"))
# cati_plot_bf = cati_plot_bf + scale_fill_npg() #colour

# Combining all the plots into a panel
# dir <- '/Users/vibui/Documents/rammps/'
# cati_plot_all <- ggarrange(cati_plot_mw, cati_plot_bf, cati_plot_drc + rremove("x.text"), 
#           labels = c("MW", "BF", "DRC"), ncol = 2, nrow = 2,
#           common.legend = TRUE, legend = "bottom", align = "v", vjust=0.5)

library(cowplot)
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

# Call outcomes
drccalls <- data %>%
  filter(!is.na(Outcome2)) %>%
  group_by(Outcome2) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'DRC')
drccallsbysource <- data %>%
  filter(!is.na(Outcome2)) %>%
  group_by(source, Outcome2) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1)) %>%
  dplyr::rename(group = source)
drccallsbysourceprov <- data %>%
  filter(!is.na(Outcome2)) %>%
  group_by(Source.prov, Outcome2) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1)) %>%
  dplyr::rename(group = Source.prov)
mwcalls <- datamw %>%
  filter(!is.na(Outcome2)) %>%
  group_by(Outcome2) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'MW')
rddcalls <- rdd.data %>%
  filter(!is.na(Outcome)) %>%
  group_by(Outcome) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'BF-RDD') %>%
  dplyr::rename(Outcome2 = Outcome)
ehcvmcalls <- ehcvm.data %>%
  filter(!is.na(Outcome)) %>%
  group_by(Outcome) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'BF-EHCVM')%>%
  dplyr::rename(Outcome2 = Outcome)
bfcalls <- bf.data %>%
  filter(!is.na(Outcome)) %>%
  group_by(Outcome) %>%
  dplyr::summarise(nperoutcome = n()) %>%
  mutate(totaln = sum(nperoutcome),
         percperoutcome = round(nperoutcome/totaln*100,1),
         group = 'BF')%>%
  dplyr::rename(Outcome2 = Outcome)

# rddcalls <- rddcalls %>% 
#   Outcome2 = case_when(Outcome2 == 'NNA or NR' ~ 'NNA/NR', TRUE ~ as.character(Outcome2))

# Change 'NNA or NR' to 'NNA/NR'
rddcalls[rddcalls == 'NNA or NR'] <- 'NNA/NR'

calls <- rbind(drccalls, drccallsbysource, drccallsbysourceprov, mwcalls, rddcalls, ehcvmcalls, bfcalls) %>%
  # mutate(Eligibility = case_when(Outcome2 %in% c('COMP', 'INCO') ~ 'Eligible',
  #                                Outcome2 %in% c('DEFER','NNU') ~ 'Ineligible',
  #                                Outcome2 %in% c('REFU','NNA','NR','NNA/NR') ~ 'Eligibility unknown',
  #                                Outcome2 %in% c('Other Ineligible') ~ 'Other Ineligible',
  #                                Outcome2 %in% c('Other Eligibility Unknown') ~ 'Other Eligibility Unknown'),
  #        Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown', 'Other Ineligible','Other Eligibility Unknown')),
  #        Outcome2 = factor(Outcome2, levels = c('COMP','REFU','INCO','NNU','NNA','NNA/NR','NR','DEFER', 'Other Ineligible','Other Eligibility Unknown')))

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
    # percperoutcome = case_when(Eligibility == 'Other Eligibility Unknown' ~ as.numeric(percperoutcome), TRUE ~ as.numeric(percperoutcome)))

write_csv(calls, "calls.csv")

ggplot(calls[calls$group=='MW'|calls$group=='DRC',]) +
geom_bar(aes(fill=group, y=percperoutcome , x=Outcome2),position="dodge", stat="identity",alpha = 0.7) +
# geom_bar(aes(color=group, y=percperoutcome , x=Outcome2),alpha = 0,size = 1,position="dodge", stat="identity" ) +
geom_text(aes(x=Outcome2, y=percperoutcome , label=percperoutcome, color = stage(group, after_scale = colorspace::darken(color, .4))), 
              size=3,position = position_dodge(width = 1), vjust = -1)+
scale_fill_manual(values = dark2pal) +
scale_color_manual(values = dark2pal) +
labs(color = 'Country',fill = 'Country')+
theme_minimal() +
theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1, size = 9)) +
xlab("")+ ylab("Percent of phone calls")


ggplot(calls[calls$group=='BF-RDD'|calls$group=='BF-EHCVM'| calls$group=='BF',]) +
  geom_bar(aes(fill=group, y=percperoutcome , x=Outcome2),position="dodge", stat="identity",alpha = 0.7) +
  # geom_bar(aes(color=group, y=percperoutcome , x=Outcome2),alpha = 0,size = 1,position="dodge", stat="identity" ) +
  geom_text(aes(x=Outcome2, y=percperoutcome , label=percperoutcome, color = stage(group, after_scale = colorspace::darken(color, .4))), 
            size=3,position = position_dodge(width = 1), vjust = -1)+
  scale_color_manual(values = c("#7570B3" ,"#E6AB02", "#66A61E")) +
  scale_fill_manual(values = c("#7570B3" ,"#E6AB02", "#66A61E")) +
  labs(color = 'Country',fill = 'Country')+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1, size = 9)) +
  xlab("")+ ylab("Percent of phone calls")

################################################################################
## Phone Call Plots
################################################################################

#MW
call_plot_mw <- ggplot(calls[calls$group=='MW',], aes(y=percperoutcome , x=Outcome2, fill=Eligibility)) +
  geom_bar(stat="identity") +
  labs(x="",y="Percent of phone calls") +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(15)) + #limits no characters on x-axis to 20
  # coord_flip() + #swap axes
  scale_y_continuous(limits = c(0, 80)) + #limits scale of y-axis to 0-80 
  scale_fill_brewer(palette = "Pastel2")
  # geom_text(aes(x=Outcome2, y=percperoutcome , label=percperoutcome,), 
  #           size=3, vjust = -1, position = position_dodge(width = 1)) +
call_plot_mw <- call_plot_mw + guides(fill=guide_legend(title="Call Outcome")) #Title for legend
# call_plot_mw = call_plot_mw + scale_fill_igv() #colour

#DRC
call_plot_drc <- ggplot(calls[calls$group=='DRC',], aes(y=percperoutcome , x=Outcome2, fill=Eligibility)) +
  geom_bar(stat="identity") +
  labs(x="",y="Percent of phone calls") +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(15)) +
  # coord_flip() +
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_brewer(palette = "Pastel2")
  # geom_text(aes(x=Outcome2, y=percperoutcome , label=percperoutcome,), 
  #           size=3, vjust = -1, position = position_dodge(width = 1)) +
call_plot_drc <- call_plot_drc + guides(fill=guide_legend(title="Call Outcome"))
# call_plot_drc = call_plot_drc + scale_fill_igv() #colour

#BF
call_plot_bf <- ggplot(calls[calls$group=='BF-RDD',], aes(y=percperoutcome , x=Outcome2, fill=Eligibility)) +
  geom_bar(stat="identity") +
  labs(x="",y="Percent of phone calls") +
  theme_bw() +
  scale_x_discrete(labels = label_wrap(15)) +
  # coord_flip() +
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_brewer(palette = "Pastel2")

  # geom_text(aes(x=Outcome2, y=percperoutcome , label=percperoutcome,), 
  #           size=3, vjust = -1, position = position_dodge(width = 1)) +
call_plot_bf <- call_plot_bf + guides(fill=guide_legend(title="Call Outcome"))
# call_plot_bf = call_plot_bf + scale_fill_igv() #colour

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

################################################################################
# Phone call duration 
################################################################################

## creating datasets grouped by month and enumerator (and province/etc if relevant) of median phone duration by enumerator per month
drcdur <- Consented %>%
  dplyr::group_by(month.interview, enumerator) %>%
  dplyr::summarise(dur = mean(phone_call_duration)) %>%
  mutate(group = 'DRC')
mwdur <- Consentedmw %>%
  group_by(month.interview, enumerator) %>%
  dplyr::summarise(dur = mean(as.numeric(phone_call_duration))) %>%
  dplyr::mutate(group = 'MW')
rdddur <- Consentedrdd %>%
  mutate(phone_call_duration = as.integer(phone_call_duration)) %>%
  group_by(month.interview) %>%
  dplyr::summarise(dur = mean(phone_call_duration)) %>%
  dplyr::mutate(group = 'BF-RDD') 

# removes 0 value call durations
rdddur <- rdddur[apply(rdddur!=0, 1, all),]


'Dec-21' %in% Consentedmw$month.interview
durations <- rbind(drcdur, mwdur, rdddur) %>%
  mutate(month.interview = factor(month.interview, levels = c('Aug-21','Sep-21','Oct-21',
                                                              'Nov-21','Dec-21','Jan-22',
                                                              'Feb-22','Mar-22','Apr-22', 
                                                              'May-22','Jun-22', 'Jul-22',
                                                            'Aug-22','Sep-22', 'Oct-22', 'Nov-22', 'Dec-22')))
'Sep-21' %in% durations$month.interview
# bfdurations <- rbind(rdddur, ehcvmdur, bfdur)%>%
#   mutate(month.interview = factor(month.interview, levels = c('Aug-21','Sep-21','Oct-21',
#                                                               'Nov-21','Dec-21','Jan-22',
#                                                               'Feb-22','Mar-22','Apr-22', 
#                                                               'May-22','Jun-22', 'Jul-22',
#                                                               'Aug-22','Sep-22')))
# 

ggplot(durations, aes(x = month.interview, y = dur/60, fill = group)) +
  geom_bar(stat = 'identity', position = "dodge") +
  scale_fill_manual(values = dark2pal) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = 'Month', y = 'Phone call duration (min)', fill = '')

ggplot(durations) +
  geom_violin(aes(x = month.interview, y = dur/60, fill = group)) +
  facet_wrap(vars(group)) +
  scale_fill_manual(values = dark2pal) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = 'Month', y = 'Phone call duration (min)', fill = '')

ggplot(bfdurations) + 
  geom_line(aes(x = month.interview, y = dur/60, group = group,color = group), size =1) +
  geom_point(aes(x = month.interview, y = dur/60, group = group,color = group), size =1) +
  scale_color_manual(values = c("#7570B3" ,"#E6AB02", "#66A61E")) +
  theme_minimal() +
  # ylim(0, 600) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = 'Month', y = 'Phone call duration (min)', color = '')

# ## Phone duration per module

# MW
introlanguage_duration <- datamw %>%
  select(introlanguage_duration_1/60) %>%
  drop_na(introlanguage_duration_1) %>%
  mutate(introlanguage_duration = as.character("Introduction")) %>%
  mutate(introlanguage_duration_1 = as.numeric(introlanguage_duration_1))
str(introlanguage_duration)

# get truncated pregnancy duration
# only consider women who have given birth or who have become pregnant before
truncpreg_dur <- Consentedmw %>%
  filter(Resp.Sex == "Female") %>%
  filter(ph2_1 == 1 | ph7_1 == 1 | ph10_1 == 1 | ph12_1 == 1 ) %>%
  mutate(trunpreg_dur = truncpregnancy_duration_1/60) %>% # convert seconds to minutes
  select(caseid, trunpreg_dur) %>%
  drop_na(trunpreg_dur)

# get full pregnancy duration
# only consider women who have given birth or who have become pregnant before
fullpreg_dur <- Consentedmw %>%
  filter(Resp.Sex == "Female") %>%
  filter(fph201_1 == 1 | fph206_1 == 1 | fph210_1 == 1) %>%
  mutate(fullpreg_dur = fullpregnancy_duration_1/60) %>%
  select(caseid, fullpreg_dur) %>%
  drop_na(fullpreg_dur)

# get all the other module durations
mw_module_dur <- Consentedmw %>%
  filter(Resp.Sex == "Female") %>%
  mutate(intro_dur = introlanguage_duration_1/60,
         elig_dur = elegibility_duration_1/60,
         consent_dur = consent_duration_1/60,
         arran_dur = arrangements_duration_1/60,
         back_dur = background_duration_1/60,
         vac_dur = vaccination_duration_1/60,
         hh_dur = household_duration_1/60,
         parent_dur = parents_duration_1/60,
         # trunpreg_dur = truncpregnancy_duration_1/60,
         # fullpreg_dur = fullpregnancy_duration_1/60,
         sib_dur = siblings_duration_1/60,
         debrif_dur = debriefing_duration_1/60) %>%
  drop_na(intro_dur, 
          elig_dur, 
          consent_dur, 
          arran_dur, 
          back_dur,
          vac_dur,
          hh_dur,
          parent_dur,
          # trunpreg_dur,
          # fullpreg_dur,
          sib_dur,
          debrif_dur) %>%
  select(caseid, 
         intro_dur, 
         elig_dur, 
         consent_dur, 
         arran_dur, 
         back_dur,
         vac_dur,
         hh_dur,
         parent_dur,
         # trunpreg_dur,
         # fullpreg_dur,
         sib_dur,
         debrif_dur)

# Join truncpreg_dur with other module durations by using caseid as ID
yas <- left_join(mw_module_dur, fullpreg_dur,  by="caseid")
# Join truncpreg_dur with all module durations
yas <- left_join(yas, truncpreg_dur, by="caseid")
# Remove caseid from df
yes2 <- yas %>% select(-caseid)

# Rewrite the column names
colnames(yes2) <- c('Introduction','Eligibility','Consent', 'Arrangment', 'Background',
                    'Vaccination', 'Household', 'Parental survival',  'Summary sibling histories',
                    'Debrief', 'Full pregnancy histories', 'Truncated pregnancy history')
# Convert df
mw_module_dur_fin <- yes2 %>%
  gather(Module, Time)
# Order the bars based on the order in the df
modules_ordered <- c('Introduction','Eligibility','Consent', 'Arrangment', 'Background',
                     'Vaccination', 'Household', 'Parental survival',  'Truncated pregnancy history'
                     , 'Full pregnancy histories','Summary sibling histories',
                     'Debrief')

# Create boxplot
ggplot(data = mw_module_dur_fin, aes(x=Module, y=Time)) +
  # geom_bar(stat='summary') +
  geom_boxplot() +
  labs(y = "Time (mins)") +
  theme(text = element_text(size=15)) +
  scale_x_discrete(limits = modules_ordered, labels = label_wrap(15)) +
  theme_bw()


# Consentedmw %>%
#   filter(Resp.Sex == "Female") %>%
#   # filter(!(ph2_1 == 1 & ph7_1 == 1 & ph10_1 == 1 & ph12_1 == 1)) %>%
#   # filter(!(fph201_1 == 1 & fph206_1 == 1 & fph210_1 == 1)) %>%
#   filter(ph2_1 == 1 | ph7_1 == 1 | ph10_1 == 1 | ph12_1 == 1 ) %>%
#   # filter(fph201_1 == 1 | fph206_1 == 1 | fph210_1 == 1) %>%
#   group_by(truncpregnancy_duration_1) %>%
#   dplyr::summarise(n=n()) %>%
#   View()
# 
# # get the truncated pregnancy duration
# truncpreg_dur <- Consentedmw %>%
#   filter(Resp.Sex == "Female") %>%
#   filter(ph2_1 == 1 | ph7_1 == 1 | ph10_1 == 1 | ph12_1 == 1 ) %>%
#   mutate(trunpreg_dur = truncpregnancy_duration_1/60) %>%
#   select(caseid, trunpreg_dur) %>%
#   drop_na(trunpreg_dur)
# str(truncpreg_dur)
# # convert df to list to add to df with other times
# truncpreg_dur_list <- as.numeric(unlist(truncpreg_dur))
# 
# # get full pregnancy duration
# fullpreg_dur <- Consentedmw %>%
#   filter(Resp.Sex == "Female") %>%
#   filter(fph201_1 == 1 | fph206_1 == 1 | fph210_1 == 1) %>%
#   mutate(fullpreg_dur = fullpregnancy_duration_1/60) %>%
#   select(caseid, fullpreg_dur) %>%
#   drop_na(fullpreg_dur)
# # convert df to list to add to df with other times
# fullpreg_dur_list <- as.numeric(unlist(fullpreg_dur))
# length(fullpreg_dur_list) <- length(mw_module_dur)
# str(truncpreg_dur_list)
# length(truncpreg_dur_list) <- length(mw_module_dur)
# 
# # combine dfs
# long_list <- cbind(mw_module_dur,truncpreg_dur_list, fullpreg_dur_list)
# as.numeric()
# 
# yas <- left_join(mw_module_dur, fullpreg_dur,  by="caseid")
# yas <- left_join(yas, truncpreg_dur, by="caseid")
# 
# yes2 <- yas %>% select(-caseid)
# ########################
# ## trucated + pregnancy
# mw_module_dur_fin <- yes2 %>%
#   gather(Module, Time)
# # Order the bars based on the order in the df
# modules_ordered <- c('Introduction','Eligibility','Consent', 'Arrangment', 'Background',
#                      'Vaccination', 'Household', 'Parental survival',  'Summary sibling histories',
#                      'Debrief', 'Truncated pregnancy history', 'Full pregnancy histories')
# module_dur_mw <- ggplot(data = mw_module_dur_fin, aes(x=Module, y=Time)) +
#   # geom_bar(stat='summary') +
#   stat_summary(fun = "mean", geom = "bar") + # find the mean of the duration
#   labs(y = "Time (mins)") +
#   theme(text = element_text(size=15)) +
#   # scale_x_discrete(limits = modules_ordered, labels = label_wrap(15)) +
#   theme_bw()
# module_dur_mw
# 
# # create boxplot
# ggplot(data = mw_module_dur_fin, aes(x=Module, y=Time)) +
#   # geom_bar(stat='summary') +
#   geom_boxplot() +
#   labs(y = "Time (mins)") +
#   theme(text = element_text(size=15)) +
#   # scale_x_discrete(limits = modules_ordered, labels = label_wrap(15)) +
#   theme_bw()
# 
# str(mw_module_dur)
# mw_module_filtered <- Consentedmw[(Consentedmw$fph201_1 == 1 | Consentedmw$fph206_1 == 1 | Consentedmw$fph210_1 == 1) & (Consentedmw$ph2_1 == 1 | Consentedmw$ph7_1 == 1 & Consentedmw$ph10_1 == 1 & Consentedmw$ph12_1 == 1),]
# unique(mw_module_filtered$fph206_1)
# mw_module_dur <- Consentedmw %>%
#   filter(Resp.Sex == "Female") %>%
#   # filter for only women who have been pregnant
#   # filter(ph2_1 == 1 | ph7_1 == 1 | ph10_1 == 1 | ph12_1 == 1 | fph201_1 == 1 | fph206_1 == 1 | fph210_1 == 1) %>%
#   # filter(!(fph201_1 == 1 & fph206_1 == 1 & fph210_1 == 1)) %>%
#   # filter(fph201_1 == 1 | fph206_1 == 1 | fph210_1 == 1) %>%
#   # filter(ph2_1 == 1 | ph7_1 == 1 | ph10_1 == 1 | ph12_1 == 1) %>%
#   # filter(ph2_1 == 1 | fph201_1 == 1) %>%
#   mutate(intro_dur = introlanguage_duration_1/60,
#          elig_dur = elegibility_duration_1/60,
#          consent_dur = consent_duration_1/60,
#          arran_dur = arrangements_duration_1/60,
#          back_dur = background_duration_1/60,
#          vac_dur = vaccination_duration_1/60,
#          hh_dur = household_duration_1/60,
#          parent_dur = parents_duration_1/60,
#          # trunpreg_dur = truncpregnancy_duration_1/60,
#          # fullpreg_dur = fullpregnancy_duration_1/60,
#          sib_dur = siblings_duration_1/60,
#          debrif_dur = debriefing_duration_1/60) %>%
#   drop_na(intro_dur, 
#           elig_dur, 
#           consent_dur, 
#           arran_dur, 
#           back_dur,
#           vac_dur,
#           hh_dur,
#           parent_dur,
#           # trunpreg_dur,
#           # fullpreg_dur,
#           sib_dur,
#           debrif_dur) %>%
#   select(intro_dur, 
#          elig_dur, 
#          consent_dur, 
#          arran_dur, 
#          back_dur,
#          vac_dur,
#          hh_dur,
#          parent_dur,
#          # trunpreg_dur,
#          # fullpreg_dur,
#          sib_dur,
#          debrif_dur)
# 
# colnames(mw_module_dur) <- c('Introduction','Eligibility','Consent', 'Arrangment', 'Background',
#                  'Vaccination', 'Household', 'Parental survival',  'Truncated pregnancy history'
#                  , 'Full pregnancy histories','Summary sibling histories',
#                  'Debrief')
# mw_module_dur_fin <- mw_module_dur %>%
#   gather(Module, Time)
# # Order the bars based on the order in the df
# modules_ordered <- c('Introduction','Eligibility','Consent', 'Arrangment', 'Background',
#                      'Vaccination', 'Household', 'Parental survival',  'Truncated pregnancy history'
#                      , 'Full pregnancy histories','Summary sibling histories',
#                      'Debrief')
# module_dur_mw <- ggplot(data = mw_module_dur_fin, aes(x=Module, y=Time)) +
#   # geom_bar(stat='summary') +
#   stat_summary(fun = "mean", geom = "bar") + # find the mean of the duration
#   labs(y = "Time (mins)") +
#   theme(text = element_text(size=15)) +
#   scale_x_discrete(limits = modules_ordered, labels = label_wrap(15)) +
#   theme_bw()
# module_dur_mw
# 
# # create boxplot
# ggplot(data = mw_module_dur_fin, aes(x=Module, y=Time)) +
#   # geom_bar(stat='summary') +
#   geom_boxplot() +
#   labs(y = "Time (mins)") +
#   theme(text = element_text(size=15)) +
#   scale_x_discrete(limits = modules_ordered, labels = label_wrap(15)) +
#   theme_bw()

# DRC
drc_duration <- Consented %>%
  select(Phone.duration/60, Elig.Time, Consent.Time, Arrangement.Time, Background.Time, C19.Time,
         HH.Time, PS.Time, SSH.Time, DB.Time) %>%
  mutate(Elig.Time = mean(Elig.Time),
         Consent.Time = mean(Consent.Time),
         Arrangement.Time = mean(Arrangement.Time),
         Background.Time = mean(Background.Time),
         C19.Time = mean(C19.Time),
         HH.Time = mean(HH.Time),
         PS.Time = mean(PS.Time),
         SSH.Time = mean(SSH.Time),
         DB.Time = mean(DB.Time, na.rm=TRUE))


Elig.Time = mean(Consented$Elig.Time/60)
Consent.Time = mean(Consented$Consent.Time/60)
Arrangement.Time = mean(Consented$Arrangement.Time/60)
Background.Time = mean(Consented$Background.Time/60)
C19.Time = mean(Consented$C19.Time/60)
HH.Time = mean(Consented$HH.Time/60)
PS.Time = mean(Consented$PS.Time/60)
SSH.Time = mean(Consented$SSH.Time/60)
DB.Time = mean((Consented$DB.Time)/60, na.rm=TRUE)

get <-((Consented$DB.Time)/60)
summary(get)
sort(get, decreasing = TRUE)

# First column of df
drc_duration_1 <- c('Eligibility', 'Consent', 'Arrangement', 'Background','Covid-19', 'Household', 'Parental survival', 'Summary sibling histories', 'Debrief')

# Second colum of df
drc_duration_2 <- c(Elig.Time, Consent.Time, Arrangement.Time, Background.Time,C19.Time, HH.Time, PS.Time, SSH.Time, DB.Time)

# Create df
drc_duration_comb <- data.frame(drc_duration_1, drc_duration_2)
# Order the bars based on the order in the df
drc_duration_comb$drc_duration_1 <- factor(drc_duration_comb$drc_duration_1, levels=drc_duration_comb$drc_duration_1)

# Create plots for DRC
module_dur_drc <- ggplot(data=drc_duration_comb, aes(x=drc_duration_1, y=drc_duration_2)) +
  geom_bar(stat = 'identity') +
  labs(x='Module', y='Time (mins)') +
  theme_bw() + labs(fill='Module') +
  # scale_fill_brewer(palette = "Paired")+ 
  # theme(text = element_text(size=15)) +
  scale_x_discrete(labels = label_wrap(15)) 
module_dur_drc

# Combine the module durations for MW and DRC
module_dur <- plot_grid(module_dur_mw + theme(legend.position="none", axis.text=element_text(size=6)), #remove legends first before combining legends
                        module_dur_drc+ theme(legend.position="none", axis.text=element_text(size=7)),
                        labels = c("MW", "DRC"), #Title for each graph
                        label_size = 9
)
module_dur

DRC_pcd <- (Consented$phone_call_duration)/60
summary(DRC_pcd)
sort(DRC_pcd, decreasing = TRUE)
# MW
# Select the data from the Consentedmw df
response_sex_mw <- Consentedmw %>%
  select(Phone.duration, Resp.Sex, Resp.Age.pyr)

# Change tibble to df
response_sex_mw<- as.data.frame(response_sex_mw)
# Change Phone.duration from character to numeric
response_sex_mw$Phone.duration <- as.numeric(Consentedmw$Phone.duration)
# View structure to check change
str(response_sex_mw)

#Create plot
response_sex_mw_plot <- ggplot(data=response_sex_mw, aes(x=Resp.Age.pyr, y=Phone.duration/60, fill=Resp.Sex)) +
  geom_bar(stat = 'identity', position = "dodge") +
  labs(x='', y='Cumulative Phone Duration (hours)') +
  scale_y_continuous(limits = c(0, 300))

response_sex_mw_plot<- response_sex_mw_plot + scale_x_discrete(breaks=c("(14,19]",
                                                  "(19,29]",
                                                  "(29,39]",
                                                  "(39,49]",
                                                  "(49,59]",
                                                  "(59,64]"),
                                         labels=c("15-19", "20-29", "30-39", "40-49", "50-59", "60-64"))
response_sex_mw_plot <- response_sex_mw_plot + guides(fill=guide_legend(title="Respondent Sex")) #Title for legend
# DRC
response_sex_drc <- Consented %>%
  select(Phone.duration, Resp.Sex, Resp.Age.pyr)

response_sex_drc_plot <- ggplot(data=response_sex_drc, aes(x=Resp.Age.pyr, y=Phone.duration/60, fill=Resp.Sex)) +
  geom_bar(stat = 'identity', position = "dodge") +
  labs(x='', y='Cumulative Phone Duration (hours)')+
  scale_y_continuous(limits = c(0, 300))

response_sex_drc_plot <- response_sex_drc_plot + scale_x_discrete(breaks=c("(14,19]",
                                                  "(19,29]",
                                                  "(29,39]",
                                                  "(39,49]",
                                                  "(49,59]",
                                                  "(59,64]"),
                                         labels=c("15-19", "20-29", "30-39", "40-49", "50-59", "60-64"))
# BF
response_sex_bf <- Consentedrdd %>%
  select(phone_call_duration, gender, age)

response_sex_bf <- response_sex_bf[apply(response_sex_bf!=0, 1, all),]

# Change tibble to df
response_sex_bf<- as.data.frame(response_sex_bf)
# Change Phone.duration from character to numeric
response_sex_bf$phone_call_duration <- as.numeric(response_sex_bf$phone_call_duration)
response_sex_bf$gender <- as.character(response_sex_bf$gender)
response_sex_bf$age <- as.numeric(response_sex_bf$age)


class(response_sex_bf$age)



response_sex_bf <- response_sex_bf %>%
  mutate(gender = case_when(gender==1 ~ "Male",
                            gender==2 ~ "Female",
                            is.na(gender)~ "Male")) %>%
  mutate(agegroup = case_when(age <= 19 ~ "15-19",
                              age > 19 & age <=29 ~ "20-29",
                              age > 29 & age <=39 ~ "30-39",
                              age > 39 & age <=49 ~ "40-49",
                              age > 49 & age <=59 ~ "50-59",
                              age > 59 ~ "60-64"))

table(response_sex_bf$agegroup)

str(response_sex_bf)
response_sex_bf_plot <- ggplot(data=response_sex_bf, aes(x=agegroup, y=phone_call_duration/60, fill=gender)) +
  geom_bar(stat = 'identity', position = "dodge") +
  labs(x='', y='Cumulative Phone Duration (hours)') +
  scale_y_continuous(limits = c(0, 300))
  

response_sex_bf_plot + scale_x_discrete(breaks=c("14,19",
                                                  "19,29",
                                                  "29,39",
                                                  "39,49",
                                                  "49,59",
                                                  "59,64"),
                                         labels=c("15-19", "20-29", "30-39", "40-49", "50-59", "60-64"))


# Combine the plots 
resp_plot_comb <- plot_grid(response_sex_mw_plot + theme(legend.position="none"), #remove legends first before combining legends
                            response_sex_drc_plot+ theme(legend.position="none"),
                            response_sex_bf_plot + theme(legend.position="none"), 
                labels = c("MW", "DRC", "BF"), #Title for each graph
                label_size = 10
)

# extract the legend from Malawi plots
legend_call_resp <- get_legend(
  response_sex_mw_plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.6, 5.15))
)
# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(resp_plot_comb, legend_call_resp, ncol = 1, rel_heights = c(1, .1))


###############################################################################
## Random workings out

Counts <- aggregate(data.frame(count = Consentedmw$Date.Interview),
                    by= list(value = Consentedmw$Date.Interview,
                             enumerator = Consentedmw$enumerator,
                             woman=Consentedmw$F.ReproAge),length) %>%
  group_by(enumerator,woman) %>%
  mutate(csum = cumsum(count)) %>% as.data.frame() 

Consentedmw$Time <- format(as.POSIXct(Consentedmw$starttime), format = "%H:%M:%S") 

Consentedmw$Time <- as.numeric(as.character(Consentedmw$Time)) 
dat

tidyr::separate(Consentedmw$starttime, datetime, c("date", "time"), sep = " ")

# Date time is the date/time variable for end time of call 
new_df <- separate(data = Consentedmw, col = DateTime, into  = c('Date', 'Time'), sep = ' ')
str(new_df$Time)

str(Consentedmw$starttime)
Counts <- aggregate(data.frame(count = Consentedmw$starttime),
                    by=list())

# Trying to work out the 

Consentedmw$StartTime <- as.POSIXct(mwdata_raw$starttime, format = "%b %d, %Y %r")
Consentedmw$StartTime

# Convert datetime string variable to datetime variable
# 
mdy(Consentedmw$StartTime)

####### MW
Consentedmw$Time
# Convert from 'Jan 31, 2022 8:50:52 PM' format to '2022-01-31 08:37:31' format
Consentedmw<- Consentedmw %>%
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))

# Create df with just the info needed
time_mw <- Consentedmw %>%
  select(Time, phone_call_duration, starttime)

time_count_mw <- aggregate(data.frame(count = Consentedmw$Date.Interview),
                           by=list(Time = Consentedmw$Time,
                                   phone_call_duration = Consentedmw$phone_call_duration,
                                   starttime = Consentedmw$Date.Interview,
                                   enumerator = Consentedmw$enumerator), length)  %>%
  group_by(enumerator) %>%
  mutate(csum = cumsum(count)) %>% as.data.frame() 

Counts <- aggregate(data.frame(count = Consentedmw$Date.Interview),
                    by= list(value = Consentedmw$Date.Interview,
                             enumerator = Consentedmw$enumerator),length) %>%
  group_by(enumerator) %>%
  mutate(csum = cumsum(count)) %>% as.data.frame() 



# # don't think need this
# # transform into date time column (if it is not already one)
# df$starttime<- ymd_hms(df$starttime)

# Creating the time categories
# create breaks
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")
# Creating new column with the time categories
time_mw$Time_of_day <- cut(x=hour(time_mw$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
time_mw

time_count_mw$Time_of_day <- cut(x=hour(time_count_mw$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)

# ggplot(data=time_mw, aes(x=Time, y=phone_call_duration)) +
#   geom_bar(stat='identity')

# All the call duration added up
ggplot(data=time_mw, aes(x=Time_of_day, y=phone_call_duration/60)) +
  geom_bar(stat='identity')

# Boxplot with morning evening ect
phone_dur_day_mw <- ggplot(data = time_mw) +
  geom_boxplot(aes(x = Time_of_day, y = phone_call_duration/60)) + 
  scale_y_continuous(name = 'Phone call duration') +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02')) + 
  theme_bw() + xlab('')+
  # theme(legend.position="none") + 
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#D95F02","#7570B3"))#"#1B9E77", 
phone_dur_day_mw

# Violin plot
ggplot(data = time_mw) +
  geom_violin(aes(x = Time_of_day, y = phone_call_duration/60)) +
  scale_fill_manual(values = dark2pal) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = 'Month', y = 'Phone call duration (min)', fill = '')


# second plot

ggplot(data = time_count_mw) +
  geom_boxplot(aes(x = Time_of_day, y = count)) + 
  scale_y_continuous(name = 'Daily Completed interviews') +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02')) + 
  theme_minimal() + xlab('Enumerator')+
  # theme(legend.position="none") + 
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#D95F02","#7570B3"))#"#1B9E77", 

###### DRC
# Convert from 'Jan 31, 2022 8:50:52 PM' format to '2022-01-31 08:37:31' format
Consented<- Consented %>%
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))

# Create df with just the info needed
time_drc <- Consented %>%
  select(Time, phone_call_duration, starttime)

# Creating the time categories
# create breaks
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")
# Creating new column with the time categories
time_drc$Time_of_day <- cut(x=hour(time_drc$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
time_drc
# Boxplot with morning evening ect
phone_dur_day_drc <- ggplot(data = time_drc) +
  geom_boxplot(aes(x = Time_of_day, y = phone_call_duration/60)) + 
  scale_y_continuous(name = 'Phone call duration', limits = c(0, 300)) +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02')) + 
  theme_bw() + xlab('')+
  # theme(legend.position="none") + 
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#D95F02","#7570B3"))#"#1B9E77", 
phone_dur_day_drc

### BF
#Split date and time into different columns
Consentedrdd$Date<- as.Date(Consentedrdd$instance_time) 
Consentedrdd$Time <- format(as.POSIXct(Consentedrdd$instance_time), format = "%H:%M:%S") 

# Create df with just the info needed
# Change phone_call_duration into number 
Consentedrdd$phone_call_duration <- as.numeric(Consentedrdd$phone_call_duration)
time_bf <- Consentedrdd %>%
  select(Time, phone_call_duration, instance_time)

# Remove rows with zeros
# Some interviews were conducted outside the Survey CTO app (calls with duration=0) 
time_bf <- time_bf[apply(time_bf!=0, 1, all),]

# Creating the time categories
# create breaks
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")
# Creating new column with the time categories
time_bf$Time_of_day <- cut(x=hour(time_bf$instance_time), breaks = breaks, labels = labels, include.lowest=TRUE)
time_bf
# Boxplot with morning evening ect
phone_dur_day_bf <- ggplot(data = time_bf) +
  geom_boxplot(aes(x = Time_of_day, y = phone_call_duration/60)) + 
  scale_y_continuous(name = 'Phone call duration', limits = c(0, 300)) +
  scale_x_discrete() +
  theme(axis.title.y = element_text(color = '#D95F02')) + 
  theme_bw() + xlab('')+
  # theme(legend.position="none") + 
  labs(fill = "Respondent characteristics")+
  scale_fill_manual(values = c("#D95F02","#7570B3"))#"#1B9E77", 
phone_dur_day_bf

Consentedrdd$phone_call_duration
str(time_bf)
test <-Consentedrdd$Outcome
str(test)

# Combine the plots 
phone_dur_day <- plot_grid(phone_dur_day_mw + theme(legend.position="none"), #remove legends first before combining legends
                            phone_dur_day_drc+ theme(legend.position="none"),
                            phone_dur_day_bf + theme(legend.position="none"), 
                            labels = c("MW", "DRC", "BF"), #Title for each graph
                            label_size = 10
)

# extract the legend from Malawi plots
legend_ohone_dur <- get_legend(
  phone_dur_day_mw + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.6, 5.15))
)
# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(phone_dur_day, legend_ohone_dur, ncol = 1, rel_heights = c(1, .1))

str(data$Outcome2)
str(Consented$Outcome2)

################################################################################
## Identifying technical difficulties and respondent feedback
################################################################################

# MW

# Technical difficulties
tech_diff <- Consentedmw %>%
  select(tech_difficulties) %>%
  filter(complete.cases(.)) %>%
  mutate(total = n()) %>%
  group_by(tech_difficulties) %>%
  mutate(sum = n(),
         percentages = (sum/total)*100)


ggplot(tech_diff, aes(x = tech_difficulties, y = sum)) +
  geom_bar( stat = 'identity')

aggregate(cbind(tech_diff$percentages, tech_diff$sum), list(tech_diff$tech_difficulties), mean)


# Respondent cooperation
resp_cooperation <- Consentedmw %>%
  select(resp_coop) %>%
  filter(complete.cases(.)) %>%
  mutate(total = n()) %>%
  group_by(resp_coop) %>%
  mutate(sum = n(),
         percentages = (sum/total)*100) 

ggplot(resp_cooperation, aes(x = resp_coop, y = percentages)) +
  geom_bar( stat = 'identity')

aggregate(cbind(resp_cooperation$percentages, resp_cooperation$sum), list(resp_cooperation$resp_coop), mean)

# DRC

# Technical difficulties
Consented <- Consented %>% 
  mutate(
    tech_difficulties = case_when(call_feedback == 1 ~ 'No difficulties',
                                  call_feedback == 2 ~ 'Background noise affected my understanding',
                                  call_feedback == 3 ~ 'Poor call quality/reception',
                                  call_feedback == 4 ~ 'Difficult communication in the chosen language',
                                  call_feedback == 5 ~ 'Other'),
    resp_coop = case_when(Respondent_cooperation == 1 ~ 'Good',
                          Respondent_cooperation == 2 ~ 'OK, but the respondent sounded hesitant, shy, or irritated',
                          Respondent_cooperation == 3 ~ 'OK, but the respondent was distracted during the interview',
                          Respondent_cooperation == 4 ~ 'Respondent was not cooperative',
                          Respondent_cooperation == 5 ~ 'Other'))
  
comments_mw <- unique(Consentedmw$comments)
grep("short", comments_mw, value = TRUE) 

# Technical difficulti
tech_diff_drc <- Consented %>%
  select(tech_difficulties) %>%
  filter(complete.cases(.)) %>%
  mutate(total = n()) %>%
  group_by(tech_difficulties) %>%
  mutate(sum = n(),
         percentages = (sum/total)*100)

ggplot(tech_diff_drc, aes(x = tech_difficulties, y = sum)) +
  geom_bar( stat = 'identity')

tech_diff_drc %>%
  group_by(tech_difficulties) %>%
  summarise(sum = n())


aggregate(cbind(tech_diff_drc$percentages, tech_diff_drc$sum), list(tech_diff_drc$resp_coop), mean)

# Respondent cooperation
# All NAs for the DRC
resp_cooperation_drc <- Consented %>%
  select(resp_coop) %>%
  filter(complete.cases(.)) %>%
  mutate(total = n()) %>%
  group_by(resp_coop) %>%
  mutate(sum = n(),
         percentages = (sum/total)*100) 

ggplot(resp_cooperation_drc, aes(x = resp_coop, y = percentages)) +
  geom_bar( stat = 'identity')

resp_cooperation_drc %>%
  mutate(total = n()) %>%
  group_by(resp_coop) %>%
  dplyr::summarise(sum = n())

aggregate(cbind(resp_cooperation_drc$percentages, resp_cooperation_drc$sum), list(resp_cooperation_drc$resp_coop), mean)

# Cyclone
cyclone <- Consentedmw %>%
  filter(Resp.Region == "Southern" | Resp.Region == "Central") %>% # get southern and central regions
  separate(DateTime, into = c('date', 'time'), sep=' ', remove = FALSE) %>% # split datetime into separate columns
  filter(date < '2023-02-01') 

# number of unique caseids
length(unique(cyclone$caseid))

# find number who reported moderate level of stress
# 2 = moderately upset
cyclone %>%
  group_by(d5_1) %>%
  summarise(n=n())

unique(cyclone$Outcome2)

# 18417 answered
sum(age$n)


 

