# RR paper
pkgs <- c('purrr','plyr','tidyverse', 'plotrix','lubridate','kableExtra','hrbrthemes','ggplot2','extrafont','float','reshape',
          'gridExtra','rsvg','png','devtools','readxl','date', 'ggpubr', 'tidyselect', 'httr', 'jsonlite', 'extrafont', 'colorspace',
          'ggrepel', 'forcats')
lapply(pkgs, require, character.only = TRUE)

dark2pal <- c("#1B9E77", "#D95F02", "#7570B3", "#66A61E", "#E6AB02")
# DRC
r.functions.dir <- 'C:/Users/KellyMcCain/London School of Hygiene and Tropical Medicine/rammps/'
# r.functions.dir <- 'C:/Users/Kelly McCain/Documents/LSHTM/rammps/'
dir.inputdata <- paste0(Sys.getenv('USERPROFILE'),"/OneDrive - London School of Hygiene and Tropical Medicine/Documents/General/Partners/UNIKIN/SurveyCTO Audits/")
# dir.inputdata <- "C:/Users/Kelly McCain/London School of Hygiene and Tropical Medicine/RAMMPS_LSHTM_Group - Documents/General/Partners/UNIKIN/SurveyCTO Audits/"

Consented <- read.csv(paste0(dir.inputdata, "Clean data/Consented2022-08-23.csv"))%>%
  mutate(source = ifelse(grepl('Fer', Source.prov),'Feroxus',
                         ifelse(grepl('IVR', Source.prov),'IVR',NA)),
         Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2)))
data <- read.csv(paste0(dir.inputdata, "Clean data/Data_Long2022-08-23.csv"))%>%
  mutate(source = ifelse(grepl('Fer', Source.prov),'Feroxus',
                         ifelse(grepl('IVR', Source.prov),'IVR',NA)),
         Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2))) 
dat.wide <- read.csv(paste0(dir.inputdata, "Clean data/Data_Wide2022-08-23.csv")) %>%
  mutate(source = ifelse(grepl('Fer',Source.prov.1),'Feroxus',
                         ifelse(grepl('IVR',Source.prov.1),'IVR', NA)),
         Eligibility = case_when(Outcome.FINAL %in% c('COMP','PART') ~ 'Eligible',
                                 Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                 Outcome.FINAL %in% c('REFU','NNA','NR','LANG','PEND') ~ 'Eligibility unknown'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
         Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)))

  # Malawi
Consentedmw <- read.csv(paste0(r.functions.dir, 'RaMMPS_MWApp/ConsentedMW.csv')) %>%
  mutate(Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2)))
dat.widemw <- read.csv(paste0(r.functions.dir,'RaMMPS_MWApp/DatWide.csv'))%>%
  mutate(Eligibility = case_when(Outcome.FINAL %in% c('COMP','PART') ~ 'Eligible',
                                 Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                 Outcome.FINAL %in% c('REFU','NNA','NR','LANG','PEND') ~ 'Eligibility unknown'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
         Outcome.FINAL = ifelse(Outcome.FINAL =='NNA'| Outcome.FINAL =='NR', 'NNA/NR', as.character(Outcome.FINAL)))
datamw <- read.csv(paste0(r.functions.dir,'RaMMPS_MWApp/DatLongMW.csv')) %>%
  # filter(call_num %in% c(1:5)) %>%
  dplyr::filter(!is.na(Outcome2)) %>%
  mutate(Outcome2 = ifelse(Outcome2 =='NNA'| Outcome2 =='NR', 'NNA/NR', as.character(Outcome2)))


  # Burkina Faso 
dir.input <- "C:/Users/KellyMcCain/OneDrive - London School of Hygiene and Tropical Medicine/Documents/General/Partners/Burkina/Survey Data/"
# dir.input <- "C:/Users/Kelly McCain/London School of Hygiene and Tropical Medicine/RAMMPS_LSHTM_Group - Documents/General/Partners/Burkina/Survey Data/"
# eval(parse(paste0(r.functions.dir,'CleaningScript_func_BF.R'), encoding="UTF-8"))
listbf <- clean_bf(dir.input)

rdd.data <- listbf[[1]]
ehcvm.data <- listbf[[2]]
rdd.dat.wide <- listbf[[3]]
ehcvm.dat.wide <- listbf[[4]]
bf.dat.wide <- listbf[[5]]
bf.data <- listbf[[6]]
Consentedrdd <- rdd.data %>%
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


########################################################################
########################################################################
########################################################################

## Calculation of response rates
# response rate 1 COMP / [COMP+ PART+ REFU + NR (5 attempts) + NNA (5attempts)+LANG]
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

allmw <- dat.widemw %>%
  group_by(Outcome.FINAL) %>%
  tally()

rddbf <- rdd.dat.wide %>%
  group_by(Outcome.FINAL) %>%
  tally()
ehcvmbf <- ehcvm.dat.wide %>%
  group_by(Outcome.FINAL) %>%
  tally()
allbf <- bf.dat.wide %>%
  group_by(Outcome.FINAL) %>%
  tally()


dfs <- list('ferkin'=ferkin,'fernk'=fernk,  'ivrkin'=ivrkin,  'ivrnk'=ivrnk,
            #'kin' = kin, 'nk'= nk,'northmw'=northmw, 'centralmw' = centralmw, 'southmw' = southmw
            'alldrc'=alldrc, 'fer'=fer, 'ivr'=ivr, 'allmw' = allmw, 'rddbf' = rddbf, 'ehcvmbf' = ehcvmbf, 'allbf' = allbf)
list_names <- names(dfs)

# to get estimation of e (percent eligible)
# OLD DEF: INEL / (INEL + COMP + PART + LANG + PEND(?) + DEFER)
# ELIG/ (ELIG+INEL) = (COMP + PART + DEFER) / ([INEL + NNU] + [COMP + PART + DEFER])
e_est <- function(df){
  elig <- round(sum(df[df$Outcome.FINAL=='COMP',]$n, df[df$Outcome.FINAL=='PART',]$n, df[df$Outcome.FINAL=='DEFER',]$n)  / 
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
            'refr10s'=refr10s) #'refr2s_0.2'=refr2s_0.2, 'refr2s_0.3'=refr2s_0.3, 'refr2s_0.5'=refr2s_0.5, 'refr2s_0.8'=refr2s_0.8)
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
                      'RefR1_0')
colnames(rrs_df) <- names(dfs)

write.csv(rrs_df, paste0('C://Users/KellyMcCain/Downloads/rrs_df', Sys.Date(),'.csv'))

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
#MW
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

outcomesdrc$Calls <- c(table(data$call_num_grp))
outcomesmw$Calls <- c(table(datamw$call_num_grp))
outcomesrdd$Calls <- c(table(rdd.data$call_num_grp))
outcomesehcvm$Calls <- c(table(ehcvm.data$call_num_grp))
outcomesbf$Calls <- c(table(bf.data$call_num_grp))
outcomes <- rbind(outcomesdrc, outcomesmw, outcomesrdd, outcomesehcvm, outcomesbf) %>%
  mutate(country = forcats::fct_inorder(country))



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

catis <- rbind(drccati, drccatibysource, drccatibysourceprov, mwcati, rddcati, ehcvmcati, bfcati) %>%
  mutate(Eligibility = case_when(Outcome.FINAL %in% c('COMP','PART') ~ 'Eligible',
                                 Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                 Outcome.FINAL %in% c('REFU','NNA','NR','LANG','PEND') ~ 'Eligibility unknown'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
         Outcome.FINAL = factor(Outcome.FINAL, levels = c('COMP','PART','REFU','NNU','NNA','NNA/NR','NR','INEL','LANG','DEFER','PEND','REASS','Other')))

ggplot(catis[catis$group=='MW'|catis$group=='DRC',]) +
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

calls <- rbind(drccalls, drccallsbysource, drccallsbysourceprov, mwcalls, rddcalls, ehcvmcalls, bfcalls) %>%
  mutate(Eligibility = case_when(Outcome2 %in% c('COMP', 'INCO') ~ 'Eligible',
                                 Outcome2 %in% c('INEL','DEFER','NNU','REFER') ~ 'Ineligible',
                                 Outcome2 %in% c('REFU','NNA','NR','LANG','REASS','INCOR','NNA/NR','Other') ~ 'Eligibility unknown'),
         Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
         Outcome2 = factor(Outcome2, levels = c('COMP','REFU','INCO', 'INCOR','NNU','NNA','NNA/NR','NR','INEL','LANG','REASS','REFER','DEFER','Other')))

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


# Phone call duration 

## creating datasets grouped by month and enumerator (and province/etc if relevant) of median phone duration by enumerator per month
drcdur <- Consented %>%
  dplyr::group_by(month.interview, enumerator) %>%
  dplyr::summarise(dur = median(phone_call_duration)) %>%
  mutate(group = 'DRC')
drcdurbysource <- Consented %>%
  group_by(source, month.interview, enumerator) %>%
  dplyr::summarise(dur = median(phone_call_duration)) %>%
  dplyr::rename(group = source) %>%
  dplyr::mutate(group = paste0("DRC - ", group))
drcdurbysourceprov <- Consented %>%
  group_by(Source.prov, month.interview, enumerator) %>%
  dplyr::summarise(dur = median(phone_call_duration)) %>%
  dplyr::rename(group = Source.prov)%>%
  dplyr::mutate(group = paste0("DRC - ", group))
mwdur <- Consentedmw %>%
  group_by(month.interview, enumerator) %>%
  dplyr::summarise(dur = median(phone_call_duration)) %>%
  dplyr::mutate(group = 'MW')
rdddur <- Consentedrdd %>%
  mutate(phone_call_duration = as.integer(phone_call_duration)) %>%
  group_by(month.interview) %>%
  dplyr::summarise(dur = median(phone_call_duration)) %>%
  dplyr::mutate(group = 'BF-RDD') 
ehcvmdur <- Consentedehcvm %>%
  mutate(phone_call_duration = as.integer(phone_call_duration)) %>%
  group_by(month.interview) %>%
  dplyr::summarise(dur = median(phone_call_duration)) %>%
  dplyr::mutate(group = 'BF-EHCVM')
bfdur <- Consentedbf %>%
  mutate(phone_call_duration = as.integer(phone_call_duration)) %>%
  group_by(month.interview) %>%
  dplyr::summarise(dur = median(phone_call_duration)) %>%
  dplyr::mutate(group = 'BF')

durations <- rbind(drcdur, drcdurbysource, mwdur) %>%
  mutate(month.interview = factor(month.interview, levels = c('Aug-21','Sep-21','Oct-21',
                                                              'Nov-21','Dec-21','Jan-22',
                                                              'Feb-22','Mar-22','Apr-22', 
                                                              'May-22','Jun-22', 'Jul-22',
                                                              'Aug-22','Sep-22')))
bfdurations <- rbind(rdddur, ehcvmdur, bfdur)%>%
  mutate(month.interview = factor(month.interview, levels = c('Aug-21','Sep-21','Oct-21',
                                                              'Nov-21','Dec-21','Jan-22',
                                                              'Feb-22','Mar-22','Apr-22', 
                                                              'May-22','Jun-22', 'Jul-22',
                                                              'Aug-22','Sep-22')))

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
