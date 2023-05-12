#_______________________________________________________________________________
#  Metadata analyses
#  This script contains analyses on metdata based on the questionnaire
#  05/2023
#_______________________________________________________________________________

# Load data and packages from response_rates.R

# Response Rate Calculations based on sex --------------------------------------

# DRC
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
rddbf_female <- rdd.dat.wide%>%
  filter(gender == "Female") %>%
  group_by(Outcome.FINAL) %>%
  tally()
rddbf_male <- rdd.dat.wide %>%
  filter(gender == "Male") %>%
  group_by(Outcome.FINAL) %>%
  tally()

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


# Total CATIs completed --------------------------------------------------------

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
# MW
mwcatis <- nrow(Consentedmw) # total MW completed 
mwcatis_female <- Consentedmw %>%
  filter(Resp.Sex == "Female") %>%
  dplyr::summarise(sum = n())
mwcatis_male <- Consentedmw %>%
  filter(Resp.Sex == "Male") %>%
  dplyr::summarise(sum = n())
(mwcatis_male/(mwcatis_female+mwcatis_male))*100
(mwcatis_female/(mwcatis_female+mwcatis_male))*100

# BF
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

# CATI Completion rates by call order plot based on sex ------------------------
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

# Plot figures
# Ratio for second x-axis
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


# Average Phone Call Duration based on sex -------------------------------------

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


# Age and sex distribution -----------------------------------------------------

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


# Residence and sex distribution -----------------------------------------------

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

# Identifying technical difficulties and respondent feedback -------------------

# MW
# Technical difficulties
tech_diff <- Consentedmw %>%
  select(tech_difficulties) %>%
  filter(complete.cases(.)) %>%
  mutate(total = n()) %>%
  group_by(tech_difficulties) %>%
  mutate(sum = n(),
         percentages = (sum/total)*100)
aggregate(cbind(tech_diff$percentages, tech_diff$sum), list(tech_diff$tech_difficulties), mean)


# # Respondent cooperation
# resp_cooperation <- Consentedmw %>%
#   select(resp_coop) %>%
#   filter(complete.cases(.)) %>%
#   mutate(total = n()) %>%
#   group_by(resp_coop) %>%
#   mutate(sum = n(),
#          percentages = (sum/total)*100) 
# aggregate(cbind(resp_cooperation$percentages, resp_cooperation$sum), list(resp_cooperation$resp_coop), mean)

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

tech_diff_drc <- Consented %>%
  select(tech_difficulties) %>%
  filter(complete.cases(.)) %>%
  mutate(total = n()) %>%
  group_by(tech_difficulties) %>%
  mutate(sum = n(),
         percentages = (sum/total)*100)

tech_diff_drc %>%
  group_by(tech_difficulties) %>%
  summarise(sum = n())

# # Respondent cooperation
# # All NAs for the DRC
# resp_cooperation_drc <- Consented %>%
#   select(resp_coop) %>%
#   filter(complete.cases(.)) %>%
#   mutate(total = n()) %>%
#   group_by(resp_coop) %>%
#   mutate(sum = n(),
#          percentages = (sum/total)*100) 
# 
# resp_cooperation_drc %>%
#   mutate(total = n()) %>%
#   group_by(resp_coop) %>%
#   dplyr::summarise(sum = n())

# Respondent cooperation -------------------------------------------------------

# For these feedback questions, Resp.Sex can be changed to RuralUrban and vice 
# versa to analyse sex or place of residence.
# Resp.Sex: options are "Male" and "Female"
# RuralUrban: options are "Rural" and "Urban"

# MW
resp_cooperation <- Consentedmw %>%
  group_by(resp_coop) %>%
  drop_na(resp_coop) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100) 

resp_cooperation_sex <- Consentedmw %>% # for RuralUrban change Resp.Sex to RuralUrban
  filter(Resp.Sex=="Male") %>% # Change 'Male' to 'Female' to get 'Female'
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

# Rural/Urban or Resp.Sex
Consented %>% # for Resp.Sex change RuralUrban to Resp.Sex
  filter(RuralUrban=="Rural") %>% # Change 'Rural' to 'Urban' to get 'Urban'
  mutate(Respondent_cooperation = case_when(Respondent_cooperation == 1 ~ 'Good',
                                            Respondent_cooperation == 2 ~ 'hesitant',
                                            Respondent_cooperation == 3 ~ 'distracted',
                                            Respondent_cooperation == 4 ~ 'not cooperative')) %>%
  group_by(Respondent_cooperation) %>%
  drop_na(Respondent_cooperation) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n/sum(n)*100) 

# How upset were you from these questions? -------------------------------------
Consentedmw %>%
  group_by(d5_1) %>%
  drop_na(d5_1) %>%
  mutate(d5_1 = case_when(d5_1 == 1 ~ "Slightly upset",
                          d5_1 == 2 ~ "Moderately upset",
                          d5_1 == 3 ~ "Very upset")) %>%
  dplyr::summarise(n = n())%>%
  mutate(freq = n/sum(n)*100)

upset_sex <- Consentedmw %>% # for RuralUrban change Resp.Sex to RuralUrban
  filter(Resp.Sex=="Male") %>% # Change 'Male' to 'Female' to get 'Female'
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

# Where respondent is taking their call? ---------------------------------------
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

# Are the respondents on the speaker? ------------------------------------------
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


# Phone ownership --------------------------------------------------------------

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

# Phone call duration based on module ------------------------------------------

# MW
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
module_join <- left_join(mw_module_dur, fullpreg_dur,  by="caseid")
# Join truncpreg_dur with all module durations
module_join <- left_join(module_join, truncpreg_dur, by="caseid")
# Remove caseid from df
module_join_comp <- module_join %>% select(-caseid)

# Rewrite the column names
colnames(module_join_comp) <- c('Introduction','Eligibility','Consent', 'Arrangment', 'Background',
                    'Vaccination', 'Household', 'Parental survival',  'Summary sibling histories',
                    'Debrief', 'Full pregnancy histories', 'Truncated pregnancy history')
# Convert df
mw_module_dur_fin <- module_join_comp %>%
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

# DRC

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



