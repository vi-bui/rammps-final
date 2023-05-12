dur0 <- data %>%
  filter(phone_call_duration==0& Resp.Consent==1 & Outcome2=='COMP') %>%
  select(c(phone_1, endtime, starttime, formdef_version, deviceid,enumerator, phone.call.log, Date.Interview, phone_call_duration, Total,Elig.Time,Consent.Time,Arrangement.Time,
                     Background.Time,C19.Time,HH.Time,PS.Time,SSH.Time,DB.Time,Phone.duration,D1,Outcome2,Other.Den.Reasons, Resp.Region, Resp.Sex)) %>%
  mutate(othervax = ifelse(Other.Den.Reasons=='', 'Other','No other') )
 
durmorethan0 <- data %>%
  filter(phone_call_duration!=0& Resp.Consent==1 & Outcome2=='COMP') %>%
  select(c(caseid, phone_1,  endtime, starttime, formdef_version,phone.call.log,deviceid,enumerator, endtime, Date.Interview, phone_call_duration, Total,Elig.Time,Consent.Time,Arrangement.Time,
           Background.Time,C19.Time,HH.Time,PS.Time,SSH.Time,DB.Time,Phone.duration,D1,Outcome2,Other.Den.Reasons, Resp.Region, Resp.Sex))%>%
  mutate(othervax = ifelse(Other.Den.Reasons=='', 'Other','No other') )
  

duration0 <- data_raw %>% select(c(phone_1, deviceid, device_info,starttime, endtime,phone.call.log,users,phone_call_duration,duration,caseid,phone_1,call_num, consent, call_status, elig_time, 
                                   consent_time, arrangements_time,
                                   background_time, vax_time, HH_time, PS_time, SSH_time, DB_time, audio_audit)) %>%
  filter(phone_call_duration==0& consent==1 & call_status==1) 

durationmorethan0 <- data_raw %>% select(c(phone_1,deviceid,starttime, endtime,phone.call.log, users,phone_call_duration,duration,caseid,phone_1,call_num, consent, call_status, elig_time, 
                                   consent_time, arrangements_time,
                                   background_time, vax_time, HH_time, PS_time, SSH_time, DB_time, audio_audit,other_waterSource)) %>%
  filter(phone_call_duration>0& consent==1 & call_status==1)
duration0$Date.Interview <- gsub('.{11}$', '', duration0$endtime)

duration0 <- duration0 %>%
  mutate(Date.Interview1 = as.POSIXct(Date.Interview, format="%h %d, %Y"),
         Date.Interview = format(Date.Interview1, "%Y-%m-%d"),
         Date.Interview = as.Date(Date.Interview)
  ) %>%
  dplyr::select(-Date.Interview1)

dates <- as.Date(c('2021-09-28','2021-09-29','2021-10-16','2021-12-22','2021-','2021-02-07'))
dates_vline <- which(duration0$Date.Interview ==as.Date('2021-09-28')|
                                      #Date.Interview ==as.Date('2021-09-29')|
                                     # Date.Interview ==as.Date('2021-10-16')|
                                     # Date.Interview ==as.Date('2021-12-22')|
                       duration0$Date.Interview ==as.Date('2022-01-20')|
                       duration0$Date.Interview ==as.Date('2022-02-07')
                                      ) #%>% select(Date.Interview) %>% as.vector()
ggplot(duration0) + 
  geom_histogram(aes(Date.Interview), color = 'black', fill = 'grey60',alpha = 0.7, bins =60) +
  # geom_vline(xintercept = as.numeric(as.POSIXct('2021-09-28')), color = 'red')
  geom_vline(xintercept = as.numeric(duration0$Date.Interview[dates_vline]), col = 'red')
#6 on 9-28, none 9-29, none on 10-16, none on 12-22, 8 on 1-20, 53 on 2-07

#######
# is there a pattern with day and enumerators? 
usersdate <- table(duration0$users, duration0$Date.Interview) %>% 
  rbind() %>% 
  as.data.frame() %>% 
  adorn_totals("row")%>% 
  adorn_totals('col')
# drc.5580@pma2020.org did the most at 361:
drc5580 <- duration0 %>%
  filter(users == 'drc.5580@pma2020.org') %>% select(c(Date.Interview, everything()))
alldrc5580 <- data_raw %>%
  mutate(Date.Interview = gsub('.{11}$', '', endtime),
         Date.Interview1 = as.POSIXct(Date.Interview, format="%h %d, %Y"),
         Date.Interview = format(Date.Interview1, "%Y-%m-%d"),
         Date.Interview = as.Date(Date.Interview) ) %>%
  dplyr::select(-Date.Interview1) %>%
  filter(users == 'drc.5580@pma2020.org' & call_status == '1') %>% select(c(deviceid,Date.Interview,starttime, endtime,users,phone_call_duration,caseid,phone_1,call_num, consent, call_status,duration, elig_time, 
                                          consent_time, arrangements_time,
                                          background_time, vax_time, HH_time, PS_time, SSH_time, DB_time, audio_audit))
#######

durationmorethan0 <- data_raw %>% select(c(KEY,endtime, phone_1, users,  consent, call_status,phone_call_duration,duration, elig_time, consent_time, arrangements_time,
                                   background_time, vax_time, HH_time, PS_time, SSH_time, DB_time, audio_audit,other_waterSource)) %>%
  filter(phone_call_duration!=0& consent==1 & call_status==1)

# auditgood <- durationmorethan0 %>% filter(audio_audit!='')
# write.csv(auditgood, 'C://Users/KellyMcCain/Downloads/audits_durationmorethan0.csv')

drcdata_raw <- pull_data('cati_lang_reassign', 'rammps', 'kelly.mccain@lshtm.ac.uk', 'rammpS!CTOsurvey')
data_raw <- drcdata_raw
testing <- data_raw %>% select(c(call_respondent_1,endtime,caseid,phone_1,phone_2,e14_b, call_num, users,  consent, call_status,phone_call_duration,duration, elig_time, consent_time, arrangements_time,background_time, vax_time, HH_time, PS_time, SSH_time, DB_time, audio_audit,other_waterSource)) %>%
         filter(users=='kelly.mccain@lshtm.ac.uk')

ggplot() +
  geom_histogram(data = dur0[dur0$Background.Time<400,], aes(x = Background.Time/60), alpha = 0.5, fill = 'red', color = 'black') +
  geom_vline(xintercept = median(dur0$Background.Time/60, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$Background.Time<400,], aes(x = Background.Time/60), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0$Background.Time/60, na.rm = T), color = 'blue')
ggplot() +
  geom_histogram(data = dur0[dur0$Consent.Time<30,], aes(x = Consent.Time/60), alpha = 0.5, fill = 'red', color = 'black') + 
  geom_vline(xintercept = median(dur0$Consent.Time/60, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$Consent.Time<30,], aes(x = Consent.Time/60), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0$Consent.Time/60, na.rm = T), color = 'blue')
ggplot() +
  geom_histogram(data = dur0[dur0$Total<3000,], aes(x = Total/60), alpha = 0.5, fill = 'red', color = 'black') + 
  geom_vline(xintercept = median(dur0$Total/60, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$Total<3000,], aes(x = Total/60), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0$Total/60, na.rm = T), color = 'blue')+
  xlab('Temps total passe dans le questionnaire')+ylab("Nombre d'entretiens")+labs(caption = "Bleu = duration de l'appel>0; Rouge = duration de l'appel=0")
ggplot() +
  geom_histogram(data = dur0[dur0$Elig.Time<400,], aes(x = Elig.Time), alpha = 0.5, fill = 'red', color = 'black') + 
  geom_vline(xintercept = median(dur0$Elig.Time, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$Elig.Time<400,], aes(x = Elig.Time), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0$Elig.Time, na.rm = T), color = 'blue')
ggplot() +
  geom_histogram(data = dur0[dur0$Arrangement.Time<50,], aes(x = Arrangement.Time), alpha = 0.5, fill = 'red', color = 'black') + 
  geom_vline(xintercept = median(dur0$Arrangement.Time, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$Arrangement.Time<50,], aes(x = Arrangement.Time), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0$Arrangement.Time, na.rm = T), color = 'blue')
ggplot() +
  geom_histogram(data = dur0[dur0$C19.Time<150,], aes(x = C19.Time), alpha = 0.5, fill = 'red', color = 'black') + 
  geom_vline(xintercept = median(dur0$C19.Time, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$C19.Time<150,], aes(x = C19.Time), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0$C19.Time, na.rm = T), color = 'blue')
ggplot() +
  geom_histogram(data = dur0[dur0$HH.Time<300,], aes(x = HH.Time), alpha = 0.5, fill = 'red', color = 'black') + 
  geom_vline(xintercept = median(dur0$HH.Time, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$HH.Time<300,], aes(x = HH.Time), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0$HH.Time, na.rm = T), color = 'blue')
ggplot() +
  geom_histogram(data = dur0[dur0$PS.Time<300,], aes(x = PS.Time), alpha = 0.5, fill = 'red', color = 'black') + 
  geom_vline(xintercept = median(dur0$PS.Time, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$PS.Time<300,], aes(x = PS.Time), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0$PS.Time, na.rm = T), color = 'blue')
ggplot() +
  geom_histogram(data = dur0[dur0$SSH.Time<300&dur0$SSH.Time>-1,], aes(x = SSH.Time), alpha = 0.5, fill = 'red', color = 'black') + 
  geom_vline(xintercept = median(dur0$SSH.Time, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$SSH.Time<300&durmorethan0$SSH.Time>-1,], aes(x = SSH.Time), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0$SSH.Time, na.rm = T), color = 'blue')
ggplot() +
  geom_histogram(data = dur0[dur0$DB.Time<500,], aes(x = DB.Time), alpha = 0.5, fill = 'red', color = 'black') + 
  geom_vline(xintercept = median(dur0$DB.Time, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$DB.Time<500,], aes(x = DB.Time), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0$DB.Time, na.rm = T), color = 'blue')


# e20
ggplot() +
  geom_histogram(data = dur0[dur0$PS.Time<300&dur0$enumerator=='E20',], aes(x = PS.Time), alpha = 0.5, fill = 'red', color = 'black') + 
  geom_vline(xintercept = median(dur0[dur0$PS.Time<300&dur0$enumerator=='E20',]$PS.Time, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$PS.Time<300&dur0$enumerator=='E20',], aes(x = PS.Time), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0[durmorethan0$PS.Time<300&dur0$enumerator=='E20',]$PS.Time, na.rm = T), color = 'blue')
ggplot() +
  geom_histogram(data = dur0[dur0$HH.Time<300&dur0$enumerator=='E20',], aes(x = HH.Time), alpha = 0.5, fill = 'red', color = 'black') + 
  geom_vline(xintercept = median(dur0[dur0$HH.Time<300&dur0$enumerator=='E20',]$HH.Time, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$HH.Time<300&dur0$enumerator=='E20',], aes(x = HH.Time), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0[durmorethan0$HH.Time<300&dur0$enumerator=='E20',]$HH.Time, na.rm = T), color = 'blue')
ggplot() +
  geom_histogram(data = dur0[dur0$DB.Time<500&dur0$enumerator=='E20',], aes(x = DB.Time), alpha = 0.5, fill = 'red', color = 'black') + 
  geom_vline(xintercept = median(dur0[dur0$DB.Time<500&dur0$enumerator=='E20',]$DB.Time, na.rm = T), color = 'red') +
  geom_histogram(data = durmorethan0[durmorethan0$DB.Time<500&dur0$enumerator=='E20',], aes(x = DB.Time), alpha = 0.5, fill = 'blue', color = 'black') +
  geom_vline(xintercept = median(durmorethan0[durmorethan0$DB.Time<500&dur0$enumerator=='E20',]$DB.Time, na.rm = T), color = 'blue')

# percent of calls w/ 0 duration per day 
per0day <- data %>%
  filter(Outcome2=='COMP' & Resp.Consent==1) %>%
  mutate(duration0 = ifelse(phone_call_duration==0, '0', '>0')) %>%
  group_by(Date.Interview, duration0) %>%
  tally() %>%
  group_by(Date.Interview) %>%
  mutate(nperday = sum(n),
         perc = n/nperday*100) %>%
  filter(duration0 == '0') %>%
  ggplot() +
  geom_bar(aes(x = Date.Interview, y = perc), stat='identity', fill = 'red', alpha = 0.3)

# By top enumerators
table(dur0$enumerator) #E1:255; E8:355; E20:304
e1 <- data %>%
  filter(Outcome2=='COMP' & Resp.Consent==1) %>%
  filter(enumerator == 'E1') %>%
  mutate(duration0 = ifelse(phone_call_duration==0, '0', '>0')) %>%
  group_by(Date.Interview, duration0) %>%
  tally() %>%
  group_by(Date.Interview) %>%
  mutate(nperday = sum(n),
         perc = n/nperday*100) #%>%
  #filter(duration0 == '0') 
ggplot(e1) +
  geom_bar(aes(x = Date.Interview, y = perc), stat='identity', fill = 'blue', alpha = 0.3)

e8 <- data %>%
  filter(Outcome2=='COMP' & Resp.Consent==1) %>%
  filter(enumerator == 'E8') %>%
  mutate(duration0 = ifelse(phone_call_duration==0, '0', '>0')) %>%
  group_by(Date.Interview, duration0) %>%
  tally() %>%
  group_by(Date.Interview) %>%
  mutate(nperday = sum(n),
         perc = n/nperday*100) #%>%
  #filter(duration0 == '0') 
ggplot(e8) +
  geom_bar(aes(x = Date.Interview, y = perc), stat='identity', fill = 'red', alpha = 0.3)
e20 <- data %>%
  filter(Outcome2=='COMP' & Resp.Consent==1) %>%
  filter(enumerator == 'E20') %>%
  mutate(duration0 = ifelse(phone_call_duration==0, '0', '>0')) %>%
  group_by(Date.Interview, duration0) %>%
  tally() %>%
  group_by(Date.Interview) %>%
  mutate(nperday = sum(n),
         perc = n/nperday*100) #%>%
  #filter(duration0 == '0') 
ggplot(e20) +
  geom_bar(aes(x = Date.Interview, y = perc), stat='identity', fill = 'red', alpha = 0.3)


# percent with 'other' answer in duration =0 versus not 0
others <- data %>%
  filter(Outcome2=='COMP' & Resp.Consent==1) %>%
  mutate(duration0 = ifelse(phone_call_duration==0, '0', '>0'),
         othervax = ifelse(Other.Den.Reasons=='', 'Other','No other') )%>% 
  group_by(Date.Interview) %>%
    mutate(nperday = n()) %>%
  ungroup() %>%
    group_by(Date.Interview, duration0, othervax) %>%
    summarize(nperday = nperday,
              n = n()) %>% distinct() %>%
  mutate(perc= n/nperday)
  

table(dur0$othervax) # 183/2143 =8.5% with no other answer
table(durmorethan0$othervax) #1284/9309=13.8% with no other answer

# Comparison of those that are duplicated ( 1 with duration = 0 and others with duration >0)
data_raw1 <- data_raw %>%
  mutate(priority = ifelse(call_status == '1','1',
                           ifelse(call_status == '6','2',
                                  ifelse(call_status=='2a','3',
                                         ifelse(call_status=='9','4',
                                                ifelse(call_status=='7','5',
                                                       ifelse(call_status=='5'|call_status=='10'|call_status=='7a','6','9')))))),
         priority= as.character(priority)) %>%
  filter(phone_1 !=244000000000 & phone_1 !=821014960&phone_1 !=817107230 & phone_1 !=812174394 &phone_1 !=826415816& phone_1 !=829742227&
           phone_1 !=995492473 &phone_1 !=818053613& phone_1 !=842287433 & phone_1 !=811551123& phone_1 !=825807368& phone_1 !=896547944
         & phone_1 !=813754456& phone_1 !=817138504& phone_1 !=896477873& phone_1 !=823939751& phone_1 !=858905822& phone_1 !=822700477&
           phone_1 !=842450129& phone_1 !=811883499& phone_1 !=898231370& phone_1 !=907023693) %>%
  filter(!caseid %in% c(12,51,57,56,54,58,53,52,59)) %>%
  filter(!caseid %in% c("MANGOYO CHADDAI","IVONE MUTUZA GLORIA","MULENDA GRACE","BUKINGA FEZA","LELA VALENCIA","MWAVITA AIME","MUTUALE DYNAH","LUBELO ASHIGO CHANCELVIE",
                        "MWAMINI AMISI MARIA","N'SUBI ALIMA HONORINE","YOMBO NGOYI JEANCY","MWAMINI KABALA DONATE")) %>%
  filter(!username %in% c('shammi.luhar2@lshtm.ac.uk','kelly.mccain@lshtm.ac.uk','malebogo.tlhajoane@lshtm.ac.uk','georges.reniers@lshtm.ac.uk'))
data_raw1 <- data_raw1[order(data_raw1$phone_1,  data_raw1$priority,data_raw1$SubmissionDate),]#data_raw1$priority,
data_raw1$caseid_original <- data_raw1$caseid
data_raw1$caseid <- paste0('ID - ',data_raw1$phone_1)
data_raw2 <- data_raw1[duplicated(data_raw1[c("phone_1","call_num",'caseid')]),]
data_raw3 <- data_raw2[duplicated(data_raw2[c("phone_1","call_num")]),]
dups <- data_raw3

table(dups$phone_call_duration)
# dupcomp <- dups %>%
#   filter(consent == 1 & call_status==1)
#get dataset of all completed interviews with phone numbers that match those that are duplicated (dups)
duplicates <- data_raw %>% 
  filter(!username %in% c('shammi.luhar2@lshtm.ac.uk','kelly.mccain@lshtm.ac.uk','malebogo.tlhajoane@lshtm.ac.uk','georges.reniers@lshtm.ac.uk')) %>%
  filter(!phone_1 %in% c('244000000000' ,'0821014960' ,'0817107230', '0812174394', '0826415816', '0829742227','0995492473','0818053613', '0842287433',
                         '0811551123','0825807368','0896547944','0813754456','0817138504','0896477873','0823939751','0858905822','0822700477',
                         '0842450129','0811883499','0898231370','0907023693')) %>%
  filter(phone_1 %in% dups[dups$call_status==1,]$phone_1) %>% 
  select(c(caseid, phone_1, call_status, endtime, SubmissionDate, users, phone_call_duration, call_num))

duplicates_comp <- duplicates %>% 
  filter(call_status==1)  #filter on completed 

#vector of phone numbers that have phone duration of 0
phones <- duplicates_comp[duplicates_comp$phone_call_duration==0,]$phone_1

#filter on these numbers w/ duration of 0 and then restrict to those with multiple completed calls 
duplic <- duplicates_comp %>%
  filter(phone_1 %in% phones) %>%
  group_by(phone_1) %>% add_tally %>%
  filter(n >1) %>%
  select(c(phone_1,endtime, users,consent, call_status,phone_call_duration,duration, elig_time, consent_time, arrangements_time,
           background_time, vax_time, HH_time, PS_time, SSH_time, DB_time, audio_audit, region, NordKivu, Kinshasa,  A18to39, A40to64,
           L1_A_1, E2,E3, E4a, education_level,school_attendance, marriage_status, marriage_formal, roofing_material, roothing_other,
           other_waterSource, water_source,
           highest_grades_a, highest_grades_b, highest_grades_c,hd1_householdrel,hd2_b, hd3_b, htotal, hd4,hd4_b, hd6, hd6_b,b4,
           NumberSistersBrothers_Bro, NumberSistersBrothers_1,ssh2_died_1,ssh3_died_1,ssh2_died_bro,ssh3_died_bro,ps1_biological_parents_1,
           ps6_pryearDied_1,ps5_p_died_age_1,ps2_parents_age_1,ps1_biological_parents2_1,ps6_pryearDied2_1,  ps5_p_died_age2_1,ps2_parents_age2_1,
           )) %>%
  dplyr::rename(Sisters = NumberSistersBrothers_1,
         Brothers = NumberSistersBrothers_Bro,
         Sis1.Dead = ssh2_died_1,
         Sis1.Dead.2019 = ssh3_died_1,
         Bro1.Dead = ssh2_died_bro,
         Bro1.Dead.Yr = ssh3_died_bro,
         M.Dead = ps1_biological_parents_1,
         M.dead.Yr = ps6_pryearDied_1,
         M.dead.Age = ps5_p_died_age_1,
         M.alive.age = ps2_parents_age_1,
         F.Dead = ps1_biological_parents2_1,
         F.dead.Yr = ps6_pryearDied2_1,         
         F.dead.Age = ps5_p_died_age2_1,
         F.alive.age = ps2_parents_age2_1,
         U5Death = hd4_b,
         O5Death = hd6_b,
         U5DiedYN = hd4,
         O5DiedYN = hd6,
         U5 = hd2_b,
         O5 = hd3_b,
         OwnPhone = b4,
         ResidAreas=E4a,
         Resp.Language = L1_A_1,
         Resp.Age = E3,
         Resp.Consent = consent,
         Resp.Region = region,
         Resp.Educ = education_level,
         Resp.Sex = E2,
         Resp.Marriage = marriage_status)
write.csv(duplic, 'C://Users/KellyMcCain/Downloads/DuplicateswithDuration0.csv', row.names = F)


# comparison of distribution of call outcomes for duration = 0 and duration >0
dur0all <- data_raw %>%
  mutate(duration0 = ifelse(phone_call_duration==0, '0', '>0')) %>%
  filter(duration0=='0') 
durnot0all <- data_raw %>%
  mutate(duration0 = ifelse(phone_call_duration==0, '0', '>0')) %>%
  filter(duration0!='0')

ggplot() +
  geom_bar(data = dur0all, aes(x = call_status), alpha = 0.5, fill = 'red', color = 'black', stat = 'count') +
  geom_bar(data = durnot0all, aes(x = call_status), alpha = 0.5, fill = 'blue', color = 'black', stat = 'count') 
  

# Those with phone duration of 0 and not consented
dur0all <- data_raw %>%
  select(c(KEY,endtime,phone_1, users,  consent, call_status,phone_call_duration,duration, elig_time, consent_time, arrangements_time,
           background_time, vax_time, HH_time, PS_time, SSH_time, DB_time, audio_audit,other_waterSource, E10, )) %>%
  filter(phone_call_duration==0 & call_status %in% c('2','2a','6','9','4a','8','11','3','4')) 
# table(dur0all$call_status)
# audios <- dur0all %>% 
#   filter(audio_audit!='')

# Creation of list of suspicious numbers
data_raw1_ <- data_raw1[!duplicated(data_raw1[c("phone_1","call_num",'caseid')]),]
data_raw1_ <- data_raw1_[!duplicated(data_raw1_[c("phone_1","call_num")]),]
data_raw1_ <- rbind(
  data_raw1_ %>%
    filter(call_status==1 & !duplicated(phone_1) ),
  data_raw1_ %>%
    filter(call_status!=1))
sus.nums <- data_raw1_ %>%
  filter(phone_call_duration == 0 &  call_status==1 & audio_audit!='') %>%
  select(c(caseid, caseid_original,endtime, call_num, caseid, audio_audit, main_language_label, phone_call_duration)) 

set.seed(2628)

swahili20 <- sus.nums %>%
  filter(main_language_label == 'Swahili') %>%
  sample_n(20)

lingala10 <- sus.nums %>%
  filter(main_language_label == 'Lingala') %>%
  sample_n(10)

extra10 <- data_raw1_ %>%
  filter(phone_call_duration >0 & call_status==1 & audio_audit!="")%>%
  filter(substr(endtime, 1, 2) != "Au" & substr(endtime, 1,5) !='Sep 6'& substr(endtime, 1,5) !='Sep 2'& substr(endtime, 1,5) !='Sep 1'
         & substr(endtime, 1,5) !='Sep 5') %>%
  select(c(caseid, endtime,phone_call_duration, call_num, caseid, audio_audit, main_language_label)) %>%
  group_by(main_language_label) %>%
  sample_n(5)

tokingsolomon1 <- rbind(swahili20, lingala10, extra10) %>%
  arrange(caseid)
write.csv(tokingsolomon1, 'C://Users/KellyMcCain/Downloads/AudioAudits_20220519.csv', row.names = F)
#write.csv(sus.nums, 'C://Users/KellyMcCain/Downloads/NumberstoInvestigate_audio.csv', row.names = F)
# write.csv(swahili, 'C://Users/KellyMcCain/Downloads/SwahiliNumberstoInvestigate_audio.csv', row.names = F)
# write.csv(lingala, 'C://Users/KellyMcCain/Downloads/LingalaNumberstoInvestigate_audio.csv', row.names = F)
# write.csv(extra10, 'C://Users/KellyMcCain/Downloads/Extra10NumberstoInvestigate_audio.csv', row.names = F)

# Getting audio audits file with the phone_call_durations and adding variables
durs <- read_xlsx('C://Users/KellyMcCain/Downloads/AudioAudits_202205191_EM_FILLED.xlsx')
lookup <- data_raw1_ %>% 
  mutate(caseid = caseid_original) %>%
  select(c(phone_1, users, caseid, call_num, phone_call_duration)) #continue_discontinue, education_level,
# vaccine_dose, hd2_b, hd3_b, NumberSistersBrothers_1, NumberSistersBrothers_Bro,
# E3, region, E2, E5, E6, E7, E7_b
durswtime <- left_join(durs, lookup, by = c('caseid', 'call_num')) %>%
  select(c(caseid, users, endtime, phone_call_duration, everything()))
# write.csv(durswtime, 'C://Users/KellyMcCain/Downloads/AudioAudits_20220519_EM_FILLED_with_enums.csv', row.names = F)




sus.numslastmonth <- data_raw1_ %>%
  filter(phone_call_duration==0 & consent ==1 & call_status==1) 

sus.numslastmonth$Date.Interview <-  gsub('.{11}$', '', sus.numslastmonth$endtime)
sus.numslastmonth <- sus.numslastmonth %>%
    mutate(Date.Interview1 = as.POSIXct(Date.Interview, format="%h %d, %Y"),
         Date.Interview = format(Date.Interview1, "%Y-%m-%d")
  ) %>%
  dplyr::select(-Date.Interview1) %>%
  filter(Date.Interview > '2022-03-01') %>%
  group_by(users) %>%
  dplyr::mutate(num = seq_along(users)) %>%
  ungroup() %>%
  filter(num < 5) %>%
  select(c(phone_1, 'enumerator' = users, Date.Interview, call_num, audio_audit, 
           'Resp.Language'=L1_A_1, 'Resp.Sex'=E2, 'Resp.Age'=E3, 'CityTownRural'=E4a, education_level,school_attendance, 'highest_grades_prim'=highest_grades_a, 
           'highest_grades_sec'=highest_grades_b, 'highest_grades_higher'=highest_grades_c,
           marriage_status, marriage_formal, roofing_material, water_source, 
           'HeadofHHRel'=hd1_householdrel,'NumChildreninHH'=hd2_b, 'NumOver5inHH'=hd3_b, 'HHsize'=htotal, 'Under5DeathYN'=hd4,'NumUnder5Deaths'=hd4_b, 
           'Over5DeathYN'=hd6, 'NumOver5Deaths'=hd6_b, 
           'MotherAliveYN'=ps1_biological_parents_1, 'CurrentMotherAge'=ps2_parents_age_1,'YrMotherDeath'=ps6_pryearDied_1, 'AgeMotherDeath'=ps5_p_died_age_1,
           'FatherAliveYN'=ps1_biological_parents2_1, 'CurrentFatherAge'=ps2_parents_age2_1, 'YrFatherDeath'=ps6_pryearDied2_1, 'AgeFatherDeath'=ps5_p_died_age2_1,
           'NumBrothers'=NumberSistersBrothers_Bro, 'NumSisters'=NumberSistersBrothers_1,'NumSisDied'=ssh2_died_1,'NumSisDiedSince2019'=ssh3_died_1,
           'NumBroDied'=ssh2_died_bro,'NumBroDiedSince2019'=ssh3_died_bro
           ))
write.csv(sus.numslastmonth, 'C://Users/KellyMcCain/Downloads/NumberstoCallback_March.csv', row.names = F)




# looking at all phone numbers of duraiton = 0 to see if they were referred before
dur0phones <- duration0$phone_1
data_w0s <- data_raw %>%
  filter(phone_1 %in% dur0phones)
table(data_w0s$call_status)

comp_dur0 <- dur0all %>%
  filter(call_status =='1')
table(comp_dur0$hd1_householdrel, useNA = "always")
table(comp_dur0$E2, useNA = "always")
table(comp_dur0$E3, useNA = "always")
table(comp_dur0$ps1_biological_parents_1, useNA = "always")


# Histograms of phone call duraitons 
Duration <- Consented %>%
  dplyr::select(Total, Elig.Time, Arrangement.Time, Consent.Time,
                Background.Time, C19.Time, HH.Time,
                PS.Time, SSH.Time, DB.Time, Phone.duration, Date.Interview, endtime, enumerator, month.interview, phone_call_duration) %>%
  drop_na(Elig.Time) %>%
  dplyr::filter(phone_call_duration <2401)
ggplot(Duration) + geom_histogram(aes(x = phone_call_duration/60), fill = 'darkblue',alpha = 0.6, color = 'black', bins = 40) + 
  xlab('Phone call duration (min)') +
  ylab('Count') + 
  labs(title = 'Phone call durations (min) of consented interviews - DRC')

Duration <- Consentedmw %>%
  dplyr::select(Date.Interview, month.interview, Phone.duration) %>%
  dplyr::filter(Phone.duration <2401)
ggplot(Duration) + geom_histogram(aes(x = as.numeric(Phone.duration)/60), fill = 'darkblue',alpha = 0.6, color = 'black', bins = 40) + 
  xlab('Phone call duration (min)') +
  ylab('Count') + 
  labs(title = 'Phone call durations (min) of consented interviews - Malawi')

# malawi interviews w/ duration of 0
nums0mw <- Consentedmw[Consentedmw$Phone.duration==0,]$phone_1
dur0mw <- data_clean3 %>% 
  filter(phone_1 %in% nums0mw) %>%
  select(c(call_num, phone_1, caseid, users, Phone.duration, duration, call_status, Date.Interview, phone_call_log, phonenumber_called_1))



#### Table with frequency distribution of calls with dur = 0 per month by enumerator
dur0 <- data %>%
  filter(phone_call_duration==0& Resp.Consent==1 & Outcome2=='COMP') %>%
  select(c(phone_1, endtime,  formdef_version, deviceid,enumerator, phone.call.log, Date.Interview, phone_call_duration, 
           Outcome2, month.interview)) 

tbl <- table(dur0$enumerator, dur0$month.interview) %>% as.data.frame() 
write.csv(tbl, 'C://Users/KellyMcCain/Downloads/Dur0bymonthandenum.csv')


######################################################
#
# Part 2 for Eddy
# 
######################################################
recent <- data_raw %>%
  mutate(Date.Interview = gsub('.{11}$', '', endtime),
         Date.Interview1 = as.POSIXct(Date.Interview, format="%h %d, %Y"),
         Date.Interview = format(Date.Interview1, "%Y-%m-%d")
  ) %>%
  dplyr::select(-Date.Interview1) %>%
  filter(call_status == 1 & audio_audit!='') %>%
  select(c(Date.Interview, caseid, users, endtime, duration, phone_call_duration, phone.call.log, call_num, caseid, audio_audit, main_language_label))
write.csv(recent, 'C://Users/KellyMcCain/Downloads/AudioAudits_2022071222222.csv')
hmm <- data_raw %>%
  filter(caseid =='2742 - June 2022 - Nord-Kivu, Orange' | caseid == '2142 - June 2022 - Nord-Kivu, Vodacom') %>%
  select(c(caseid, users, endtime, duration, phone_call_duration, phone.call.log, call_num, caseid, audio_audit, main_language_label))
  