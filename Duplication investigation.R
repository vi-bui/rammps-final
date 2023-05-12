duplicatesdat1 <- dat1mw %>%
  dplyr::group_by(phone_1, caseid, call_num) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

# Investigating duplicates
`%notin%` <- Negate(`%in%`)
dir.inputdata <- paste0(Sys.getenv('USERPROFILE'),"/London School of Hygiene and Tropical Medicine/RAMMPS_LSHTM_Group - Documents/General/Partners/UNIKIN/SurveyCTO Audits/")
data_raw <- read.csv(paste0(dir.inputdata,"RAMMPS DRC 20_07_WIDE.csv"))
data_raw1 <- data_raw %>%
  mutate(priority = ifelse(call_status == '1','1',
                           ifelse(call_status == '6','2',
                                  ifelse(call_status=='2a','3',
                                         ifelse(call_status=='9','4',
                                                ifelse(call_status=='7','5',
                                                       ifelse(call_status=='5'|call_status=='10'|call_status=='7a','6','9')))))),
         priority= as.factor(priority)) %>%
   filter(phone_1 !=244000000000 & phone_1 !=821014960&phone_1 !=817107230 & phone_1 !=812174394 &phone_1 !=826415816& phone_1 !=829742227&
           phone_1 !=995492473 &phone_1 !=818053613& phone_1 !=842287433 & phone_1 !=811551123& phone_1 !=825807368& phone_1 !=896547944
         & phone_1 !=813754456& phone_1 !=817138504& phone_1 !=896477873& phone_1 !=823939751& phone_1 !=858905822& phone_1 !=822700477&
         phone_1 !=842450129& phone_1 !=811883499& phone_1 !=898231370& phone_1 !=907023693) %>%
  filter(!caseid %in% c(12,51,57,56,54,58,53,52,59)) %>%
  filter(!caseid %in% c("MANGOYO CHADDAI","IVONE MUTUZA GLORIA","MULENDA GRACE","BUKINGA FEZA","LELA VALENCIA","MWAVITA AIME","MUTUALE DYNAH","LUBELO ASHIGO CHANCELVIE",
                        "MWAMINI AMISI MARIA","N'SUBI ALIMA HONORINE","YOMBO NGOYI JEANCY","MWAMINI KABALA DONATE")) %>%
  filter(!username %in% c('shammi.luhar2@lshtm.ac.uk','kelly.mccain@lshtm.ac.uk','malebogo.tlhajoane@lshtm.ac.uk','georges.reniers@lshtm.ac.uk'))
data_raw1 <- data_raw1[order(data_raw1$phone_1, data_raw1$priority, data_raw1$SubmissionDate),]
data_raw12 <- data_raw1
data_raw1 <- data_raw1[!duplicated(data_raw1[c("phone_1","call_num",'caseid')]),]
data_raw1 <- data_raw1[!duplicated(data_raw1[c("phone_1","call_num")]),]
data_raw1 <- rbind(
  data_raw1 %>%
    filter(call_status==1 & !duplicated(phone_1) ),
  data_raw1 %>%
    filter(call_status!=1))
data_raw1 <- data_raw1 %>%
  group_by(phone_1) %>%
  mutate(closed = ifelse(call_status %in% c(1, '2a','4a',6, 7, 9,11),call_num,NA)) %>%
  fill(closed) %>%
  group_by(phone_1, caseid) %>%
  mutate(remove = ifelse(call_num > closed, 1, NA)) %>%
  filter(is.na(remove))
#data_raw1 <- data_raw1[!duplicated(data_raw1[c("phone_1","call_num")]),]
# b4dedup <- data_raw1 %>% filter(consent == 1& now_complete == 'Yes'&call_num %notin% c(0,6)&!is.na(devicephonenum)&region!=3)
# afterdedup <- data_raw1 %>% filter(consent == 1& now_complete == 'Yes'&!is.na(devicephonenum)&call_num %notin% c(0,6)&region!=3)#where data_raw12 is linke 22

# Create the dataset of duplicates
data_raw1 <- data_raw %>%
  mutate(priority = ifelse(call_status == '1','1',
                           ifelse(call_status == '6','2',
                                  ifelse(call_status=='2a','3',
                                         ifelse(call_status=='9','4',
                                                ifelse(call_status=='7','5',
                                                       ifelse(call_status=='5'|call_status=='10'|call_status=='7a','6','9')))))),
         priority= as.factor(priority)) %>%
  filter(phone_1 !=244000000000 & phone_1 !=821014960&phone_1 !=817107230 & phone_1 !=812174394 &phone_1 !=826415816& phone_1 !=829742227&
           phone_1 !=995492473 &phone_1 !=818053613& phone_1 !=842287433 & phone_1 !=811551123& phone_1 !=825807368& phone_1 !=896547944
         & phone_1 !=813754456& phone_1 !=817138504& phone_1 !=896477873& phone_1 !=823939751& phone_1 !=858905822& phone_1 !=822700477&
           phone_1 !=842450129& phone_1 !=811883499& phone_1 !=898231370& phone_1 !=907023693) %>%
  filter(!caseid %in% c(12,51,57,56,54,58,53,52,59)) %>%
  filter(!caseid %in% c("MANGOYO CHADDAI","IVONE MUTUZA GLORIA","MULENDA GRACE","BUKINGA FEZA","LELA VALENCIA","MWAVITA AIME","MUTUALE DYNAH","LUBELO ASHIGO CHANCELVIE",
                        "MWAMINI AMISI MARIA","N'SUBI ALIMA HONORINE","YOMBO NGOYI JEANCY","MWAMINI KABALA DONATE")) %>%
  filter(!username %in% c('shammi.luhar2@lshtm.ac.uk','kelly.mccain@lshtm.ac.uk','malebogo.tlhajoane@lshtm.ac.uk','georges.reniers@lshtm.ac.uk'))
data_raw1 <- data_raw1[order(data_raw1$phone_1, data_raw1$priority, data_raw1$SubmissionDate),]

#get duplicated before removing
data_raw1dup1 <- data_raw1[duplicated(data_raw1[c("phone_1","call_num",'caseid')]),]
nrow(data_raw1dup1)
#removing based on these 3 vars
data_raw1dedup1 <- data_raw1[!duplicated(data_raw1[c("phone_1","call_num",'caseid')]),]
nrow(data_raw1dedup1)

#get duplicates based on 2 vars before removing
data_raw1dup2 <- data_raw1dedup1[duplicated(data_raw1dedup1[c("phone_1","call_num")]),]
nrow(data_raw1dup2)
#removing based on these 2 vars
data_raw1dedup2 <- data_raw1dedup1[!duplicated(data_raw1dedup1[c("phone_1","call_num")]),]
nrow(data_raw1dedup2)

#get duplicate consented ones
data_raw1dup2.5 <- data_raw1dedup2 %>%
    filter(call_status==1 & duplicated(phone_1))
nrow(data_raw1dup2.5)
#removing duplicate consented ones
data_raw1dedup2.5 <- rbind(
  data_raw1dedup2 %>%
    filter(call_status==1 & !duplicated(phone_1) ),
  data_raw1dedup2 %>%
    filter(call_status!=1))
nrow(data_raw1dedup2.5)

#get final duplicates
data_raw1dup3 <- data_raw1dedup2.5 %>%
  group_by(phone_1) %>%
  mutate(closed = ifelse(call_status %in% c(1, '2a','4a',6, 7, 9,11),call_num,NA)) %>%
  fill(closed) %>%
  group_by(phone_1, caseid) %>%
  mutate(remove = ifelse(call_num > closed, 1, NA)) %>%
  filter(!is.na(remove)) %>% select(-c(closed, remove))
nrow(data_raw1dup3)
#remove final duplicates
data_raw1dedup3 <- data_raw1dedup2.5 %>%
  group_by(phone_1) %>%
  mutate(closed = ifelse(call_status %in% c(1, '2a','4a',6, 7, 9,11),call_num,NA)) %>%
  fill(closed) %>%
  group_by(phone_1, caseid) %>%
  mutate(remove = ifelse(call_num > closed, 1, NA)) %>%
  filter(is.na(remove)) %>% select(-c(closed, remove))
nrow(data_raw1dedup3)

#bind all 3 duplicated dfs together
duplicates <- rbind(data_raw1dup1, data_raw1dup2, data_raw1dup2.5,data_raw1dup3)
#table(duplicates$call_status)
#write.csv(duplicates, paste0(dir.inputdata,'Duplicates DFs/Duplicates_',Sys.Date(),'.csv'))


# data_raw1 <- data_raw1[!duplicated(data_raw1[c("phone_1","call_num",'caseid')]),]
dupsindata_raw <- data_raw1[duplicated(data_raw1[c("phone_1","call_num")]),]
dupsindata_raw <- data_raw1 %>% filter(phone_1 %in% dupsindata_raw$phone_1)
priority <- dupsindata_raw %>% select(c(call_status, priority, phone_1, caseid, call_num, SubmissionDate))

#below - this is a summary of those with completed interviews that are duplicated i.e. have more than 1 caseid per phone number among those who have completed interviews
dupl <- priority %>% 
  filter(call_status==1 ) %>%
  group_by(phone_1, caseid) %>% 
  mutate(num_dups = n(), 
         dup_id = row_number()) %>% 
  summarize(n=n()) %>%
  group_by(phone_1) %>%
  summarise(n=n())
table(dupl$n)
# 139+12+1 = 152 to be removed
# keep all that hvae more than 1 caseid overall, but only 1 associated w completed case
# keep only 1 completed interview for those with >1 caseid associated with completed interview for that phone#
check <- data_raw1 %>% filter(phone_1=='808303327') %>% select(c(call_status, call_num, phone_1, caseid, SubmissionDate, username))
check <- data_raw1 %>% filter(phone_1=='808401314') %>% select(c(call_status, call_num, phone_1, caseid, SubmissionDate, username))
# After de-duping by phone_1 and call_num, then must get rid of mismatching caseids (ex. completed on call 2 then 3-5 w/ same # but diff ids)
  # outcomes that result in a closed case: call_status %in% c(1, 6, 7, 9) #complted, refused, NNU, ineligilbe
# for situations that result in a closed case, then any call nums greater than those for that call that triggers a closed case should be 
# removed from the dataset
closed <- data_raw1 %>%
  filter(call_status %in% c(1, 6, 7, 9)) %>% select(c(call_num, call_status, phone_1, caseid))

smalldf <- data_raw1 %>% select(c(call_num, call_status, phone_1, caseid,SubmissionDate)) %>%
  group_by(phone_1) %>%
  mutate(closed = ifelse(call_status %in% c(1, 6, 7, 9),call_num,NA)) %>%
  fill(closed) %>%
  group_by(phone_1, caseid) %>%
  mutate(remove = ifelse(call_num > closed, 1, NA)) %>%
  filter(is.na(remove))


test <- left_join(smalldf, closed,by =c('phone_1','caseid', 'call_num')) %>%
  group_by(phone_1, caseid, call_num) %>%
  arrange(caseid, call_num, .by_group = TRUE)
# first, need to identify all phone numbers with these answers that should result in a closed case, THen need to get ID those with call 
# nums greater than the one that triggers the closed case for that phone number

check2 <- priority %>%
  filter(phone_1 %in% closed$phone_1) %>%
  group_by(phone_1) %>%
  distinct(caseid, .keep_all = TRUE) %>%
  mutate(nidpernum=n()) #this tells us the #s that should be closed 




##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##### 
##### Looking into the duplicates (duplicates df above)
#####
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
dupnums <- duplicates$phone_1
duplic <- data_raw12 %>%
  select(c(phone_1, caseid, call_status, username, region, E3, consent, education_level, E2, marriage_status, water_source, NumberSistersBrothers_1, 
           NumberSistersBrothers_Bro, ssh2_died_1, ssh3_died_1, ssh2_died_bro, ssh3_died_bro, ps1_biological_parents_1, ps6_pryearDied_1,
           ps5_p_died_age_1, ps2_parents_age_1, ps1_biological_parents2_1, ps6_pryearDied2_1, ps5_p_died_age2_1, ps2_parents_age2_1,
           hd4_b,hd5b_0_1,hd6_b,hd7b_age_1, hd2_b, hd3_b, vaccine_dose, vaccine_names, SubmissionDate)) %>%
  filter(phone_1 %in% dupnums & call_status == 1) %>%
  group_by(phone_1) %>%
  mutate(n=n()) %>%
  filter(n>1)
write.csv(duplic, paste0(dir.inputdata,'Duplicates DFs/Cons_Duplicates_',Sys.Date(),'.csv'))

library(janitor)
duplic2 <- duplic %>%
  select(-c(SubmissionDate)) %>%
  group_by(phone_1) 

df <- duplic2
Var <- duplic2$E3
i <- duplic2$education_level
crosstabs <- function(df, Var) {
  df$variable <- Var
  #listofdfs <- list()
  #for (i in varnames){
  df1 <- df %>% 
    tabyl(phone_1, variable)
  df1$count <- apply(df1, 1, function(x) (length(which(x >0 & x<100))))  
  
  
  # df1 <- df %>%
  #   tabyl(phone_1, variable) %>%
  #listofdfs[[i]] <- df1
  #}
  #return(listofdfs)
  return(df1)
}
crosstabs(duplic2, duplic2$vaccine_dose)

# sapply(duplic2,colnames(duplic2), crosstabs)
varnames <- paste0('duplic2$',colnames(duplic2)) 
varnames <- varnames[! varnames %in% c('duplic2$phone_1', 'duplic2$caseid', 'duplic2$call_status', 'duplic2$username')]
listofdfs <- list()
for (i in varnames) {
  #duplic2$variable <- duplic2[,i]
  #print(variable)
 # df$Var <-
  listofdfs[[i]] <- crosstabs(duplic2, i)

  return(listofdfs)
}

region <- crosstabs(duplic2, duplic2$region) %>% select(c(phone_1,count)) %>% 
  group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%
  dplyr::rename(count_region=count) %>%
  adorn_totals("row")
E3 <- crosstabs(duplic2, duplic2$E3) %>% select(c(count)) %>%   
  group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%
  dplyr::rename(count_E3=count)%>%
  adorn_totals("row")
education <- crosstabs(duplic2, duplic2$education_level) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_educ=count)%>%
  adorn_totals("row")
E2 <- crosstabs(duplic2, duplic2$E2) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_E2=count)%>%
  adorn_totals("row")
marriage <- crosstabs(duplic2, duplic2$marriage_status) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_marriage=count)%>%
  adorn_totals("row")
water <- crosstabs(duplic2, duplic2$water_source) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_water=count)%>%
  adorn_totals("row")
NumSis <- crosstabs(duplic2, duplic2$NumberSistersBrothers_1) %>% select(c(count)) %>%group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>% dplyr::rename(count_NumSis=count)%>%
  adorn_totals("row")
NumBro <- crosstabs(duplic2, duplic2$NumberSistersBrothers_Bro) %>% select(c(count)) %>%group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>% dplyr::rename(count_NumBro=count)%>%
  adorn_totals("row")
ssh21 <- crosstabs(duplic2, duplic2$ssh2_died_1) %>% select(c(count)) %>%group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>% dplyr::rename(count_ssh21=count)%>%
  adorn_totals("row")
ssh31 <- crosstabs(duplic2, duplic2$ssh3_died_1) %>% select(c(count)) %>%group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>% dplyr::rename(count_ssh31=count)%>%
  adorn_totals("row")
ssh2bro <- crosstabs(duplic2, duplic2$ssh2_died_bro) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_ssh2bro=count)%>%
  adorn_totals("row")
ssh3bro <- crosstabs(duplic2, duplic2$ssh3_died_bro) %>% select(c(count)) %>%group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>% dplyr::rename(count_ssh3bro=count)%>%
  adorn_totals("row")
momalive <- crosstabs(duplic2, duplic2$ps1_biological_parents_1) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_momalive=count)%>%
  adorn_totals("row")
dadalive <- crosstabs(duplic2, duplic2$ps1_biological_parents2_1) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_dadalive=count)%>%
  adorn_totals("row")
momyrdead <- crosstabs(duplic2, duplic2$ps6_pryearDied_1) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_momyrd=count)%>%
  adorn_totals("row")
dadyrdead <- crosstabs(duplic2, duplic2$ps6_pryearDied2_1) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_dadyrd=count)%>%
  adorn_totals("row")
momagedead <- crosstabs(duplic2, duplic2$ps5_p_died_age_1) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_momaged=count)%>%
  adorn_totals("row")
dadagedead <- crosstabs(duplic2, duplic2$ps5_p_died_age2_1) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_dadaged=count)%>%
  adorn_totals("row")
momagealive <- crosstabs(duplic2, duplic2$ps2_parents_age_1) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_momagea=count)%>%
  adorn_totals("row")
dadagealive <- crosstabs(duplic2, duplic2$ps2_parents_age2_1) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_dadagea=count)%>%
  adorn_totals("row")
hd4b <- crosstabs(duplic2, duplic2$hd4_b) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_hd4b=count)%>%
  adorn_totals("row")
hd5b <- crosstabs(duplic2, duplic2$hd5b_0_1) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_hd5b=count)%>%
  adorn_totals("row")
hd6b <- crosstabs(duplic2, duplic2$hd6_b) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_hd6b=count)%>%
  adorn_totals("row")
hd7b <- crosstabs(duplic2, duplic2$hd7b_age_1) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_hd7b=count)%>%
  adorn_totals("row")
hd2b <- crosstabs(duplic2, duplic2$hd2_b) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_hd2b=count)%>%
  adorn_totals("row")
hd3b <- crosstabs(duplic2, duplic2$hd3_b) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_hd3b=count)%>%
  adorn_totals("row")
vaccinedose <- crosstabs(duplic2, duplic2$vaccine_dose) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_dose=count)%>%
  adorn_totals("row")
vaxname <- crosstabs(duplic2, duplic2$vaccine_names) %>% select(c(count)) %>% group_by(count) %>% 
  summarise(ninterview =n()) %>%
  ungroup() %>%
  mutate(percent = ninterview/sum(ninterview)) %>%dplyr::rename(count_vname=count)%>%
  adorn_totals("row")
listofdfs <- list(region, E3, education, E2, marriage, water,NumSis, NumBro, ssh21, ssh31,ssh2bro, ssh3bro,momalive,dadalive, momyrdead, 
                  dadyrdead, momagedead, dadagedead, momagealive, dadagealive, hd2b, hd3b, hd4b, hd5b, hd6b, hd7b, vaccinedose, vaxname)

dups <- bind_cols(listofdfs) 
dups <- dups %>%
  adorn_totals("row") %>%
  group_by()
  #pivot_longer(2:29, names_to = 'variable',values_to = 'count')
duplist <- split(dups, f = dups$phone_1)  
dups2 <- bind_cols(listofdfs)

check1 <- duplic2 %>% 
  tabyl(phone_1, E3)
check1$count1 <- apply(check1, 1, function(x) (length(which(x >0 & x<100))))  
check2 <- duplic2 %>% 
  tabyl(phone_1, education_level)
check3 <- duplic2 %>% 
  tabyl(phone_1, marriage_status)
check4 <- duplic2 %>% 
  tabyl(phone_1, water_source)
check5 <- duplic2 %>% 
  tabyl(phone_1, NumberSistersBrothers_1)
check6 <- duplic2 %>% 
  tabyl(phone_1, NumberSistersBrothers_Bro)
check7 <- duplic2 %>% 
  tabyl(phone_1, ssh2_died_1)
check8 <- duplic2 %>% 
  tabyl(phone_1, ssh2_died_bro)
check9 <- duplic2 %>% 
  tabyl(phone_1, ps1_biological_parents_1)
check10 <- duplic2 %>% 
  tabyl(phone_1, ps1_biological_parents2_1)
check11 <- duplic2 %>% 
  tabyl(phone_1, call_status)
