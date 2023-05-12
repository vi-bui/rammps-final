# Malawi data cleaning script - MWApp
function(mwdata_raw){
pkgs <- c('plyr','tidyverse', 'readxl','date',  'tidyselect', 'httr', 'jsonlite', 'foreign')
lapply(pkgs, require, character.only = TRUE)

  dir.output <- "/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/IPOR/SurveyData/"
  dir.gen2 <- "/Users/lshvb5/Documents/rammps/RaMMPS_MWApp/"

`%notin%` <- Negate(`%in%`)

dir.inputdata <- dir.output

# Treatment of duplicate caseid/phone_1/call_nums
# first, make date/time variable for end time of call 
mwdata_raw$DateTime <- as.POSIXct(mwdata_raw$endtime, format = "%b %d, %Y %r")
mwdata_raw$StartTime <- as.POSIXct(mwdata_raw$starttime, format = "%b %d, %Y %r")
mwdata_raw$compandcons <- ifelse(mwdata_raw$call_status == 1 & mwdata_raw$now_complete=='Yes'&mwdata_raw$C1_1 ==1, 1,0)
# then sort by date/time 
temp <- mwdata_raw %>%
  group_by(caseid) %>%
  dplyr::arrange(DateTime, .by_group = TRUE) %>%
# The labelling of referrals was done by Kelly, but it was removed to analyse referrals
# # for situations where there was a referral, if time > time when call_num = 0, then the caseid should be modified to be {caseid}-Refer
#   dplyr::mutate(caseid = case_when(
#     lag(call_num)== 0~ paste0(caseid, "-Refer"),
#     lag(call_num,2)==0~ paste0(caseid, "-Refer"),
#     lag(call_num,3)==0~ paste0(caseid, "-Refer"),
#     lag(call_num,4)==0~ paste0(caseid, "-Refer"),
#     TRUE ~ caseid)) %>%
# make new call_num which is just numbered observations within group of caseid/phone number
  group_by(caseid) %>%
  dplyr::mutate(call_num_old = call_num, 
                call_num = 1:n()) 
 
# re-combine this de=duped consented list with the rest 
mwdata_raw1 <- rbind(
  temp %>%
    filter(compandcons == 1) %>%
    filter(phone_call_duration !=0) %>%
    group_by(caseid) %>%
    slice_min(DateTime, n = 1), # if there is a duplicated consented interview, then remove earlier one
  temp %>% filter(compandcons != 1)# re-add all the other interviews (non-consented)
) %>% ungroup()

# Remove observations after the case that is supposed to close it 
mwdata_raw1 <- mwdata_raw1 %>%
  group_by(caseid) %>%
  mutate(closed = ifelse(call_status %in% c(1, 3,4,7, 11, 12, 18), call_num,NA)) %>%
  fill(closed) %>%
  group_by(caseid) %>%
  mutate(remove = ifelse(call_num > closed, 1, NA))%>%
  filter(is.na(remove)) %>% select(-c(remove, closed)) %>% ungroup()

mwdata_raw1$phone_1 <- ifelse(nchar(mwdata_raw1$phone_1)==9, paste0("0",mwdata_raw1$phone_1), mwdata_raw1$phone_1)
# mwdata_raw <- mwdata_raw %>%
#   dplyr::mutate(priority = ifelse(call_status == 'Completed interview',1,2))
# mwdata_raw <- mwdata_raw[order(mwdata_raw$priority),]
# mwdata_raw1 <- mwdata_raw[!duplicated(mwdata_raw[c('phone_1','caseid','call_num')]),]
# mwdata_raw1 <- rbind(
#   mwdata_raw1 %>%
#     filter(call_status==1 ) %>%
#     filter(!duplicated(c(caseid) )),
#   mwdata_raw1 %>%
#     filter(call_status!=1))


data_clean <- mwdata_raw1 %>% 
  dplyr::filter(users !="") %>%
  dplyr::filter(phone_1 !='') %>%
  dplyr::mutate(Resp.Language = main_language_label_1,
                Resp.Age = e3_1,
                Resp.Consent = C1_1,
                Resp.Region = region_1,
                Resp.DistrictCity = e4_label_1,
                Resp.YNBoma = e4b_1,
                #Resp.Educ = d2_1,
                Resp.Sex = E2_1,
                Phone.duration = phone_call_duration,
                E10_1 = E10_1,
                E12_1 = E12_1,
                d5_1 = d5_1,
                d4_1 = d4_1,
                phone_place=i1_1,
                speaker=i5_1,
                time_preference = i4_1,
                day_preference = i3_1,
                callback_time = callback_time, 
                L2_1 = L2_1,
                l6_1 = l6_1,
                E15_1 = E15_1,
                i3_1 = i3_1,
                L3_1 = L3_1,
                L4_1 = L4_1,
                l7_1 = l7_1,
                E16_1 = E16_1,
                i4_1 = i4_1,
                e3_a_1 = e3_a_1,
                e4b_1 = e4b_1,
                e4c_1 = e4c_1) %>% 
  dplyr::mutate(#Resp.Region = ifelse(Resp.Region == 'Northen Region',1,
                                # ifelse(Resp.Region == 'Central Region',2,
                                #        ifelse(Resp.Region== 'Southern Region',3,''))),
                call_status_label = case_when(call_status == '1' ~ 'Completed interview',
                                                  call_status == '2' ~ 'Incomplete (callback)',
                                                  call_status == '3' ~ 'Partially complete (no callback)',
                                                  call_status == '4' ~ 'Refusal',
                                                  call_status == '5' ~ "No answer",
                                                  call_status == '6' ~ 'Busy tone',
                                                  call_status == '7' ~ "Number not in use (not registered on the network)",
                                                  call_status == '8' ~ 'Number not accessible (number exists, but (temporarily) not reachable)',
                                                  call_status == '9' ~ 'Referral',
                                                  call_status == '10' ~ 'Deferral',
                                                  call_status == '11' ~ 'Ineligible(no refer/ defer)',
                                                  call_status == '12' ~ 'Incomprehensible',
                                                  call_status == '13' ~ 'Answered, but not by the respondent',
                                                  call_status == '14' ~ 'Reassigned to other enumerator (General)',
                                                  call_status == '15' ~ 'Already interviewed (contacted on another SIM)',
                                                  call_status == '16' ~ 'Quota full (available /Defer)',
                                                  call_status == '17' ~ 'Quota full (ineligible)',
                                                  call_status == '18' ~ 'Interview under testing (Demonstration)',
                                                  call_status == '19' ~ 'Reassigned (Need female interviewer)',
                                                  call_status == '111' ~ 'Other (specify)'),
                Resp.Age.grp = cut(as.numeric(Resp.Age), c(17,49,100)),
                Resp.Age.pyr = cut(as.numeric(Resp.Age), c(14,19,29,39,49,59,64)),
                Resp.Educ_lab = case_when(d2_1 == 1 ~ 'Primary',
                                            d2_1 == 2 ~ 'Secondary',
                                            d2_1 == 3 ~ 'Higher',
                                            d1_1 == 0~ 'No education'),
                RuralUrban = ifelse(Resp.YNBoma=='1' | (Resp.DistrictCity %in% c('Blantyre City','Lilongwe City','Mzuzu City','Zomba City')) , 'Urban',
                                    ifelse(Resp.YNBoma == '2','Rural', NA)),
                Resp.Marriage_lab = case_when(b1_1 ==1 ~ 'Married',
                                              b1_1 ==2 ~ 'Cohabiting',
                                              b1_1 ==3 ~ 'Not in Union',
                                              b1_1 ==4 ~ 'Refuse'),
                Resp.Marriage_lab = case_when(b3_1 ==1 ~ 'Widowed/(cohabiting) partner passed away',
                                              b3_1 ==2|b3_1 ==3 ~ 'Divorced/Separated',
                                              Resp.Marriage_lab == 'Cohabiting' | Resp.Marriage_lab == 'Married' ~ 'Married/Cohabiting',
                                              b2_1 == 3 ~ 'Never married',
                                              b3_1 == 4 | b2_1 == 4 ~ 'Refuse'),
                water_source = case_when(b8_1 == 1 ~ 'Piped into dwelling',
                                         b8_1 == 2 ~ 'Piped into yard/plot',
                                         b8_1 == 3 ~ 'Public tap',
                                         b8_1 == 4 ~ 'Tubewell/borehole',
                                         b8_1 == 5 ~ 'Protected well',
                                         b8_1 == 6 ~ 'Unprotected well',
                                         b8_1 == 7 ~ 'Protected spring',
                                         b8_1 == 8 ~ 'Unprotected spring',
                                         b8_1 == 9 ~ 'Rainwater',
                                         b8_1 == 10 ~ 'Bottled water',
                                         b8_1 == 11 ~ 'Cart with small tank',
                                         b8_1 == 12 ~ 'Tank/drum',
                                         b8_1 == 13 ~ 'Tanker truck',
                                         b8_1 == 14 ~ 'Surface water',
                                         b8_1 == 16 ~ 'Refuse',
                                         b8_1 == 111 ~ 'Other'),
                Resp.Age.grp_lab = case_when(Resp.Age.grp== '(17,49]' ~'18-49 yrs',
                                             Resp.Age.grp=='(49,100]' ~ '50-65 yrs'),
                electricity_status_lab = case_when(b7_1 == 1 ~ 'Access to electricity',
                                                   b7_1 == 2 ~'No access to electricity',
                                                   b7_1 == 3 ~ "Don't know",
                                                   b7_1 == 4 ~'Refuse'),
                tech_difficulties = case_when(ea2_1 == 1 ~ 'No difficulties',
                                              ea2_1 == 2 ~ 'Background noise affected my understanding',
                                              ea2_1 == 3 ~ 'Poor call quality/reception',
                                              ea2_1 == 4 ~ 'Difficult communication in the chosen language',
                                              ea2_1 == 111 ~ 'Other'),
                resp_coop = case_when(ea3_1 == 1 ~ 'Good',
                                              ea3_1 == 2 ~ 'OK, but the respondent sounded hesitant, shy, or irritated',
                                              ea3_1 == 3 ~ 'OK, but the respondent was distracted during the interview',
                                              ea3_1 == 4 ~ 'Respondent was not cooperative',
                                              ea3_1 == 111 ~ 'Other'),
                residential_areas = case_when(e4c_1 == 1 ~ 'Village',
                                              e4c_1 == 2 ~ 'Small Town',
                                              e4c_1 == 3 ~ 'Do not Know',
                                              e4c_1 == 4 ~ 'Refuse'),
                call_location = case_when(i1_1 == 1 ~ 'Home',
                                          i1_1 == 2 ~ 'Workplace/School',
                                          i1_1 == 3 ~ 'Other location',
                                          i1_1 == 3 ~ 'On the road',
                                          i1_1 == 4 ~ 'Market',
                                          i1_1 == 5 ~ 'Refuse'),
                on_speaker = case_when(i5_1 == 1 ~ 'Yes',
                                      i5_1 == 0 ~ 'No'),
                phone_ownership = case_when(b4_1 == 1 ~'Owns phone',
                                            b4_1 == 2 ~ 'Belongs to someone else',
                                            b4_1 == 3 ~ 'Refuse'),
                time_preference = case_when(L3_1 == 6 ~ 'No preference',
                                            L3_1 == 0 ~ 'Before 09:00',
                                            L3_1 == 1 ~ '9.00-12.00',
                                            L3_1 == 2 ~ '12.00-14.00',
                                            L3_1 == 3 ~ '14.00-17.00',
                                            L3_1 == 4 ~ '17.00-19.00',
                                            L3_1 == 5 ~ 'After 19:00')) %>%
  dplyr::rename(Vaccine.Dose = cv1_1,
                Vaccine.Likelihood = cv3_1,
                Vaccine.Name = cv2_1
                #Vaccine.Den.Reasons = cv4_1,
                #Other.Den.Reasons = other_den_reasons,
               # Vaccine.Yes.Reasons = cv5_1
                #Other.Yes.Reasons = other_yes_reasons
               
  ) %>% 
  dplyr::mutate(U5 = as.numeric(f_htotal_1), 
                O5 = as.numeric(s_htotal_1),
                O5_missed = as.numeric(hd3b_number_1),
                HHdeaths = as.numeric(hd4_number_1),
                O5 = O5 + O5_missed,
                # U5Death = ifelse(hd6_1_1)
                HHsize = O5 + U5+1)
data_clean$Date.Interview <- gsub('.{11}$', '', data_clean$endtime)

#spsstodate <- function(p) as.Date(p/86400, origin = "1582-10-14")
data_clean2 <- data_clean %>%
  dplyr::filter(!is.na(Date.Interview)) %>%
  dplyr::mutate(#endtime = spsstodate(endtime),
         Date.Interview = endtime,
          Date.Interview1 = as.POSIXct(Date.Interview, format="%h %d, %Y"),
          Date.Interview = format(Date.Interview1, "%Y-%m-%d")
  ) %>%
  #dplyr::select(-Date.Interview1) %>%
  dplyr::mutate(month.interview = lubridate::floor_date(as.Date(Date.Interview), 'month'),
                month.interview = case_when(month.interview == '2021-08-01'~'Aug-21',
                                            month.interview == '2021-09-01'~'Sep-21',
                                            month.interview == '2021-10-01'~'Oct-21',
                                            month.interview == '2021-11-01'~'Nov-21',
                                            month.interview == '2021-12-01'~'Dec-21',
                                            month.interview == '2022-01-01'~'Jan-22',
                                            month.interview == '2022-02-01'~'Feb-22',
                                            month.interview == '2022-03-01'~'Mar-22',
                                            month.interview == '2022-04-01'~'Apr-22',
                                            month.interview == '2022-05-01'~'May-22',
                                            month.interview == '2022-06-01'~'Jun-22',
                                            month.interview == '2022-07-01'~'Jul-22',
                                            month.interview == '2022-08-01'~'Aug-22',
                                            month.interview == '2022-09-01'~'Sep-22',
                                            month.interview == '2022-10-01'~'Oct-22',
                                            month.interview == '2022-11-01'~'Nov-22',
                                            month.interview == '2022-12-01'~'Dec-22',
                                            month.interview == '2023-01-01'~'Jan-23',
                                            month.interview == '2023-02-01'~'Feb-23',
                                            month.interview == '2023-03-01'~'Mar-23',
                                            month.interview == '2023-04-01'~'Apr-23',
                                            month.interview == '2023-05-01'~'May-23',
                                            month.interview == '2023-06-01'~'Jun-23'))

data_clean3 <- data_clean2 %>%
  dplyr::mutate(Resp.Age = as.integer(Resp.Age),
                F.ReproAge = ifelse(Resp.Age <50 & Resp.Sex==2,'Woman <50 years of age',"All other respondents"), 
                Resp.Age.pyr = cut(as.integer(Resp.Age), c(14,19,29,39,49,59,64)),
                Outcome2 = ifelse(call_status == '7', 'NNU', # used to be DYS
                                  ifelse(call_status == "1" & Resp.Consent == 1 & now_complete=='Yes' & !is.na(RuralUrban), 'COMP',
                                         ifelse(call_status == "11"|call_status == "17" , 'INEL',
                                                ifelse((call_status == "2" | call_status == "3"), 'INCO',
                                                       ifelse(call_status == "9",'REFER',
                                                              ifelse(call_status == '4','REFU',
                                                                     ifelse(call_status == '10'|call_status == '16','DEFER',
                                                                            ifelse(call_status == '12'|call_status =='18','LANG', #used to be lang-close
                                                                                   ifelse(call_status == '19'|call_status =='14','REASS', #used to be lang
                                                                                          ifelse(call_status == '6' | call_status == '5', "NR",
                                                                                                 ifelse(call_status == '8', 'NNA', #used to be NO-ACCESS
                                                                                                        ifelse(call_status == "13", 'INCOR',
                                                                                                               ifelse(call_status == '3','PART',
                                                                                                                      'Other'))))))))))))), #oother includes 'Already interviewed'
                phone_1 = as.numeric(phone_1),
                enumerator = case_when(username == 'beatrice' ~ 'E1',
                                       username == 'bertha' ~ 'E2',
                                       username == 'bridget' ~ 'E3',
                                       username == 'carolyn' ~ 'E4',
                                       username == 'christina' ~ 'E5',
                                       username == 'faith' ~ 'E6',
                                       username == 'geoffrey' ~ 'E7',
                                       username == 'gracian' ~ 'E8',
                                       username == 'gracious' ~ 'E9',
                                       username == 'loveness' ~ 'E10',
                                       username == 'lusungu' ~ 'E11',
                                       username == 'mercy' ~ 'E12',
                                       username == 'salome' ~ 'E13',
                                       username == 'smart' ~ 'E14',
                                       username == 'caroline (not yet authenticated)' ~ 'E15',
                                       username == 'andrew' ~ 'E16',
                                       username == 'annie' ~ 'E17',
                                       username == 'caromanjolo' ~ 'E18',
                                       username == 'ethelbanda (not yet authenticated)'|username == 'ethel' ~ 'E19',
                                       username == 'febby' ~ 'E19',
                                       username == 'grace' ~ 'E20',
                                       username == 'jean' ~ 'E21',
                                       username == 'leamsuku' ~ 'E22',
                                       username == 'leamtonda' ~ 'E23',
                                       username == 'ruth' ~ 'E24',
                                       username == 'sumaya' ~ 'E25',
                                       username == 'patricia' ~ 'E26'
                ),
                Resp.Sex = ifelse(Resp.Sex == 1, 'Male',
                                  ifelse(Resp.Sex ==2, 'Female',NA)),
                Resp.Region = ifelse(Resp.Region == 1, 'Northern',
                                     ifelse(Resp.Region ==2, 'Central',
                                            ifelse(Resp.Region==3, 'Southern',NA))))

data_clean3$Outcome2[which(data_clean3$call_status =='COMP' & is.na(data_clean3$Resp.Consent))] <- 'INCO'

# Reassign incomplete cases witha refusal for the initial language question to refused
data_clean3$Outcome2[which(data_clean3$Outcome2=="INCO" & data_clean3$Resp.Language=="Refuse")] <- "REFU"

# Reassign Truly ineligible cases which have not been marked as such appropriately  
data_clean3$Outcome2[which((data_clean3$Outcome2=="REFU"| 
                              data_clean3$Outcome2=="INCO"|
                              data_clean3$Outcome2=="Other") &
                             !is.na(data_clean3$E10_1))] <-"INEL"

# Limit to data cases and those who consent/those who collect in Malawi
Consented <- data_clean3[which(data_clean3$compandcons == 1),]
Consented <- Consented[which(!is.na(Consented$RuralUrban)),]

  # Convert to wide dataset
dat1mw <- data.frame(call_num=data_clean3$call_num,
                     phone_1 = data_clean3$phone_1,
                     Outcome=data_clean3$Outcome2,
                     caseid = data_clean3$caseid,
                     inel.age = data_clean3$E10_1,
                     lang = data_clean3$Resp.Language,
                     call_num_2 = data_clean3$call_num,
                     time = data_clean3$Date.Interview,
                     enum = data_clean3$enumerator,
                     region = data_clean3$Resp.Region,
                     phone_call_duration = data_clean3$phone_call_duration,
                     Resp.Sex = data_clean3$Resp.Sex,
                     E10_1 = data_clean3$E10_1,
                     E12_1 = data_clean3$E12_1,
                     d5_1 = data_clean3$d5_1,
                     d4_1 = data_clean3$d4_1,
                     phone_place=data_clean3$phone_place,
                     speaker=data_clean3$speaker,
                     starttime = data_clean3$starttime,
                     L2_1= data_clean3$L2_1,
                     l6_1 = data_clean3$l6_1,
                     E15_1 = data_clean3$E15_1,
                     i3_1 = data_clean3$i3_1,
                     L3_1= data_clean3$L3_1,
                     l7_1 = data_clean3$l7_1,
                     E16_1 = data_clean3$E16_1,
                     i4_1 = data_clean3$i4_1)

# dat1mw <- dat1mw[!duplicated(dat1mw[c('phone_1','call_num','caseid')]),]
n <- max(dat1mw$call_num)

dat.widemw <- pivot_wider(dat1mw, names_from = call_num,  #values_fn = list,#ifelse(Outcome=='COMP'),
                          values_from = c('Outcome', 'inel.age', 'lang', 'time', 'enum','call_num_2', 'region', 'L2_1',
                                          'l6_1', 'E15_1', 'i3_1', 'L3_1', 'l7_1', 'E16_1', 'i4_1', 'phone_call_duration',
                                          'Resp.Sex', 'E10_1', 'E12_1', 'd5_1', 'd4_1', 'phone_place', 'speaker', 'starttime')) %>%
  #reshape(dat1, idvar = "caseid", timevar = "call_num", direction = "wide")%>%
  # selects the last nonNA outcome for the respondent over the specified columns
  dplyr::mutate(latest = coalesce(Outcome_14, Outcome_13, Outcome_12, Outcome_11, Outcome_10, Outcome_9, Outcome_8, Outcome_7, Outcome_6, Outcome_5, Outcome_4,Outcome_3, Outcome_2, Outcome_1),
                inel.age = coalesce(inel.age_14, inel.age_13, inel.age_12, inel.age_11, inel.age_10, inel.age_9, inel.age_8, inel.age_7, inel.age_6, inel.age_5, inel.age_4,inel.age_3, inel.age_2, inel.age_1),
                lang = coalesce(lang_14, lang_13, lang_12, lang_11, lang_10, lang_9, lang_8, lang_7, lang_6, lang_5, lang_4,lang_3, lang_2, lang_1),
                call_num = coalesce(call_num_2_14, call_num_2_13, call_num_2_12, call_num_2_11, call_num_2_10, call_num_2_9, call_num_2_8, call_num_2_7, call_num_2_6, 
                                    call_num_2_5,call_num_2_4,call_num_2_3, call_num_2_2, call_num_2_1)) %>%
  dplyr::mutate(Outcome.FINAL = case_when(latest == 'COMP'~'COMP',
                                          latest =="REFU" ~ "REFU",
                                          latest == "NNU" ~ "NNU",
                                          latest== "INEL" ~ "INEL",
                                          latest=="DEFER" ~ "DEFER",
                                          latest == 'NR' & Outcome_3 =='NR' ~ 'NR',
                                          latest == 'NNA' & Outcome_3 == 'NNA' ~ "NNA",
                                          latest =="LANG" ~ "LANG",
                                          Outcome_1 == "INCO" | Outcome_2 == "INCO"| Outcome_3 =='INCO'|Outcome_4=='INCO'|Outcome_5 == "INCO"
                                          & ((Outcome_1 != "COMP" & Outcome_2 != "COMP"& Outcome_3 != "COMP" &Outcome_4!='COMP'&Outcome_5!='COMP') | 
                                               Outcome_1 != "REFU" | Outcome_2 != "REFU"| Outcome_3 != "REFU" | Outcome_4 !='REFU'|Outcome_5 !='REFU'|
                                               Outcome_1 != "INEL" | Outcome_2 != "INEL"| Outcome_3 != "INEL" | Outcome_4 !='INEL'|Outcome_5 !='INEL'|
                                               !is.na(inel.age)) ~ "PART",
                                          TRUE ~ "PEND"),
                # --- Coalesce variables so we have complete data in the dat.wide data
                enum = coalesce(enum_1, enum_2, enum_3, enum_4, enum_5,enum_6,enum_7,enum_8,enum_9,enum_10,enum_11,enum_12,enum_13,enum_14),
                time = coalesce(time_14, time_13, time_12, time_11, time_10, time_9, time_8, time_7, time_6, time_5,time_4,time_3, time_2, time_1),
                region = coalesce(region_1, region_2, region_3, region_4,region_5, region_6, region_7, region_8, region_9, region_10,region_11, region_12,region_13, region_14))

Consenteddta <- Consented
names(Consenteddta) <- gsub(x = names(Consenteddta), pattern = ".", replacement = "_")  

write.csv(dat.widemw, paste0(dir.gen2, 'DatWide.csv'))
write.csv(Consented, paste0(dir.gen2, 'ConsentedMW.csv'))
write_csv(Consented, paste0(dir.output, 'ConsentedMW.csv'))
write.csv(data_clean3, paste0(dir.gen2, 'DatLongMW.csv'))
library(haven)
write_dta(Consenteddta, paste0(dir.output, 'ConsentedMW.dta'))

data_clean3_sml <- data_clean3 %>%
  select(c(caseid, enumerator, Date.Interview, month.interview, duration, starts_with('Resp'), RuralUrban, call_num, Outcome2, phone_call_duration,
           F.ReproAge, i1_1,b4_1, Phone.duration, ps1_1_1,ps1_1_2,ps5_year_1_1,ps5_year_1_2,ps6_age_1_1,ps6_age_1_2,HHsize,O5,U5,ps2_years_1_2,ps2_years_1_1,
           ps2_years_1_1,ps2_years_1_2,ssh1_number_1,ssh2_number_1,ssh3_number_1,starts_with('ssh5'),starts_with('ssh6'),starts_with('ssh7'), L2_1, l6_1, E15_1, i3_1))

write.csv(data_clean3_sml, paste0(dir.gen2, 'DatLongMW_sml.csv'))

return(list(Consented, data_clean3, dat.widemw,data_clean3_sml))
}
###########################################################################################################################################################
#
#
#End of data cleaning/wrangling
#
#
#
###########################################################################################################################################################
