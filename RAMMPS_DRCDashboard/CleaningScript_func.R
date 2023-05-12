# Cleaning script
# 3 Nov 2021

function(data_raw, feroxus, ivr){
  
  # Load/install packages 
  pkgs <- c('plyr','tidyverse', 'readxl','date',  'tidyselect', 'httr', 'jsonlite','arrow')
  # ldpkg(pkgs)
  lapply(pkgs, require, character.only = TRUE)
  
  # Read in the data
  dir.input <- "/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/UNIKIN/SurveyCTO Audits/"
  dir.output <- "/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/UNIKIN/SurveyCTO Audits/Clean data/"
  dir.gen <- "/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/UNIKIN/"
  # Directory for DRC Dashboard
  dir.gen2 <- "/Users/lshvb5/Documents/rammps/RAMMPS_DRCDashboard/"
  

  `%notin%` <- Negate(`%in%`)
  
  #######
  # Filtering out observations done for practice 
  data_raw1 <- data_raw %>%
    filter(phone_1 !=244000000000 & phone_1 !=821014960&phone_1 !=817107230 & phone_1 !=812174394 &phone_1 !=826415816& phone_1 !=829742227&
             phone_1 !=995492473 &phone_1 !=818053613& phone_1 !=842287433 & phone_1 !=811551123& phone_1 !=825807368& phone_1 !=896547944
           & phone_1 !=813754456& phone_1 !=817138504& phone_1 !=896477873& phone_1 !=823939751& phone_1 !=858905822& phone_1 !=822700477&
             phone_1 !=842450129& phone_1 !=811883499& phone_1 !=898231370& phone_1 !=907023693) %>%
    filter(!caseid %in% c(12,51,57,56,54,58,53,52,59)) %>%
    filter(!caseid %in% c("MANGOYO CHADDAI","IVONE MUTUZA GLORIA","MULENDA GRACE","BUKINGA FEZA","LELA VALENCIA","MWAVITA AIME","MUTUALE DYNAH","LUBELO ASHIGO CHANCELVIE",
                          "MWAMINI AMISI MARIA","N'SUBI ALIMA HONORINE","YOMBO NGOYI JEANCY","MWAMINI KABALA DONATE")) %>%
    filter(!username %in% c('shammi.luhar2@lshtm.ac.uk','kelly.mccain@lshtm.ac.uk','malebogo.tlhajoane@lshtm.ac.uk','georges.reniers@lshtm.ac.uk'))
  
  
  # Long data with only essential variables -- removing unneeded variables 
  data_raw1 <- data_raw1 %>%
    dplyr::select(vars_select(names(data_raw1), !starts_with('log') & !starts_with('next') & !starts_with('find') 
                              & !starts_with('add') & !starts_with('call_end') & !starts_with('timestamp') &
                                !starts_with('call_info') & !starts_with('length_s') & !starts_with('start_') &
                                !starts_with('end_') & !starts_with('pn') & !starts_with('call_simp') & 
                                !starts_with('all_') & !starts_with('all_pn')&!starts_with('num_') 
    )) %>%
    dplyr::filter(!is.na(devicephonenum))
  
  # adding correctly formatted date of interview variable 
  data_raw1$Date.Interview <- substr(data_raw1$endtime, 1, 12)#gsub('.{11}$', '', data_raw1$endtime)
  
  data_raw1 <- data_raw1 %>%
    dplyr::mutate(Date.Interview1 = ifelse(grepl('2017', Date.Interview), gsub('.{11}$', '', data_raw1$SubmissionDate), Date.Interview)) %>%
    dplyr::mutate(Date.Interview2 = as.POSIXct(Date.Interview1, format="%h %d, %Y"),
                  Date.Interview = format(Date.Interview2, "%Y-%m-%d"),
                  Date.Interview = as.Date(Date.Interview)
                  # Date.Interview = as.POSIXct(Date.Interview, '%Y-%m-%d',tz = "GMT")
    ) #%>%
  
  # making month variable out of new date.interview variable
  data_raw1 <- data_raw1 %>%
    dplyr::mutate(month.interview = lubridate::floor_date(as.Date(Date.Interview), 'month'),#month(Date.Interview),
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
                                              month.interview == '2023-06-01'~'Jun-23'),
                  compandcons = ifelse(call_status == 1 & now_complete=='Yes'& consent ==1, 1,0) # completed and consented?
    ) 
  
  # Treatment of duplicate caseid/phone_1/call_nums
  # first, make date/time variable for end time of call 
  data_raw1$DateTime <- as.POSIXct(data_raw1$endtime, format = "%b %d, %Y %r")
  data_raw1$caseid_original <- data_raw1$caseid
  data_raw1$caseid <- paste0('ID - ',data_raw1$phone_1)
  # then sort by date/time 
  temp <- data_raw1 %>%
    group_by(phone_1) %>%
    dplyr::arrange(as.POSIXct(DateTime), .by_group = TRUE) %>%
    # for situations where there was a referral, if time > time when call_num = 0, then the caseid should be modified to be {caseid}-Refer
    dplyr::mutate(caseid = case_when(
      lag(call_num)== 0~ paste0(caseid, "-Refer"),
      lag(call_num,2)==0~ paste0(caseid, "-Refer"),
      lag(call_num,3)==0~ paste0(caseid, "-Refer"),
      lag(call_num,4)==0~ paste0(caseid, "-Refer"),
      lag(call_num,5)==0~ paste0(caseid, "-Refer"),
      TRUE ~ caseid)) 
  
  
  # re-combine this de-duped consented list with the rest 
  data_raw1 <- rbind(
    temp %>%# if there is a duplicated consented interview, then remove earlier one
      filter(compandcons == 1) %>%
      filter(phone_call_duration !=0) %>%
      group_by(caseid) %>%
      slice_min(DateTime, n = 1, with_ties = FALSE)
    , 
    temp %>% filter(compandcons!=1) # re-add all the other interviews (non-consented)
  ) %>% ungroup() %>%
    # make new call_num which is just numbered observations within group of caseid/phone number
    group_by(caseid) %>%
    dplyr::arrange(DateTime, .by_group = TRUE) %>%
    dplyr::mutate(call_num_old = call_num, 
                  call_num = 1:n()) 
  
  
  
  # here, this removes observations after the case that is supposed to close it 
  data_raw1 <- data_raw1 %>%
    group_by(phone_1) %>%
    mutate(closed = ifelse(call_status %in% c(1, '2a','4a',6, 7, 9,11), call_num,NA)) %>%
    fill(closed) %>%
    group_by(phone_1, caseid) %>%
    mutate(remove = ifelse(call_num > closed, 1, NA)) %>%
    filter(is.na(remove)) %>% select(-c(remove, closed)) %>% ungroup()
  # data_raw1$caseid <- data_raw1$caseid_original
  
  # Change the names of the variables in the dataset to make more readable; 
  data_clean <- data_raw1 %>% 
    dplyr::mutate(region = case_when(as.character(Kinshasa) == '1'~ '1',
                                     as.character(NordKivu) == '1'~'2',
                                     as.character(region) == '3' ~ '3',
                                     as.character(region) == '4' ~ '4',
                                     as.character(region) == '5'~'5',
                                     as.character(region) == as.character(E6) & ((call_status == 1 & consent == 1)
                                                                                 |(as.character(E7) == '1'& E7_b > 90) | (as.character(E7) == '2' & E7_b >3) |
                                                                                   (as.character(E7) == '3' & E7_b >0) | as.character(E7) == '4') ~ as.character(region),
                                     #as.character(region) == '2' & as.character(E6) == '2' & ((as.character(E7) == '1'& E7_b > 90) | (as.character(E7) == '2' & E7_b >3) | (as.character(E7) == '3' & E7_b >0) | as.character(E7) == '4') ~ '2',
                                     as.character(region) != as.character(E6) & ((as.character(E7) == '1'& E7_b > 90) | (as.character(E7) == '2' & E7_b >3) | (as.character(E7) == '3' & E7_b >0) | as.character(E7) == '4') ~ as.character(E6),
                                     as.character(E5) == '3' ~ '3',
                                     is.na(region) ~ '',
                                     TRUE ~ '66'),
                  region = as.integer(region)) %>%
    dplyr::rename(Resp.Language = main_language_label,
                  Resp.Age = E3,
                  Resp.Consent = consent,
                  Resp.Region = region,
                  Resp.Educ = education_level,
                  Resp.Sex = E2,
                  Resp.Marriage = marriage_status) %>%
    dplyr::mutate(Resp.Age = ifelse(!is.na(corr_respagemomalive_1), corr_respagemomalive_1, 
                                    ifelse(!is.na(corr_respagemomdead_1), corr_respagemomdead_1,
                                           ifelse(!is.na(corr_respagedadalive_1), corr_respagedadalive_1,
                                                  ifelse(!is.na(corr_respagedaddead_1), corr_respagedaddead_1, Resp.Age)))),
                  Resp.Region.name = case_when(Resp.Region == '1' ~ 'Kinshasa', Resp.Region == '2' ~ 'Nord Kivu', Resp.Region == '3' ~ 'Other', Resp.Region == '4'~ "Don't Know"),
                  Resp.Sex = ifelse(Resp.Sex == 1, 'Male',
                                    ifelse(Resp.Sex ==2, "Female",NA)),
                  Resp.Age.grp = cut(as.numeric(Resp.Age), c(17,39,100)),
                  Resp.Age.pyr = cut(as.numeric(Resp.Age), c(14,19,29,39,49,59,64))) %>%
    dplyr::mutate(call_status = ifelse((SubmissionDate < 'Sep 8, 2021 3:45:00 PM' & call_status == '2'), '9', call_status),
                  call_status_label_eng = case_when(call_status == '1' ~ 'Completed',
                                                    call_status == '2' ~ 'Incomplete',
                                                    call_status == '2a' ~ 'Partially completed (no callback)',
                                                    call_status == '3' ~ 'Answered, but not by the respondent',
                                                    call_status == '4' ~ "Reassigned (e.g. didn't speak the same language)",
                                                    call_status == '4a' ~ 'Incomprehensible',
                                                    call_status == '9' ~ "Ineligible (no referral)",
                                                    call_status == '5' ~ 'No answer',
                                                    call_status == '6' ~ 'Refusal',
                                                    call_status == '7' ~ 'Number does not work/is not functional',
                                                    call_status == '7a' ~ 'Phone number is functional but inaccessible',
                                                    call_status == '10' ~ 'Busy tone',
                                                    call_status == '8' ~ 'Referral',
                                                    call_status == '11' ~ 'Deferral',
                                                    call_status == 'Oth' ~ 'Other'),
                  enumerator = case_when(username == "drc.5004@pma2020.org" ~ 'E1',
                                         username == "drc.5006@pma2020.org" ~ 'E2',
                                         username == "drc.5008@pma2020.org" ~ 'E3',
                                         username == "drc.5013@pma2020.org" ~ 'E4',
                                         username == "drc.5312@pma2020.org" ~ 'E5',
                                         username == "drc.5432@pma2020.org" ~ 'E6',
                                         username == "drc.5578@pma2020.org" ~ 'E7',
                                         username == "drc.5580@pma2020.org" ~ 'E8',
                                         username == "rammps.5001@gmail.com" ~ 'E9',
                                         username == "rammps.5003@gmail.com" ~ 'E10',
                                         username == "rammps.5004@gmail.com" ~ 'E11',
                                         username == "rammps.5005@gmail.com" ~ 'E12',
                                         username == "rammps.5006@gmail.com" ~ 'E13',
                                         username == "rammps.5007@gmail.com" ~ 'E14',
                                         username == "rammps.5008@gmail.com" ~ 'E15',
                                         username == "rammps.5009@gmail.com" ~ 'E16',
                                         username == "rammps.5010@gmail.com" ~ 'E17',
                                         username == "rammps.5011@gmail.com" ~ 'E18',
                                         username == "rammps.5012@gmail.com" ~ 'E19',
                                         username == "rammps.5013@gmail.com" ~ 'E20') ) %>%
    # adding changes that initially were just in "Consented"
    dplyr::mutate(Sisters = NumberSistersBrothers_1,
                  Brothers = NumberSistersBrothers_Bro,
                  Sis1.Dead = ssh2_died_1,
                  Sis2.Dead = ssh2_died_2,
                  Sis1.Dead.2019 = ssh3_died_1,
                  Sis2.Dead.2019 = ssh3_died_2,
                  Bro1.Dead = ssh2_died_bro,
                  Bro1.Dead.Yr = ssh3_died_bro,
                  M.Dead = ps1_biological_parents_1,
                  M.dead.Yr = ps6_pryearDied_1,
                  M.dead.Age = ps5_p_died_age_1,
                  M.alive.age = ps2_parents_age_1,
                  F.Dead = ps1_biological_parents2_1,
                  F.dead.Yr = ps6_pryearDied2_1,         
                  F.dead.Age = ps5_p_died_age2_1,
                  F.alive.age = ps2_parents_age2_1) %>%
    dplyr::mutate(M.alive.age = ifelse(!is.na(corr_momagealive_1), corr_momagealive_1, M.alive.age),
                  M.alive.age = ifelse(M.alive.age > 130 | M.alive.age < 30, -99, M.alive.age),
                  M.dead.Age = ifelse(!is.na(corr_momagedead_1), corr_momagedead_1, M.dead.Age),
                  F.alive.age = ifelse(!is.na(corr_dadagealive_1), corr_dadagealive_1, F.alive.age),
                  F.alive.age = ifelse(F.alive.age > 130 | F.alive.age < 30, -99, F.alive.age),
                  F.dead.Age = ifelse(!is.na(corr_dadagedead_1), corr_deadagedead_1, F.dead.Age),
                  M.dead.Yr = ifelse(!is.na(corr_momyeardead_1), corr_momyeardead_1, M.dead.Yr),
                  M.dead.Yr = ifelse(M.dead.Yr < 1950, -99, M.dead.Yr),
                  F.dead.Yr = ifelse(!is.na(corr_dadyeardead_1), corr_dadyeardead_1, F.dead.Yr),
                  F.dead.Yr = ifelse(F.dead.Yr < 1950, -99, F.dead.Yr)) %>%
    # modifications for hhsize density and cdr
    dplyr::rename(U5Death = hd4_b,
                  O5Death = hd6_b,
                  U5 = hd2_b,
                  O5 = hd3_b,
                  OwnPhone = b4) %>%
    mutate(O5 = ifelse(is.na(O5),0,as.numeric(O5)), 
           U5 = ifelse(is.na(U5),0,as.numeric(U5)),
           HHsize = as.numeric(U5)+as.numeric(O5)+1,
           Sis1.Dead = ifelse(is.na(Sis1.Dead),0,as.numeric(Sis1.Dead)),
           Bro1.Dead = ifelse(is.na(Bro1.Dead),0,as.numeric(Bro1.Dead)),
           Sib1.Dead = as.numeric(Sis1.Dead) + as.numeric(Bro1.Dead),
           U5Death = ifelse(is.na(U5Death),0,as.numeric(U5Death)),
           O5Death = ifelse(is.na(O5Death),0,as.numeric(O5Death)),
           HHdeathstot = as.numeric(U5Death) + as.numeric(O5Death),
           U5Death.Age = hd5b_0_1,# need multiple for each loop
           O5Death.Age = hd7b_age_1) %>%# need multiple for each loop) 
    # modifications for duration 
    dplyr::rename(Total = duration,
                  Elig.Time = elig_time,
                  Consent.Time = consent_time,
                  Arrangement.Time = arrangements_time,
                  Background.Time = background_time,
                  C19.Time = vax_time,
                  HH.Time = HH_time,
                  PS.Time = PS_time,
                  SSH.Time = SSH_time,
                  DB.Time = DB_time
    ) %>%
    rowwise() %>%
    dplyr::mutate(Phone.duration1= sum(Elig.Time, Consent.Time, Arrangement.Time, Background.Time, C19.Time, HH.Time, PS.Time, SSH.Time, DB.Time, na.rm=T),
                  Phone.duration = ifelse(is.na(Phone.duration1), phone_call_duration, Phone.duration1)) %>%
    ungroup() %>%
    dplyr::rename(Vaccine.Dose = vaccine_dose,
                  Vaccine.Likelihood = dose_frequency,
                  Vaccine.Name = vaccine_names,
                  Vaccine.Den.Reasons = vaccines_den_reasons,
                  Other.Den.Reasons = other_den_reasons,
                  Vaccine.Yes.Reasons = vaccines_yes_reasons,
                  Other.Yes.Reasons = other_yes_reasons
    ) %>%
    dplyr::mutate(S_CD_Preg1 = CD1_copy_1_1,
                  S_CD_Preg2 = CD1_copy_1_2,
                  S_CD_Preg3 = CD1_copy_1_3,
                  S_CD_Preg4 = CD1_copy_1_4,
                  S_CD_Preg5 = CD1_copy_1_5,
                  S_CD_Cbirth1 = CD2_copy_1_1,
                  S_CD_Cbirth2 = CD2_copy_1_2,
                  S_CD_Cbirth3 = CD2_copy_1_3,
                  S_CD_Cbirth4 = CD2_copy_1_4,   
                  S_CD_Cbirth5 = CD2_copy_1_5,  
                  S_CD_2M1 = CD3_copy_1_1,
                  S_CD_2M2 = CD3_copy_1_2,
                  S_CD_2M3 = CD3_copy_1_3,
                  S_CD_2M4 = CD3_copy_1_4,
                  S_CD_2M5 = CD3_copy_1_5) %>%
    dplyr::mutate(M_VisitHF = CD8_1,
                  M_AdmittedHF = CD9_1,
                  M_DieHF = CD10_1,
                  F_VisitHF = CD8_2_1,
                  F_AdmittedHF = CD9_2_1,
                  F_DieHF = CD10_2_1,
                  S_VisitHF1 = CD8_copy_1_1,
                  S_VisitHF2 = CD8_copy_1_2,
                  S_VisitHF3 = CD8_copy_1_3,
                  S_VisitHF4 = CD8_copy_1_4,
                  B_VisitHF1 = CD8_copy_bro_1,
                  B_VisitHF2 = CD8_copy_bro_2,
                  B_VisitHF3 = CD8_copy_bro_3,
                  B_VisitHF4 = CD8_copy_bro_4,
                  B_VisitHF5 = CD8_copy_bro_5,
                  B_VisitHF6 = CD8_copy_bro_6,
                  S_AdmitHF1 = CD9_copy_1_1,
                  S_AdmitHF2 = CD9_copy_1_2,
                  S_AdmitHF3 = CD9_copy_1_3,
                  S_AdmitHF4 = CD9_copy_1_4,
                  B_AdmitHF1 = CD9_copy_bro_1,
                  B_AdmitHF2 = CD9_copy_bro_2,
                  B_AdmitHF3 = CD9_copy_bro_3,
                  B_AdmitHF4 = CD9_copy_bro_4,
                  B_AdmitHF5 = CD9_copy_bro_5,
                  #B_AdmitHF6 = CD9_copy_bro_6,
                  S_DieHF1 = CD10_copy_1_1,
                  S_DieHF2 = CD10_copy_1_2,
                  S_DieHF3 = CD10_copy_1_3,
                  S_DieHF4 = CD10_copy_1_4,
                  B_DieHF1 = CD10_copy_bro_1,
                  B_DieHF2 = CD10_copy_bro_2,
                  B_DieHF3 = CD10_copy_bro_3,
                  B_DieHF4 = CD10_copy_bro_4,
                  B_DieHF5 = CD10_copy_bro_5,
                  #B_DieHF6 = CD10_copy_bro_6
    ) %>%
    dplyr::mutate(F.Dead.COVID = case_when(CD14_2_1 == 1 ~ 'Very likely',
                                           CD14_2_1 ==2 ~ 'Somewhat likely',
                                           CD14_2_1 ==3 ~ 'Somewhat unlikely',
                                           CD14_2_1 ==4 ~ 'Very unlikely',
                                           CD14_2_1 ==5 ~ 'Dont know',
                                           CD14_2_1 =="R" ~ 'R'),
                  F.Dead.Viol = case_when(CD5_2_1 == 1 ~ 'Y',
                                          CD5_2_1 ==2 ~ 'N',
                                          CD5_2_1 ==3 ~ 'Dk',
                                          CD5_2_1 =="R" ~ 'R'),
                  F.Dead.Acc = case_when(CD4_2_1 == 1 ~ 'Y',
                                         CD4_2_1 ==2 ~ 'N',
                                         CD4_2_1 ==3 ~ 'Dk',
                                         CD4_2_1 =="R" ~ 'R')) %>%
    dplyr::mutate(F.Dead.External = if_else(F.Dead.Viol=="Y"|F.Dead.Acc=="Y", "Y", "N")) %>%
    dplyr::mutate(M.Dead.COVID = case_when(CD14_1 == 1 ~ 'Very likely',
                                           CD14_1 ==2 ~ 'Somewhat likely',
                                           CD14_1 ==3 ~ 'Somewhat unlikely',
                                           CD14_1 ==4 ~ 'Very unlikely',
                                           CD14_1 ==5 ~ 'Dont know',
                                           CD14_1 =="R" ~ 'R'),
                  M.Dead.Viol = case_when(CD5_1 == 1 ~ 'Y',
                                          CD5_1 ==2 ~ 'N',
                                          CD5_1 ==3 ~ 'Dk',
                                          CD5_1 =="R" ~ 'R'),
                  M.Dead.Acc = case_when(CD4_1 == 1 ~ 'Y',
                                         CD4_1 ==2 ~ 'N',
                                         CD4_1 ==3 ~ 'Dk',
                                         CD4_1 =="R" ~ 'R'),
                  M.Dead.Preg = case_when(CD1_1 == 1 ~ 'Y',
                                          CD1_1 ==2 ~ 'N',
                                          CD1_1 ==3 ~ 'Dk',
                                          CD1_1 =="R" ~ 'R'),
                  M.Dead.CB = case_when(CD2_1 == 1 ~ 'Y',
                                        CD2_1 ==2 ~ 'N',
                                        CD2_1 ==3 ~ 'Dk',
                                        CD2_1 =="R" ~ 'R'),
                  M.Dead.post.Preg = case_when(CD3_1 == 1 ~ 'Y',
                                               CD3_1 ==2 ~ 'N',
                                               CD3_1 ==3 ~ 'Dk',
                                               CD3_1 =="R" ~ 'R')) %>%
    dplyr::mutate(M.Dead.External = if_else(M.Dead.Viol=="Y"|M.Dead.Acc=="Y", "Y", "N")) %>%
    dplyr::mutate(M.Dead.mat = if_else(M.Dead.CB=="Y"|M.Dead.Preg=="Y"|M.Dead.post.Preg=="Y", "Y", "N")) %>%
    dplyr::mutate(Vaccine.Name = case_when(Vaccine.Name == 1 ~ 'AstraZeneca',
                                           Vaccine.Name == 2 ~ 'Pfizer-BioNTech',
                                           Vaccine.Name == 3 ~ 'Moderna',
                                           Vaccine.Name == 4 ~ "Johnson & Johnson's Janssen",
                                           Vaccine.Name == 5 ~ "Don't Know",
                                           Vaccine.Name == 6 ~ 'Sinovac',
                                           Vaccine.Name == 7 ~ 'Sputnik',
                                           Vaccine.Name == 'R' ~ 'Refuse'
    ),
    Vaccine.Dose = case_when(Vaccine.Dose == 1 ~ 'One Dose',
                             Vaccine.Dose == 2 ~ 'Two Doses',
                             Vaccine.Dose == 3 ~ 'No vaccine',
                             Vaccine.Dose == 4 ~ "Don't Know",
                             is.na(Vaccine.Dose) ~ 'Refuse'
    )
    ) %>%
    dplyr::mutate(Resp.Region.name = case_when(Resp.Region == 1 ~ 'Kinshasa', 
                                               Resp.Region == 2 ~ 'Nord Kivu', 
                                               Resp.Region == 3 ~ 'Other', 
                                               Resp.Region == 4~ "Don't Know"),
                  OwnPhone = case_when(OwnPhone ==1 ~ "Owns phone",
                                       OwnPhone ==2 ~ "Belongs to somebody else",
                                       OwnPhone ==4 ~ "Refuse to answer")) %>%
    dplyr::mutate(F.Dead.setting.death = case_when(CD16_2_1 == 1 ~ 'Health Facility',
                                                   CD16_2_1 == 2  ~ 'Home',
                                                   CD16_2_1 == 3  ~ 'On road to health facility',
                                                   CD16_2_1 == 4  ~ 'Other',
                                                   CD16_2_1 == 5  ~ 'DK',
                                                   is.na(CD16_2_1)   ~ 'missing'),
                  F.Dead.Buried.Prov = case_when(CD17_2_1 == 1 ~ 'Kinshasa',
                                                 CD17_2_1 == 2  ~ 'NK',
                                                 CD17_2_1 == 3  ~ 'Dk',
                                                 CD17_2_1 == 4  ~ 'Refuse',
                                                 is.na(CD17_2_1)   ~ 'missing'),
                  F.Dead.Buried.plot = case_when(CD18_2_1 == 1 ~ 'Cemetery',
                                                 CD18_2_1 == 2  ~ 'Family plot',
                                                 CD18_2_1 == 3  ~ 'Was not buried',
                                                 CD18_2_1 == 4  ~ 'Other',
                                                 CD18_2_1 == 5  ~ 'DK',
                                                 CD18_2_1 == "R"  ~ 'R',
                                                 is.na(CD18_2_1)   ~ 'missing'),
                  F.Dead.Location.Death = case_when(CD15_2_1 == 1 ~ 'Kinshasa',
                                                    CD15_2_1 == 2  ~ 'NK',
                                                    CD15_2_1 == 3  ~ 'Dk',
                                                    CD15_2_1 == 4  ~ 'Refuse',
                                                    is.na(CD15_2_1)   ~ 'missing'),
                  F.Dead.LiveB4 = case_when(Live_before_passing2_1 == 1 ~ 'Same HH',
                                            Live_before_passing2_1 == 2  ~ 'Same Village/town',
                                            Live_before_passing2_1 == 3  ~ 'Same Prov',
                                            Live_before_passing2_1 == 4  ~ 'Diff Prov',
                                            Live_before_passing2_1 == "R"  ~ 'Refuse',
                                            is.na(Live_before_passing2_1)   ~ 'missing'),
                  F.Dead.Who.buried = case_when(CD19_2_1 == 1 ~ 'Health personnel',
                                                CD19_2_1 == 2  ~ 'Family members only',
                                                CD19_2_1 == 3  ~ 'Both Family members plus selected people from community',
                                                CD19_2_1 == 4  ~ 'Dont know',
                                                CD19_2_1 == "R"  ~ 'Refuse',
                                                is.na(CD19_2_1)   ~ 'missing'),
                  F.Dead.Local.auth = case_when(CD20_2_1 == 1 ~ 'Y',
                                                CD20_2_1 ==2 ~ 'N',
                                                CD20_2_1 ==3 ~ 'Dk',
                                                CD20_2_1 =="R" ~ 'R')) %>%
    dplyr:: mutate(M.Dead.setting.death = case_when(CD16_1 == 1 ~ 'Health Facility',
                                                    CD16_1 == 2  ~ 'Home',
                                                    CD16_1 == 3  ~ 'On road to health facility',
                                                    CD16_1 == 4  ~ 'Other',
                                                    CD16_1 == 5  ~ 'DK',
                                                    is.na(CD16_1)   ~ 'missing'),
                   M.Dead.Buried.Prov = case_when(CD17_1 == 1 ~ 'Kinshasa',
                                                  CD17_1 == 2  ~ 'NK',
                                                  CD17_1 == 3  ~ 'Dk',
                                                  CD17_1 == 4  ~ 'Refuse',
                                                  is.na(CD17_1)   ~ 'missing'),
                   M.Dead.Buried.plot = case_when(CD18_1 == 1 ~ 'Cemetery',
                                                  CD18_1 == 2  ~ 'Family plot',
                                                  CD18_1 == 3  ~ 'Was not buried',
                                                  CD18_1 == 4  ~ 'Other',
                                                  CD18_1 == 5  ~ 'DK',
                                                  CD18_1 == "R"  ~ 'R',
                                                  is.na(CD18_1)   ~ 'missing'),
                   M.Dead.Location.Death = case_when(CD15_1 == 1 ~ 'Kinshasa',
                                                     CD15_1 == 2  ~ 'NK',
                                                     CD15_1 == 3  ~ 'Dk',
                                                     CD15_1 == 4  ~ 'Refuse',
                                                     is.na(CD15_1)   ~ 'missing'),
                   M.Dead.LiveB4 = case_when(Live_before_passing_1 == 1 ~ 'Same HH',
                                             Live_before_passing_1 == 2  ~ 'Same Village/town',
                                             Live_before_passing_1 == 3  ~ 'Same Prov',
                                             Live_before_passing_1 == 4  ~ 'Diff Prov',
                                             Live_before_passing_1 == "R"  ~ 'Refuse',
                                             is.na(Live_before_passing_1)   ~ 'missing'),
                   M.Dead.Who.buried = case_when(CD19_1 == 1 ~ 'Health personnel',
                                                 CD19_1 == 2  ~ 'Family members only',
                                                 CD19_1 == 3  ~ 'Both Family members plus selected people from community',
                                                 CD19_1 == 4  ~ 'Dont know',
                                                 CD19_1 == "R"  ~ 'Refuse',
                                                 is.na(CD19_1)   ~ 'missing'),
                   M.Dead.Local.auth = case_when(CD20_1 == 1 ~ 'Y',
                                                 CD20_1 ==2 ~ 'N',
                                                 CD20_1 ==3 ~ 'Dk',
                                                 CD20_1 =="R" ~ 'R')) %>%
    dplyr:: mutate(M.Dead.C19.Symp = if_else(CD7_1_1!="", "Y", "N"),                                           
                   F.Dead.C19.Symp = if_else(CD7_2_1!="", "Y", "N")) %>%
    dplyr::mutate(water_source_lab = case_when(water_source =="1" ~ 'Piped into dwelling',
                                               water_source =="2" ~ 'Piped to yard/plot',
                                               water_source =="3" ~ 'Public tap',
                                               water_source =="4" ~ 'Tubewell/Borehole',
                                               water_source =="5" ~ 'Protected well',
                                               water_source =="6" ~ 'Unprotected well',
                                               water_source =="7" ~ 'Protected spring',
                                               water_source =="8" ~ 'Unprotected spring',
                                               water_source =="9" ~ 'Rainwater',
                                               water_source =="10" ~ 'Bottled water',
                                               water_source =="11" ~ 'Cart with small tank',
                                               water_source =="12" ~ 'Tank/Drum',
                                               water_source =="13" ~ 'Tanker-truck',
                                               water_source =="14" ~ 'Surface water',
                                               water_source =="15" ~ 'Other',
                                               water_source == 'R' ~ 'Refuse')) %>%
    dplyr::mutate(Resp.Age.grp_lab = case_when(Resp.Age.grp =="(17,40]" ~ '18-39',
                                               Resp.Age.grp =="(40,100]" ~ '40+'),
                  Resp.Region_lab = case_when(Resp.Region =="1" ~ 'Kinshasa',
                                              Resp.Region =="2" ~ 'Nord Kivu'),
                  Resp.Educ_lab = case_when(Resp.Educ =="" ~ 'No Education',
                                            Resp.Educ =="PR" ~ 'Primary',
                                            Resp.Educ =="SE" ~ 'Secondary',
                                            Resp.Educ =="HI" ~ 'Higher',
                                            Resp.Educ == 4 ~ 'Refuse'),
                  #Resp.Educ_lab = ordered(Resp.Educ_lab, levels = c('No Education','Primary','Secondary','Higher','Refuse')),
                  Resp.Marriage_lab = case_when(Resp.Marriage ==1 ~ 'Married',
                                                Resp.Marriage ==2 ~ 'Cohabiting',
                                                Resp.Marriage ==3 ~ 'Not in Union',
                                                Resp.Marriage ==4 ~ 'Refuse'),
                  Resp.Marriage_lab = case_when(marital_status ==1 ~ 'Widowed/(cohabiting) partner passed away',
                                                marital_status ==2 ~ 'Divorced/Separated',
                                                Resp.Marriage_lab == 'Cohabiting' | Resp.Marriage_lab == 'Married' ~ 'Married/Cohabiting',
                                                marriage_formal == 3 ~ 'Never married',
                                                marital_status == 4 | marriage_formal == 4 ~ 'Refuse'),
                  E4a_lab = case_when(E4a ==1 ~ 'City',
                                      E4a ==2 ~ 'Town/Trading Centre',
                                      E4a ==3 ~ 'Rural',
                                      E4a ==4 ~ 'Refuse'),
                  electricity_status_lab = case_when(electricity_status =="1" ~ 'Access to electricity',
                                                     electricity_status =="2" ~ 'No access to electricity',
                                                     electricity_status == '3' ~ "Don't know",
                                                     electricity_status == 'R' ~ 'Refuse')) %>%
    dplyr::mutate(Education.Prim = case_when(highest_grades_a ==10 ~ "less than one year",
                                             highest_grades_a ==11 ~ "Première",
                                             highest_grades_a ==12 ~ "Deuxième",
                                             highest_grades_a ==13 ~ "Troisième",
                                             highest_grades_a ==14 ~ "Quatrième",
                                             highest_grades_a ==15 ~ "Cinquième",
                                             highest_grades_a ==16 ~ "Sixième",
                                             highest_grades_a ==98 ~ "Don't know", 
                                             highest_grades_a == -99 ~ "Prefer not to say"),
                  Education.Sec = case_when(highest_grades_b ==20 ~ "less than one year",
                                            highest_grades_b ==21 ~ "Première",
                                            highest_grades_b ==22 ~ "Deuxième",
                                            highest_grades_b ==23 ~ "Troisième",
                                            highest_grades_b ==24 ~ "Quatrième",
                                            highest_grades_b ==25 ~ "Cinquième",
                                            highest_grades_b ==26 ~ "Sixième",
                                            highest_grades_b ==98 ~ "Don't know", 
                                            highest_grades_b == -99 ~ "Prefer not to say"),
                  Education.Hig = case_when(highest_grades_c ==30 ~ "less than one year",
                                            highest_grades_c ==31 ~ "1er Graduat",
                                            highest_grades_c ==32 ~ "2ème Graduat",
                                            highest_grades_c ==33 ~ "3ème Graduat",
                                            highest_grades_c ==34 ~ "1ère Licence",
                                            highest_grades_c ==35 ~ "2ème Licence",
                                            highest_grades_c ==98 ~ "Don't know", 
                                            highest_grades_c == -99 ~ "Prefer not to say")) %>%
    # filter(call_num %notin% c(0,6)) %>%
    dplyr::mutate(Roof = case_when(roofing_material =="1" ~ 'No roof',
                                   roofing_material =="2" ~ 'Grass/Thatch/Palm/Straw',
                                   roofing_material =="3" ~ 'Cardboard',
                                   roofing_material =="4" ~ 'Plastic/Tarpaulin',
                                   roofing_material =="5" ~ 'Wood',
                                   roofing_material =="6" ~ 'Metal (corrugated iron)',
                                   roofing_material =="7" ~ 'Cement/Tiles',
                                   roofing_material =="8" ~ 'Other',
                                   roofing_material =="9" ~ 'Dont Know',
                                   roofing_material =="10" ~ 'Refuse')) %>%
    dplyr::mutate(Location_call = case_when(call_location =="1" ~ 'Home',
                                            call_location =="2" ~ 'Work/Uni',
                                            call_location =="3" ~ 'Other',
                                            call_location =="4" ~ 'Refuse'),
                  agegp = cut(Resp.Age,
                              breaks=c(17, 30, 40, 50, 60, 70),
                              labels=c("18-29","30-39", "40-49", "50-59", "60+")))
  
  
  #selecting only needed variables
  data_clean2 <- data_clean %>%
    select(c(#MISC
      SubmissionDate, starttime, endtime, Date.Interview, month.interview, call_num, caseid, phone_1, enumerator, now_complete, Total,Elig.Time,
      Consent.Time,Arrangement.Time,
      Background.Time,C19.Time,HH.Time,PS.Time,SSH.Time,DB.Time,Phone.duration,
      Elig, NordKivu, Kinshasa,  A18to39, A40to64, to_be_deferred,   pull_label, pull_IVRprovince,
      #INTRO
      response_1, Resp.Language,L1_A_1, L1_B_1, L2_1, Resp.Sex, E4a, Resp.Region,Resp.Region.name,E5, E6, E7, E7_b,Resp.Age, agegp, Resp.Age.grp,Resp.Age.pyr,
      E10, E9_1, E12, L2, L3, E15, E16, time_availability2, time_availability2_1,
      #CONSENT+INTERVIEW
      Resp.Consent,Location_call, specify_location, continue_discontinue, SpeakerPhone,
      #BACKGROUND
      school_attendance, Resp.Educ,Education.Prim,highest_grades_a, Education.Sec, highest_grades_b, Education.Hig, highest_grades_c, Resp.Marriage, 
      marriage_formal, marital_status, Resp.Marriage_lab, OwnPhone, b5, Roof, electricity_status_lab, water_source_lab, other_waterSource,
      #COVID VAX
      Vaccine.Dose, Vaccine.Name, Vaccine.Likelihood, Vaccine.Den.Reasons, vaccines_den_reasons_1, vaccines_den_reasons_2,vaccines_den_reasons_3,
      vaccines_den_reasons_4,vaccines_den_reasons_5,vaccines_den_reasons_6,vaccines_den_reasons_7,vaccines_den_reasons_8, vaccines_den_reasons_9,
      vaccines_den_reasons_10,vaccines_den_reasons_11,vaccines_den_reasons_12,vaccines_den_reasons_13,vaccines_den_reasons_14, vaccines_den_reasons_15,
      vaccines_den_reasons_16,vaccines_den_reasons_17, vaccines_den_reasons_18,vaccines_den_reasons_19, vaccines_den_reasons_20, vaccines_den_reasons_R, 
      Other.Den.Reasons, Vaccine.Yes.Reasons,vaccines_yes_reasons_1,vaccines_yes_reasons_2,vaccines_yes_reasons_3,vaccines_yes_reasons_4,
      vaccines_yes_reasons_5,vaccines_yes_reasons_6,vaccines_yes_reasons_7,vaccines_yes_reasons_8,vaccines_yes_reasons_9,vaccines_yes_reasons_10,
      vaccines_yes_reasons_11, vaccines_yes_reasons_12,vaccines_yes_reasons_13,vaccines_yes_reasons_14,vaccines_yes_reasons_15,vaccines_yes_reasons_R,
      Other.Yes.Reasons,
      #HH
      hd1_householdrel, U5, O5, HHsize, hd4, U5Death, hd5_a0_1, hd5_a0_2, hd5_a_1, hd5_a_2, hd5b_1, hd5b_2, hd5b_0_1, hd5b_0_2, hd6, O5Death, 
      starts_with('hd7'),
      O5Death.Age,HHdeathstot,
      #MOTHER
      M.Dead, M.alive.age,note_momagealive_1,mistake_momalive_1,corr_respagemomalive_1,corr_momagealive_1, ps3_parent_home_1, OtherLoc_copy_1,
      ps4_parents_jab_1, M.dead.Yr, mom2019die_1, momspecificyear_1, momdie10yrs_1, ps7_months_1,ps5_p_died_age.cat_1, M.dead.Age, note_momagedead_1,
      mistake_momdead_1, corr_respagemomdead_1, corr_momagedead_1,corr_momyeardead_1, M.Dead.LiveB4, OtherLoc_1, CD1_1, CD2_1, CD3_1, CD4_1,
      CD5_1, CD6_1, CD7_1, CD7_1_1,CD7_2_1,CD7_3_1,CD7_4_1,CD7_5_1,CD7_6_1,CD7_7_1,CD7_8_1,CD7_9_1,CD7_10_1,CD7_11_1,CD7_12_1,CD7_R_1, CD7other_1,
      CD8_1, CD9_1, CD11_1, CD12_1, CD13_1, CD14_1, CD16_1, CD18_1, CD19_1, CD20_1,
      #FATHER
      F.Dead, F.alive.age, note_dadagealive_1, mistake_dadalive_1, corr_respagedadalive_1, corr_dadagealive_1, ps3_parent_home2_1, OtherLoc_copy2_1,
      ps4_parents_jab2_1, F.dead.Yr, dad2019die_1, dadspecificyear_1, daddie10yrs_copy_1, ps7_months2_1, ps5_p_died_age.cat2_1, F.dead.Age, note_dadagedead_1,
      mistake_daddead_1, corr_respagedaddead_1, corr_dadagedead_1, corr_dadyeardead_1, F.Dead.LiveB4, OtherLoc2_1, CD4_2_1, CD5_2_1, CD6_2_1,
      CD7_2_1, CD7_2_1_1,CD7_2_2_1,CD7_2_3_1,CD7_2_4_1,CD7_2_5_1,CD7_2_6_1,CD7_2_7_1,CD7_2_8_1,CD7_2_9_1, CD7_2_10_1,CD7_2_11_1, CD7_2_12_1,
      CD7_2_R_1, CD7_2other_1, CD8_2_1, CD9_2_1, CD11_2_1, CD12_2_1, CD13_2_1, CD14_2_1, CD16_2_1, CD18_2_1, CD19_2_1, CD20_2_1,
      #SISTERS
      Sisters,ssh2_died_1, ssh3_died_1, ssh4_yearDied_copy_1_1,ssh4_yearDied_copy_1_2, ssh4_yearDied_copy_1_3, ssh4_yearDied_copy_1_4,
      ssh4_yearDied_copy_1_5,ssh4_months_copy_1_1, ssh4_months_copy_1_2, ssh4_months_copy_1_3, ssh4_months_copy_1_4, ssh4_months_copy_1_5,
      CD1_copy_1_1,CD1_copy_1_2,CD1_copy_1_3,CD1_copy_1_4,CD1_copy_1_5, 
      CD2_copy_1_1, CD2_copy_1_2, CD2_copy_1_3, CD2_copy_1_4, CD2_copy_1_5,
      CD3_copy_1_1,CD3_copy_1_2,CD3_copy_1_3,CD3_copy_1_4,CD3_copy_1_5, 
      CD4_copy_1_1,CD4_copy_1_2,CD4_copy_1_3,CD4_copy_1_4,CD4_copy_1_5,
      CD5_copy_1_1, CD5_copy_1_2, CD5_copy_1_3,CD5_copy_1_4,CD5_copy_1_5, 
      CD6_copy_1_1, CD6_copy_1_2, CD6_copy_1_3,CD6_copy_1_4,CD6_copy_1_5,
      CD7_copy_1_1, CD7_copy_1_2, CD7_copy_1_3, CD7_copy_1_4, CD7_copy_1_5, 
      CD7_copy_1_1_1, CD7_copy_1_1_2,CD7_copy_1_1_3,CD7_copy_1_1_4,CD7_copy_1_1_5,
      CD7_copy_2_1_1,CD7_copy_2_1_2,CD7_copy_2_1_3,CD7_copy_2_1_4,CD7_copy_2_1_5,
      CD7_copy_3_1_1,CD7_copy_3_1_2, CD7_copy_3_1_3, CD7_copy_3_1_4,CD7_copy_3_1_5,
      CD7_copy_4_1_1, CD7_copy_4_1_2, CD7_copy_4_1_3,CD7_copy_4_1_4, CD7_copy_4_1_5,
      CD7_copy_5_1_1, CD7_copy_5_1_2,CD7_copy_5_1_3, CD7_copy_5_1_4,CD7_copy_5_1_5,
      CD7_copy_6_1_1,CD7_copy_6_1_2,CD7_copy_6_1_3,CD7_copy_6_1_4,CD7_copy_6_1_5,
      CD7_copy_7_1_1,CD7_copy_7_1_2,CD7_copy_7_1_3,CD7_copy_7_1_4,CD7_copy_7_1_5,
      CD7_copy_8_1_1,CD7_copy_8_1_2,CD7_copy_8_1_3,CD7_copy_8_1_4,CD7_copy_8_1_5,
      CD7_copy_9_1_1,CD7_copy_9_1_2,CD7_copy_9_1_3,CD7_copy_9_1_4,CD7_copy_9_1_5,
      CD7_copy_10_1_1,CD7_copy_10_1_2,CD7_copy_10_1_3,CD7_copy_10_1_4,CD7_copy_10_1_5,
      CD7_copy_11_1_1,CD7_copy_11_1_2,CD7_copy_11_1_3,CD7_copy_11_1_4,CD7_copy_11_1_5,
      CD7_copy_12_1_1,CD7_copy_12_1_2,CD7_copy_12_1_3,CD7_copy_12_1_4,CD7_copy_12_1_5,
      CD7_copy_R_1_1,CD7_copy_R_1_2,CD7_copy_R_1_3,CD7_copy_R_1_4,CD7_copy_R_1_5,
      CD7_copyother_1_1,CD7_copyother_1_2,CD7_copyother_1_3,CD7_copyother_1_4,CD7_copyother_1_5,
      CD8_copy_1_1,CD8_copy_1_2,CD8_copy_1_3,CD8_copy_1_4,CD8_copy_1_5,
      CD9_copy_1_1,CD9_copy_1_2,CD9_copy_1_3,CD9_copy_1_4,CD9_copy_1_5,
      CD11_copy_1_1,CD11_copy_1_2,CD11_copy_1_3,CD11_copy_1_4,CD11_copy_1_5,
      CD12_copy_1_1,CD12_copy_1_2,CD12_copy_1_3,CD12_copy_1_4,CD12_copy_1_5,
      CD13_copy_1_1,CD13_copy_1_2,CD13_copy_1_3,CD13_copy_1_4,CD13_copy_1_5,
      CD14_copy_1_1,CD14_copy_1_2,CD14_copy_1_3,CD14_copy_1_4,CD14_copy_1_5,
      CD16_copy_1_1,CD16_copy_1_2,CD16_copy_1_3,CD16_copy_1_4,CD16_copy_1_5,
      CD18_copy_1_1,CD18_copy_1_2,CD18_copy_1_3,CD18_copy_1_4,CD18_copy_1_5,
      CD19_copy_1_1,CD19_copy_1_2,CD19_copy_1_3,CD19_copy_1_4,CD19_copy_1_5,
      CD20_copy_1_1,CD20_copy_1_2,CD20_copy_1_3,CD20_copy_1_4,CD20_copy_1_5,
      #BROTHERS
      Brothers,ssh2_died_bro, ssh3_died_bro, ssh4_yearDied_bro_1,ssh4_yearDied_bro_2, ssh4_yearDied_bro_3, ssh4_yearDied_bro_4,
      ssh4_yearDied_bro_5,ssh4_yearDied_bro_6, 
      ssh4_months_bro_1, ssh4_months_bro_2, ssh4_months_bro_3, ssh4_months_bro_4, ssh4_months_bro_5,ssh4_months_bro_6,
      CD4_copy_bro_1,CD4_copy_bro_2,CD4_copy_bro_3,CD4_copy_bro_4,CD4_copy_bro_5,CD4_copy_bro_5,CD4_copy_bro_6,
      CD5_copy_bro_1, CD5_copy_bro_2, CD5_copy_bro_3,CD5_copy_bro_4,CD5_copy_bro_5,CD5_copy_bro_5, CD5_copy_bro_6,
      CD6_copy_bro_1, CD6_copy_bro_2, CD6_copy_bro_3,CD6_copy_bro_4,CD6_copy_bro_5,CD6_copy_bro_6,
      CD7_copy_bro_1, CD7_copy_bro_2, CD7_copy_bro_3, CD7_copy_bro_4, CD7_copy_bro_5, CD7_copy_bro_6,
      CD7_copy_bro_1_1, CD7_copy_bro_1_2,CD7_copy_bro_1_3,CD7_copy_bro_1_4,CD7_copy_bro_1_5,CD7_copy_bro_1_6,
      CD7_copy_bro_2_1,CD7_copy_bro_2_2,CD7_copy_bro_2_3,CD7_copy_bro_2_4,CD7_copy_bro_2_5,CD7_copy_bro_2_6,
      CD7_copy_bro_3_1,CD7_copy_bro_3_2, CD7_copy_bro_3_3, CD7_copy_bro_3_4,CD7_copy_bro_3_5,CD7_copy_bro_3_6,
      CD7_copy_bro_4_1, CD7_copy_bro_4_2, CD7_copy_bro_4_3,CD7_copy_bro_4_4, CD7_copy_bro_4_5,CD7_copy_bro_4_6,
      CD7_copy_bro_5_1, CD7_copy_bro_5_2,CD7_copy_bro_5_3, CD7_copy_bro_5_4,CD7_copy_bro_5_5,CD7_copy_bro_5_6,
      CD7_copy_bro_6_1,CD7_copy_bro_6_2,CD7_copy_bro_6_3,CD7_copy_bro_6_4,CD7_copy_bro_6_5,CD7_copy_bro_6_6,
      CD7_copy_bro_7_1,CD7_copy_bro_7_2,CD7_copy_bro_7_3,CD7_copy_bro_7_4,CD7_copy_bro_7_5,CD7_copy_bro_7_6,
      CD7_copy_bro_8_1,CD7_copy_bro_8_2,CD7_copy_bro_8_3,CD7_copy_bro_8_4,CD7_copy_bro_8_5,CD7_copy_bro_8_6,
      CD7_copy_bro_9_1,CD7_copy_bro_9_2,CD7_copy_bro_9_3,CD7_copy_bro_9_4,CD7_copy_bro_9_5,CD7_copy_bro_9_6,
      CD7_copy_bro_10_1,CD7_copy_bro_10_2,CD7_copy_bro_10_3,CD7_copy_bro_10_4,CD7_copy_bro_10_5,CD7_copy_bro_10_6,
      CD7_copy_bro_11_1,CD7_copy_bro_11_2,CD7_copy_bro_11_3,CD7_copy_bro_11_4,CD7_copy_bro_11_5,CD7_copy_bro_11_6,
      CD7_copy_bro_12_1,CD7_copy_bro_12_2,CD7_copy_bro_12_3,CD7_copy_bro_12_4,CD7_copy_bro_12_5,CD7_copy_bro_12_6,
      CD7_copy_bro_R_1,CD7_copy_bro_R_2,CD7_copy_bro_R_3,CD7_copy_bro_R_4,CD7_copy_bro_R_5,CD7_copy_bro_R_6,
      CD7_copy_broother_1,CD7_copy_broother_2,CD7_copy_broother_3,CD7_copy_broother_4,CD7_copy_broother_5,CD7_copy_broother_6,
      CD8_copy_bro_1,CD8_copy_bro_2,CD8_copy_bro_3,CD8_copy_bro_4,CD8_copy_bro_5,CD8_copy_bro_6,
      CD9_copy_bro_1,CD9_copy_bro_2,CD9_copy_bro_3,CD9_copy_bro_4,CD9_copy_bro_5,CD9_copy_bro_6,
      CD11_copy_bro_1,CD11_copy_bro_2,CD11_copy_bro_3,CD11_copy_bro_4,CD11_copy_bro_5,CD11_copy_bro_6,
      CD12_copy_bro_1,CD12_copy_bro_2,CD12_copy_bro_3,CD12_copy_bro_4,CD12_copy_bro_5,CD12_copy_bro_6,
      CD13_copy_bro_1,CD13_copy_bro_2,CD13_copy_bro_3,CD13_copy_bro_4,CD13_copy_bro_5,CD13_copy_bro_6,
      CD14_copy_bro_1,CD14_copy_bro_2,CD14_copy_bro_3,CD14_copy_bro_4,CD14_copy_bro_5,CD14_copy_bro_6,
      CD16_copy_bro_1,CD16_copy_bro_2,CD16_copy_bro_3,CD16_copy_bro_4,CD16_copy_bro_5,CD16_copy_bro_6,
      CD18_copy_bro_1,CD18_copy_bro_2,CD18_copy_bro_3,CD18_copy_bro_4,CD18_copy_bro_5,CD18_copy_bro_6,
      CD19_copy_bro_1,CD19_copy_bro_2,CD19_copy_bro_3,CD19_copy_bro_4,CD19_copy_bro_5,CD19_copy_bro_6,
      CD20_copy_bro_1,CD20_copy_bro_2,CD20_copy_bro_3,CD20_copy_bro_4,CD20_copy_bro_5,CD20_copy_bro_6,
      #DEBRIEF
      D1, call_status, call_status_label_eng, call_feedback, call_feedback_1,call_feedback_2,
      call_feedback_3,call_feedback_4,call_feedback_5,call_feedback_other, Respondent_cooperation,
      
      
      S_CD_Preg1,S_CD_Preg2,S_CD_Preg3,S_CD_Preg4,S_CD_Preg5,
      S_CD_Cbirth1,S_CD_Cbirth2,S_CD_Cbirth3,S_CD_Cbirth4,S_CD_Cbirth5,
      S_CD_2M1,S_CD_2M2,S_CD_2M3,S_CD_2M4,S_CD_2M5,
      M_VisitHF,M_AdmittedHF,M_DieHF,F_VisitHF,F_AdmittedHF,F_DieHF,S_VisitHF1,S_VisitHF2,S_VisitHF3,S_VisitHF4,B_VisitHF1,B_VisitHF2,B_VisitHF3,B_VisitHF4,
      B_VisitHF5,B_VisitHF6,S_AdmitHF1,S_AdmitHF2,S_AdmitHF3,S_AdmitHF4,B_AdmitHF1,B_AdmitHF2,B_AdmitHF3,B_AdmitHF4,B_AdmitHF5,S_DieHF1,S_DieHF2,
      S_DieHF3,S_DieHF4,B_DieHF1,B_DieHF2,B_DieHF3,B_DieHF4,B_DieHF5,endtime,
      F.Dead.COVID,F.Dead.Viol,F.Dead.Acc,F.Dead.External,M.Dead.COVID,M.Dead.Viol,M.Dead.Acc,M.Dead.Preg,M.Dead.CB,M.Dead.post.Preg,M.Dead.External,M.Dead.mat,
      F.Dead.setting.death,F.Dead.Buried.Prov,F.Dead.Buried.plot,F.Dead.Location.Death,F.Dead.LiveB4,F.Dead.Who.buried,F.Dead.Local.auth,M.Dead.setting.death,
      M.Dead.Buried.Prov,M.Dead.Buried.plot,M.Dead.Location.Death,M.Dead.LiveB4,M.Dead.Who.buried,M.Dead.Local.auth,M.Dead.C19.Symp,F.Dead.C19.Symp,
      Elig,D1,roothing_other, now_complete,specify_location, Resp.Educ_lab,Resp.Age.grp_lab,Resp.Region_lab,Resp.Marriage_lab,E4a, E4a_lab,electricity_status_lab,
      Resp.Language,
      phone_call_duration,deviceid, formdef_version, KEY, phone.call.log,caseid_original,compandcons,
      
      Sisters,Brothers,Sis1.Dead,Sis2.Dead,Sis1.Dead.2019,Sis2.Dead.2019,Bro1.Dead,
      Bro1.Dead.Yr,M.Dead,M.dead.Yr,M.dead.Age,M.alive.age,F.Dead,F.dead.Yr,F.dead.Age,F.alive.age
    ))
  
  
  # ---- Get a clean set of ultimate outcomes for the data in LONG FORMAT ie attempts
  data_clean3 <- data_clean2 %>%
    dplyr::mutate(Outcome2 = ifelse(call_status == '7', 'NNU', # used to be DYS
                                    ifelse(call_status == "1" & Resp.Consent == 1, 'COMP',
                                           ifelse(call_status == "9" , 'INEL',
                                                  ifelse((call_status == "2" | call_status == "2a"), 'INCO',
                                                         ifelse(call_status == "8",'REFER',
                                                                ifelse(call_status == '6','REFU',
                                                                       ifelse(call_status == '11','DEFER',
                                                                              ifelse(call_status == '4a','LANG', #used to be lang-close
                                                                                     ifelse(call_status == '4','REASS', #used to be lang
                                                                                            ifelse(call_status == '10' | call_status == '5', "NR",
                                                                                                   ifelse(call_status == '7a', 'NNA', #used to be NO-ACCESS
                                                                                                          ifelse(call_status == "3", 'INCOR',
                                                                                                                 'Other')))))))))))),
                  phone_1 = as.numeric(phone_1))
  
  data_clean3$Outcome2[which(data_clean3$call_status ==1 & is.na(data_clean3$Resp.Consent))] <- 'INCO'
  
  # Reassignment of call outcomes
  data_clean3$Outcome2[which(!is.na(data_clean3$Elig) & 
                               data_clean3$Outcome2 =="INEL" & 
                               data_clean3$Resp.Consent ==1 & 
                               (data_clean3$D1 ==3))] <- "REFU"
  
  data_clean3$Outcome2[which(!is.na(data_clean3$Elig) & 
                               data_clean3$Outcome2 =="INEL" & 
                               is.na(data_clean3$Resp.Consent) & 
                               is.na(data_clean3$D1))] <- "INCO"
  
  # Reassign incomplete cases witha refusal for the initial language question to refused
  data_clean3$Outcome2[which(data_clean3$Outcome2=="INCO" & data_clean3$Resp.Language=="Refus")] <- "REFU"
  
  # Reassign Truly ineligible cases which have not been marked as such appropriately  
  data_clean3$Outcome2[which((data_clean3$Outcome2=="REFU"| 
                                data_clean3$Outcome2=="INCO"|
                                data_clean3$Outcome2=="Other") &
                               !is.na(data_clean3$E10))] <-"INEL"
  
  
  # merging with ivr numbers to ID them
  ivr <- ivr %>% filter(!duplicated(phone_1))
  data_clean3test <- left_join(data_clean3, ivr, by = 'phone_1') 
  # merging with both sets of feroxus numbers to ID them 
  feroxus <- feroxus %>% filter(!duplicated(phone_1))
  testing123 <- left_join(data_clean3test, feroxus, by = 'phone_1') %>%
    dplyr::mutate(fer = ferox) %>% distinct()
  
  data_clean3 <- testing123 %>% 
    dplyr::mutate(Source.number = ifelse(source == 'IVR group 1' & is.na(fer),'IVR group 1',
                                         ifelse(source == 'IVR group 2' & is.na(fer),'IVR group 2',
                                                ifelse(is.na(source) & (fer == 'Kin' | fer =='NK'), 'Feroxus', 'Feroxus'))),
                  Source.prov = ifelse(fer == 'Kin' & is.na(Province), 'Feroxus-Kin',
                                       ifelse(fer == 'NK'& is.na(Province), 'Feroxus-NK',
                                              ifelse(Province == 'Kinshasa'&is.na(fer),'IVR group 1-Kin',
                                                     ifelse(Province == 'Nord Kivu'&is.na(fer), "IVR group 1-NK", 'IVR group 2'))))) %>%
    dplyr::mutate(Source.prov = ifelse(!is.na(Source.prov), Source.prov,'IVR group 2'))
  
  # de-duplication of the data 
  # Removed this part below - could be that we do need it, but i don't see how it is helping
  data <- rbind(
    data_clean3 %>%
      dplyr::filter(compandcons==1) %>%
      distinct(caseid, .keep_all = TRUE) %>%
      dplyr::mutate(Outcome2 = ifelse(Resp.Region == 3, 'INEL',
                                      ifelse(is.na(Resp.Consent),'INCO', Outcome2)))
    ,
    data_clean3 %>%
      dplyr::filter(!(compandcons== 1)))
  
  
  # Limit to data cases and those who consent/those who collect in DRC
  # Complete <- data[which(data$now_complete=="Yes"),]
  Consented <- data[which(data$compandcons==1),]
  
  ###### Assignment of Roofing materials to defined groups
  # Reallocate roof materials appropriately 
  Consented$Roof[grepl("bois", Consented$roothing_other, ignore.case=TRUE) ] <- "Wood"
  Consented$roothing_other[grepl("bois", Consented$roothing_other, ignore.case=TRUE) ] <- NA
  
  Consented$Roof[grepl("planche", Consented$roothing_other, ignore.case=TRUE) ] <- "Wood"
  Consented$roothing_other[grepl("planche", Consented$roothing_other, ignore.case=TRUE) ] <- NA
  
  Consented$Roof[grepl("beton", Consented$roothing_other, ignore.case=TRUE) ] <- "Concrete"
  Consented$roothing_other[grepl("beton", Consented$roothing_other, ignore.case=TRUE) ] <- NA
  
  Consented$Roof[grepl("paille", Consented$roothing_other, ignore.case=TRUE) ] <- "Grass/Thatch/Palm/Straw"
  Consented$roothing_other[grepl("paille", Consented$roothing_other, ignore.case=TRUE) ] <- NA
  
  Consented$Roof[grepl("chaume", Consented$roothing_other, ignore.case=TRUE) ] <- "Grass/Thatch/Palm/Straw"
  Consented$roothing_other[grepl("chaume", Consented$roothing_other, ignore.case=TRUE) ] <- NA
  
  ######
  # Assignment of water sources to predefined groups
  # send the other into another predefined group and remove it from the list of 'other'
  
  # Reallocate roof materials appropriately 
  Consented$water_source_lab[grepl("CANNAU", Consented$other_waterSource, ignore.case=TRUE) ] <- "Surface water"
  Consented$other_waterSource[grepl("CANNAU", Consented$other_waterSource, ignore.case=TRUE) ] <- NA
  
  Consented$water_source_lab[grepl("RIVIERE", Consented$other_waterSource, ignore.case=TRUE) ] <- "Surface water"
  Consented$other_waterSource[grepl("RIVIERE", Consented$other_waterSource, ignore.case=TRUE) ] <- NA
  
  Consented$water_source_lab[grepl("Lac", Consented$other_waterSource, ignore.case=TRUE) ] <- "Surface water"
  Consented$other_waterSource[grepl("Lac", Consented$other_waterSource, ignore.case=TRUE) ] <- NA
  
  Consented$water_source_lab[grepl("FONTAINE", Consented$other_waterSource, ignore.case=TRUE) ] <- "Public tap"
  Consented$other_waterSource[grepl("FONTAINE", Consented$other_waterSource, ignore.case=TRUE) ] <- NA
  
  # ===== Location of call
  
  # --- identify words related to University or course
  Consented$Location_call[grepl("univer", Consented$specify_location, ignore.case=TRUE) |
                            grepl("unikin", Consented$specify_location, ignore.case=TRUE) |
                            grepl("classe", Consented$specify_location, ignore.case=TRUE)|
                            grepl("cours", Consented$specify_location, ignore.case=TRUE)|
                            grepl("stage", Consented$specify_location, ignore.case=TRUE)] <- "Work/Uni"
  
  # --- Remove from the 'other'variable
  Consented$specify_location[grepl("univer", Consented$specify_location, ignore.case=TRUE) |
                               grepl("unikin", Consented$specify_location, ignore.case=TRUE) |
                               grepl("classe", Consented$specify_location, ignore.case=TRUE) |
                               grepl("cours", Consented$specify_location, ignore.case=TRUE) |
                               grepl("stage", Consented$specify_location, ignore.case=TRUE) ] <- NA
  
  # --- identify words suggesting out and about 
  Consented$Location_call[grepl("route", Consented$specify_location, ignore.case=TRUE) |
                            grepl("salon", Consented$specify_location, ignore.case=TRUE) |
                            grepl("bus", Consented$specify_location, ignore.case=TRUE) |
                            grepl("studio", Consented$specify_location, ignore.case=TRUE) |
                            grepl("chez", Consented$specify_location, ignore.case=TRUE) |
                            grepl("marche", Consented$specify_location, ignore.case=TRUE)|
                            grepl("champ", Consented$specify_location, ignore.case=TRUE)|
                            grepl("centre de sante", Consented$specify_location, ignore.case=TRUE)|
                            grepl("eglise", Consented$specify_location, ignore.case=TRUE)|
                            grepl("Hôpital", Consented$specify_location, ignore.case=TRUE)|
                            grepl("Hôtel", Consented$specify_location, ignore.case=TRUE)|
                            grepl("Rue", Consented$specify_location, ignore.case=TRUE)|
                            grepl("Shop", Consented$specify_location, ignore.case=TRUE)|
                            grepl("AVENUE", Consented$specify_location, ignore.case=TRUE)|
                            grepl("ville", Consented$specify_location, ignore.case=TRUE)|
                            grepl("aeroport", Consented$specify_location, ignore.case=TRUE)|
                            grepl("ville", Consented$specify_location, ignore.case=TRUE)|
                            grepl("promenade", Consented$specify_location, ignore.case=TRUE)] <- "Out and about"
  
  # --- remove words containing this from the 'specify location' variable
  Consented$specify_location[grepl("route", Consented$specify_location, ignore.case=TRUE) |
                               grepl("salon", Consented$specify_location, ignore.case=TRUE) |
                               grepl("bus", Consented$specify_location, ignore.case=TRUE) |
                               grepl("studio", Consented$specify_location, ignore.case=TRUE) |
                               grepl("chez", Consented$specify_location, ignore.case=TRUE) |
                               grepl("marche", Consented$specify_location, ignore.case=TRUE)|
                               grepl("champ", Consented$specify_location, ignore.case=TRUE)|
                               grepl("centre de sante", Consented$specify_location, ignore.case=TRUE)|
                               grepl("eglise", Consented$specify_location, ignore.case=TRUE)|
                               grepl("Hôpital", Consented$specify_location, ignore.case=TRUE)|
                               grepl("Hôtel", Consented$specify_location, ignore.case=TRUE)|
                               grepl("Rue", Consented$specify_location, ignore.case=TRUE)|
                               grepl("Shop", Consented$specify_location, ignore.case=TRUE)|
                               grepl("AVENUE", Consented$specify_location, ignore.case=TRUE)|
                               grepl("ville", Consented$specify_location, ignore.case=TRUE)|
                               grepl("aeroport", Consented$specify_location, ignore.case=TRUE)|
                               grepl("ville", Consented$specify_location, ignore.case=TRUE)|
                               grepl("promenade", Consented$specify_location, ignore.case=TRUE)] <- NA
  
  # Convert to wide dataset
  dat1 <- data.frame(call_num=data$call_num,
                     Outcome=data$Outcome2,
                     caseid = data$caseid,
                     phone_1 = data$phone_1,
                     inel.age = data$E10,
                     lang = data$L2_1,
                     Source = data$Source.number,
                     Source.prov = data$Source.prov,
                     call_num_2 = data$call_num,
                     pull_label = data$pull_label,
                     pull_IVRprovince = data$pull_IVRprovince,
                     time = data$Date.Interview,
                     enum = data$enumerator, 
                     region = data$Resp.Region,
                     Resp.Sex= data$Resp.Sex,
                     E12 = data$E12,
                     L2 = data$L2,
                     L3 = data$L3,
                     E15 = data$E15,
                     E16 = data$E16,
                     time_availability2_1 = data$time_availability2_1,
                     time_availability2 = data$time_availability2) #%>%
  
  dat1 <- dat1 %>% 
    group_by(phone_1, call_num) 
  # dat1 <- dat1[!duplicated(dat1[c('phone_1','call_num','caseid')]),]
  dat.wide <- pivot_wider(dat1, names_from = call_num, values_from = c('Outcome', 'inel.age', 'lang', 'Source','Source.prov','call_num_2',
                                                                       'pull_label','pull_IVRprovince','time', 'enum','region', 'Resp.Sex', 'E12',
                                                                       'L2', 'L3', 'E15', 'E16', 'time_availability2_1', 'time_availability2')) %>%
    #reshape(dat1, idvar = "caseid", timevar = "call_num", direction = "wide")%>%
    dplyr::filter(caseid %notin% c('12','51','57','56','54','58','53','52','59')) %>%
    # selects the last nonNA outcome for the respondent over the specified columns
    dplyr::mutate(latest = coalesce(Outcome_15,Outcome_14,Outcome_13,Outcome_12,Outcome_11,Outcome_10,Outcome_9,Outcome_8,Outcome_7,Outcome_6, Outcome_5, Outcome_4, Outcome_4, Outcome_3, Outcome_2, Outcome_1),
                  inel.age = coalesce(inel.age_15,inel.age_14,inel.age_13,inel.age_12,inel.age_11,inel.age_10,inel.age_9,inel.age_8,inel.age_7,inel.age_6, inel.age_5, inel.age_4, inel.age_3, inel.age_2, inel.age_1),
                  lang = coalesce(lang_15,lang_14,lang_13,lang_12,lang_11,lang_10,lang_9,lang_8,lang_7,lang_6, lang_5, lang_4, lang_3, lang_2, lang_1),
                  call_num = coalesce(call_num_2_15,call_num_2_14,call_num_2_13,call_num_2_12,call_num_2_11,call_num_2_10,call_num_2_9,call_num_2_8,call_num_2_7,call_num_2_6, call_num_2_5, call_num_2_4, call_num_2_3, call_num_2_2, call_num_2_1),
                  E12 = coalesce(E12_15,E12_14,E12_13,E12_12,E12_11,E12_10,E12_9,E12_8,E12_7,E12_6, E12_5, E12_4, E12_4, E12_3, E12_2, E12_1)) %>%
    dplyr::mutate(Outcome.FINAL = case_when(latest == 'COMP' ~ 'COMP',
                                            latest =="REFU" ~ "REFU",
                                            latest == "NNU" ~ "NNU",
                                            latest== "INEL" ~ "INEL",
                                            latest=="DEFER" ~ "DEFER",
                                            #latest == 'REFER'~'REFER',
                                            #Outcome.1== "NR" & Outcome.2== "NR" & Outcome.3== "NR" & Outcome.4== "NR" & Outcome.5== "NR" ~ "NR",
                                            latest == 'NR' & Outcome_5 =='NR' ~ 'NR',
                                            #Outcome.1== "NNA" & Outcome.2== "NNA" & Outcome.3== "NNA" & Outcome.4== "NNA" & Outcome.5== "NNA" ~ "NNA",
                                            latest == 'NNA' & Outcome_5 == 'NNA' ~ "NNA",
                                            latest =="LANG" ~ "LANG",
                                            #latest == 'REASS' ~ 'REASS',
                                            Outcome_1 == "INCO" | Outcome_2 == "INCO"| Outcome_3 == "INCO"| Outcome_4 == "INCO" |Outcome_5 == "INCO"  
                                            & ((Outcome_1 != "COMP" & Outcome_2 != "COMP"& Outcome_3 != "COMP" & Outcome_4 != "COMP" & Outcome_5 != "COMP") |
                                                 Outcome_1 != "REFU" | Outcome_2 != "REFU"| Outcome_3 != "REFU" | Outcome_4 != "REFU" | Outcome_5 != "REFU" |
                                                 Outcome_1 != "INEL" | Outcome_2 != "INEL"| Outcome_3 != "INEL" | Outcome_4 != "INEL" |Outcome_5 != "INEL" 
                                               |!is.na(inel.age)) ~ "PART",
                                            #Outcome_1 == 'COMP' | Outcome_2 == 'COMP' |Outcome_3 == 'COMP' |Outcome_4 == 'COMP' |Outcome_5 == 'COMP'~'COMP',
                                            TRUE ~ "PEND"),
                  # --- Coalesce variables so we have complete data in the dat.wide data
                  Source.1 = coalesce(Source_1, Source_2, Source_3, Source_4, Source_5,Source_6, Source_7,Source_8,Source_9,Source_10,Source_11,Source_12,Source_13,Source_14,Source_15) ,
                  Source.prov.1 = coalesce(Source.prov_1, Source.prov_2, Source.prov_3, Source.prov_4, Source.prov_5,Source.prov_6,Source.prov_7,Source.prov_8,Source.prov_9,Source.prov_10,Source.prov_11,Source.prov_12,Source.prov_13,Source.prov_14,Source.prov_15),
                  pull_label = coalesce(pull_label_1, pull_label_2, pull_label_3, pull_label_4, pull_label_5,pull_label_6,pull_label_7,pull_label_8,pull_label_9,pull_label_10,pull_label_11,pull_label_12,pull_label_13,pull_label_14,pull_label_15),
                  pull_IVRprovince = coalesce(pull_IVRprovince_1, pull_IVRprovince_2, pull_IVRprovince_3, pull_IVRprovince_4, pull_IVRprovince_5,pull_IVRprovince_6,pull_IVRprovince_7,pull_IVRprovince_8,pull_IVRprovince_9,pull_IVRprovince_10,pull_IVRprovince_11,pull_IVRprovince_12,pull_IVRprovince_13,pull_IVRprovince_14,pull_IVRprovince_15),
                  enum = coalesce(enum_1, enum_2, enum_3, enum_4, enum_5,enum_6,enum_7,enum_8,enum_9,enum_10,enum_11,enum_12,enum_13,enum_14,enum_15),
                  time = coalesce(time_15,time_14,time_13,time_12,time_11,time_10,time_9,time_8,time_7,time_6,time_5, time_4, time_3, time_2, time_1),
                  region = coalesce(region_1, region_2, region_3, region_4,region_5,region_6,region_7,region_8,region_9,region_10,region_11,region_12,region_13,region_14,region_15),
                  Resp.Sex = coalesce(Resp.Sex_1, Resp.Sex_2, Resp.Sex_3, Resp.Sex_4,Resp.Sex_5,Resp.Sex_6,Resp.Sex_7,Resp.Sex_8,Resp.Sex_9,Resp.Sex_10,Resp.Sex_11,Resp.Sex_12,Resp.Sex_13,Resp.Sex_14,Resp.Sex_15),
                  L2 = coalesce(L2_1, L2_2, L2_3, L2_4,L2_5,L2_6,L2_7,L2_8,L2_9,L2_10,L2_11,L2_12,L2_13,L2_14,L2_15),
                  L3 = coalesce(L3_1, L3_2, L3_3, L3_4,L3_5,L3_6,L3_7,L3_8,L3_9,L3_10,L3_11,L3_12,L3_13,L3_14,L3_15),
                  E15 = coalesce(E15_1, E15_2, E15_3, E15_4,E15_5,E15_6,E15_7,E15_8,E15_9,E15_10,E15_11,E15_12,E15_13,E15_14,E15_15),
                  E16 = coalesce(E16_1, E16_2, E16_3, E16_4,E16_5,E16_6,E16_7,E16_8,E16_9,E16_10,E16_11,E16_12,E16_13,E16_14,E16_15),
                  time_availability2_1 = coalesce(time_availability2_1_1, time_availability2_1_2, time_availability2_1_3, time_availability2_1_4,time_availability2_1_5,time_availability2_1_6,time_availability2_1_7,time_availability2_1_8,time_availability2_1_9,time_availability2_1_10,time_availability2_1_11,time_availability2_1_12,time_availability2_1_13,time_availability2_1_14,time_availability2_1_15),
                  time_availability2 = coalesce(time_availability2_1, time_availability2_2, time_availability2_3, time_availability2_4,time_availability2_5,time_availability2_6,time_availability2_7,time_availability2_8,time_availability2_9,time_availability2_10,time_availability2_11,time_availability2_12,time_availability2_13,time_availability2_14,time_availability2_15))
  
  
  
  
  
  
  
  
  ### INFORMATION ON COVID 19 VAX REASONS FOR TAKING/NOT
  ##############################
  
  # Reasons for not taking vaccine
  
  # Already had COVID-19 and believe I am immune (1)
  Consented$vaccines_den_reasons_1[grepl("immun", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("ANTICORPS", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 1
  Consented$vaccines_den_reasons_13[grepl("immun", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("ANTICORPS", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("immun", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("ANTICORPS", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- "REASS"
  
  # Vaccine is not effective (2)
  Consented$vaccines_den_reasons_2[grepl("effic", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("pas efocase", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("aucune utilit", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("pas utile", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("pas de garantie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("garanti rien", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("guaranti rien", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("sont encore infect", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("attrape toujours", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("encore atteinte du covid", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("convai", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("prendre le vaccin ou pas il va mourir", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("CONTINUE  Ã€ TOMBER  MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("fiable", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("L'ENQUETÃ‰ N'AS PAS LA MOTIVATION DE PRENDRE LE VACCIN. 
                                         QU'IL LE PRENNE OÃ™ PAS ,IL SERA INFECTÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("PAS PRÃ‰VENTIF", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("PRENDRE LE VACCIN  OU PAS  RIEN NE  CHANGE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("POUVEZ TOUJOURS FAIRE UNE FORME GRAVE DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("C est  pas  utile", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("C est pas dans  ma t", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("C'EST PAS VALABLE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("CEUX QUI SONT VACCINÃ‰ CONTINUE  Ã€ TOMBER  MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("LE VACCIN NE GUARENTI RIEN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("N'AS PAS LA MOTIVATION DE PRENDRE LE VACCIN. 
QU'IL LE PRENNE", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 1
  Consented$vaccines_den_reasons_13[grepl("effic", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("pas efocase", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("aucune utilit", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("pas utile", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("pas de garantie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("garanti rien", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("guaranti rien", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("sont encore infect", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("attrape toujours", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("encore atteinte du covid", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("convai", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("prendre le vaccin ou pas il va mourir", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("CONTINUE  Ã€ TOMBER  MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("fiable", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("L'ENQUETÃ‰ N'AS PAS LA MOTIVATION DE PRENDRE LE VACCIN. 
                                         QU'IL LE PRENNE OÃ™ PAS ,IL SERA INFECTÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PAS PRÃ‰VENTIF", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PRENDRE LE VACCIN  OU PAS  RIEN NE  CHANGE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("POUVEZ TOUJOURS FAIRE UNE FORME GRAVE DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("C est  pas  utile", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("C est pas dans  ma t", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("C'EST PAS VALABLE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("CEUX QUI SONT VACCINÃ‰ CONTINUE  Ã€ TOMBER  MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LE VACCIN NE GUARENTI RIEN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("N'AS PAS LA MOTIVATION DE PRENDRE LE VACCIN. 
QU'IL LE PRENNE", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("effic", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("pas efocase", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("aucune utilit", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("pas utile", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("pas de garantie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("garanti rien", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("guaranti rien", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("sont encore infect", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("attrape toujours", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("encore atteinte du covid", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("convai", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("prendre le vaccin ou pas il va mourir", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("CONTINUE  Ã€ TOMBER  MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("fiable", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("L'ENQUETÃ‰ N'AS PAS LA MOTIVATION DE PRENDRE LE VACCIN. 
                                         QU'IL LE PRENNE OÃ™ PAS ,IL SERA INFECTÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PAS PRÃ‰VENTIF", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PRENDRE LE VACCIN  OU PAS  RIEN NE  CHANGE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("POUVEZ TOUJOURS FAIRE UNE FORME GRAVE DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("C est  pas  utile", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("C est pas dans  ma t", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("C'EST PAS VALABLE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("CEUX QUI SONT VACCINÃ‰ CONTINUE  Ã€ TOMBER  MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LE VACCIN NE GUARENTI RIEN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("N'AS PAS LA MOTIVATION DE PRENDRE LE VACCIN. 
QU'IL LE PRENNE", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- "REASS"
  
  # Vaccine is not safe/causes harm/side effects (3)
  Consented$vaccines_den_reasons_3[grepl("Le vaccin change les groupes sanguin", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("N A PAS ETE EXPERIMENTE AVANT DE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("ne connaissons pas son origine", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("risque", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("effet", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("peur", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("confuse", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("tue", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("dangereux", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("DÃ‰CÃ‰DÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("dÃ©cÃ©dÃ©", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("INFECOND", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("abÃ®me", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("pas bien", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("mauvais", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("donne des allergies", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("allergique", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("COMORBIDITÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("risquant", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("coagule le sang", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("poison", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("asme", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("Asthmatique", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("JE TOMBE DIFFICILEMENT MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("J'ATTENDS QUE MON Ã‰TAT DE SANTÃ‰ PUISSE SE STABILISER", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("NTÃ‰ APRÃˆS PAR CE QUE JE SUIS EN TRÃˆS BONNE SANTÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("IL  Y'A  QUELQUE CHOSE  CACHÃ‰E  DERRIÃˆRE  LE VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("me la sant", Consented$Other.Den.Reasons, ignore.case=TRUE)| # abime la sante
                                     grepl("truit les gens", Consented$Other.Den.Reasons, ignore.case=TRUE)|#detruit les gens
                                     grepl("DEFORME LE CORPS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("TRUIT LA SANT", Consented$Other.Den.Reasons, ignore.case=TRUE)|#detruit la sante|
                                     grepl("IL  Y'A  QUELQUE CHOSE  CACH", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("LA VACCIN CRÃ‰E DES MALAISES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("LE VACCIN CRÃ‰E DE PROBLEMES  A LA SANTÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("LE VACCIN N'EST PAS S", Consented$Other.Den.Reasons, ignore.case=TRUE)| #n'est pas sur
                                     grepl("LE VACCIN REND MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("SON AMI EST D", Consented$Other.Den.Reasons, ignore.case=TRUE)| #son ami est decede apres avoir prise le vaccin
                                     grepl("Son mari est d", Consented$Other.Den.Reasons, ignore.case=TRUE)# son ami est decede apres l'avoir pris
  ]<- 1 
  Consented$vaccines_den_reasons_13[grepl("Le vaccin change les groupes sanguin", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("N A PAS ETE EXPERIMENTE AVANT DE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("ne connaissons pas son origine", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("risque", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("effet", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("peur", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("confuse", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("tue", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("dangereux", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("DÃ‰CÃ‰DÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("dÃ©cÃ©dÃ©", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("INFECOND", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("abÃ®me", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("pas bien", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("mauvais", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("mauvais", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("donne des allergies", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("allergique", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("COMORBIDITÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("risquant", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("coagule le sang", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("poison", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("asme", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Asthmatique", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JE TOMBE DIFFICILEMENT MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("J'ATTENDS QUE MON Ã‰TAT DE SANTÃ‰ PUISSE SE STABILISER", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NTÃ‰ APRÃˆS PAR CE QUE JE SUIS EN TRÃˆS BONNE SANTÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("IL  Y'A  QUELQUE CHOSE  CACHÃ‰E  DERRIÃˆRE  LE VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("me la sant", Consented$Other.Den.Reasons, ignore.case=TRUE)| # abime la sante
                                      grepl("truit les gens", Consented$Other.Den.Reasons, ignore.case=TRUE)|#detruit les gens
                                      grepl("DEFORME LE CORPS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("TRUIT LA SANT", Consented$Other.Den.Reasons, ignore.case=TRUE)|#detruit la sante|
                                      grepl("IL  Y'A  QUELQUE CHOSE  CACH", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LA VACCIN CRÃ‰E DES MALAISES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LE VACCIN CRÃ‰E DE PROBLEMES  A LA SANTÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LE VACCIN N'EST PAS S", Consented$Other.Den.Reasons, ignore.case=TRUE)| #n'est pas sur
                                      grepl("LE VACCIN REND MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SON AMI EST D", Consented$Other.Den.Reasons, ignore.case=TRUE)| #son ami est decede apres avoir prise le vaccin
                                      grepl("Son mari est d", Consented$Other.Den.Reasons, ignore.case=TRUE)# son ami est decede apres l'avoir pris
  ]<- 0
  Consented$Other.Den.Reasons[grepl("Le vaccin change les groupes sanguin", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("N A PAS ETE EXPERIMENTE AVANT DE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("ne connaissons pas son origine", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("risque", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("effet", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("peur", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("confuse", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("tue", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("dangereux", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("DÃ‰CÃ‰DÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("dÃ©cÃ©dÃ©", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("INFECOND", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("abÃ®me", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("pas bien", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("mauvais", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("mauvais", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("donne des allergies", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("allergique", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("COMORBIDITÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("risquant", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("coagule le sang", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("poison", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("asme", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Asthmatique", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("JE TOMBE DIFFICILEMENT MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("J'ATTENDS QUE MON Ã‰TAT DE SANTÃ‰ PUISSE SE STABILISER", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("NTÃ‰ APRÃˆS PAR CE QUE JE SUIS EN TRÃˆS BONNE SANTÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("IL  Y'A  QUELQUE CHOSE  CACHÃ‰E  DERRIÃˆRE  LE VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("me la sant", Consented$Other.Den.Reasons, ignore.case=TRUE)| # abime la sante
                                grepl("truit les gens", Consented$Other.Den.Reasons, ignore.case=TRUE)|#detruit les gens
                                grepl("DEFORME LE CORPS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("TRUIT LA SANT", Consented$Other.Den.Reasons, ignore.case=TRUE)|#detruit la sante|
                                grepl("IL  Y'A  QUELQUE CHOSE  CACH", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LA VACCIN CRÃ‰E DES MALAISES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LE VACCIN CRÃ‰E DE PROBLEMES  A LA SANTÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LE VACCIN N'EST PAS S", Consented$Other.Den.Reasons, ignore.case=TRUE)| #n'est pas sur
                                grepl("LE VACCIN REND MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("SON AMI EST D", Consented$Other.Den.Reasons, ignore.case=TRUE)| #son ami est decede apres avoir prise le vaccin
                                grepl("Son mari est d", Consented$Other.Den.Reasons, ignore.case=TRUE)# son ami est decede apres l'avoir pris
  ] <- "REASS"
  
  # May get covid from the vaccine (4)
  Consented$vaccines_den_reasons_4[grepl("provoque la maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("donne la maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("CEUX QUI SE SONT VACCINÃ‰  SONT  MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 1
  Consented$vaccines_den_reasons_13[grepl("provoque la maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("donne la maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("CEUX QUI SE SONT VACCINÃ‰  SONT  MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("provoque la maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("donne la maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("CEUX QUI SE SONT VACCINÃ‰  SONT  MALADE", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- "REASS"
  
  # Religious objections/traditional beliefs (specify) (5)
  Consented$vaccines_den_reasons_5[grepl("dieu", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("conviction religieuse", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("PRIÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("religion", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("DÃ‰MONIAQUE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("SCIENCES OCCULTES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("satanique", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("le signe de la fin du monde", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("religieu", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("tÃ©moin de jehovah", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("OCCULTISME", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("pasteur", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("chrÃ©tien", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 1
  Consented$vaccines_den_reasons_13[grepl("dieu", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("conviction religieuse", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PRIÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("religion", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("DÃ‰MONIAQUE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SCIENCES OCCULTES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("satanique", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("le signe de la fin du monde", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("religieu", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("tÃ©moin de jehovah", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("OCCULTISME", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("pasteur", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("chrÃ©tien", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("dieu", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("conviction religieuse", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PRIÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("religion", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("DÃ‰MONIAQUE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("SCIENCES OCCULTES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("satanique", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("le signe de la fin du monde", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("religieu", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("tÃ©moin de jehovah", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("OCCULTISME", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("pasteur", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("chrÃ©tien", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- "REASS"
  
  
  # Chance of contracting COVID-19 is small (6)
  Consented$vaccines_den_reasons_6[grepl("exist", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("expos", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("PAS COVID 19 EN RDC", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("ne connait pas si la  maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("YA PAS ASSEZ DES CAS DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("CROIS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("CROIT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("Ici chez nous il y a pas de covid", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("N' YA PAS DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("ne se propage pas", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("PAS ATTRAPER", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("POUR LES BLAMCS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("pas de cas de covid", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("personne n'a souffert de Ã‡A DANS MA VILLE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("pas de covid ici", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("la maladie est deja termine", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("POUR LES BLANCS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("noir", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("NE  ME  CONCERNE PAS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("N'AI PAS BESOIN DU VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("C EST POUR LES  CHINOIS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("PAS DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("PAS DE CORONA", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("IL Y A PAS CETTE  MALADIE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("On ne connait pas cette  maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("Pas  de covid", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("LE COVID EST DEJA FINI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("Le covid  c est pour  le blanc", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("LE COVID 19 EST DÃ‰JÃ€ PASSÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("LE COVID  Ã€ DIMINUER", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("LA MALADIE EST  DEJA TERMINE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("LA MALADIE  N EST PAS ICI  CHEZ NOUS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("IL  N y'a pas  la maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("Je ne  pas  encore  vu quelqu un d", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("JE NE VOIT PAS DES CAS DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("PARCE QUE LA MALADIE  Ã€ DIMINUER", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("PERSONNE N'A SOUFFERT DE Ã‡A DANS MA VILLE", Consented$Other.Den.Reasons, ignore.case=TRUE)
  ]<- 1
  Consented$vaccines_den_reasons_13[grepl("exist", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("expos", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PAS COVID 19 EN RDC", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("ne connait pas si la  maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("YA PAS ASSEZ DES CAS DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("CROIS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("CROIT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Ici chez nous il y a pas de covid", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("N' YA PAS DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("ne se propage pas", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PAS ATTRAPER", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("POUR LES BLAMCS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("pas de cas de covid", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("personne n'a souffert de Ã‡A DANS MA VILLE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("pas de covid ici", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("la maladie est deja termine", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("POUR LES BLANCS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("noir", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NE  ME  CONCERNE PAS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("N'AI PAS BESOIN DU VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("C EST POUR LES  CHINOIS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PAS DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PAS DE CORONA", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("IL Y A PAS CETTE  MALADIE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("On ne connait pas cette  maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Pas  de covid", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LE COVID EST DEJA FINI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Le covid  c est pour  le blanc", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LE COVID 19 EST DÃ‰JÃ€ PASSÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LE COVID  Ã€ DIMINUER", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LA MALADIE EST  DEJA TERMINE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LA MALADIE  N EST PAS ICI  CHEZ NOUS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("IL  N y'a pas  la maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Je ne  pas  encore  vu quelqu un d", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JE NE VOIT PAS DES CAS DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PARCE QUE LA MALADIE  Ã€ DIMINUER", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PERSONNE N'A SOUFFERT DE Ã‡A DANS MA VILLE", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("exist", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("expos", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PAS COVID 19 EN RDC", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("ne connait pas si la  maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("YA PAS ASSEZ DES CAS DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("CROIS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("CROIT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Ici chez nous il y a pas de covid", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("N' YA PAS DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("ne se propage pas", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PAS ATTRAPER", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("POUR LES BLAMCS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("pas de cas de covid", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("personne n'a souffert de Ã‡A DANS MA VILLE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("pas de covid ici", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("la maladie est deja termine", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("POUR LES BLANCS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("noir", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("NE  ME  CONCERNE PAS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("N'AI PAS BESOIN DU VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("C EST POUR LES  CHINOIS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PAS DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PAS DE CORONA", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("IL Y A PAS CETTE  MALADIE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("On ne connait pas cette  maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Pas  de covid", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LE COVID EST DEJA FINI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Le covid  c est pour  le blanc", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LE COVID 19 EST DÃ‰JÃ€ PASSÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LE COVID  Ã€ DIMINUER", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LA MALADIE EST  DEJA TERMINE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LA MALADIE  N EST PAS ICI  CHEZ NOUS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("IL  N y'a pas  la maladie", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Je ne  pas  encore  vu quelqu un d", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("JE NE VOIT PAS DES CAS DE COVID", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PARCE QUE LA MALADIE  Ã€ DIMINUER", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PERSONNE N'A SOUFFERT DE Ã‡A DANS MA VILLE", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- "REASS"
  
  
  # COVID-19 is not serious or life threatening (for me) (7)
  Consented$vaccines_den_reasons_7[grepl("important", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("néc", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("nec", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("n'est pas grave", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("je suis en bonne", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("je suis encore fort", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("pas d'importance", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("pas d importance", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("pas urgent", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("PAS  L IMPORTANCE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("N'EST PAS DANS L'URGENCE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("NE TROUVE PAS LA NÃ‰CESSITÃ‰ DE PRENDRE LE VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                     grepl("Il ne moura pas du virus", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 1
  Consented$vaccines_den_reasons_13[grepl("pas important", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("néc", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("nec", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("n'est pas grave", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("je suis en bonne", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("je suis encore fort", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("pas d'importance", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("pas d importance", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("pas urgent", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PAS  L IMPORTANCE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("N'EST PAS DANS L'URGENCE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NE TROUVE PAS LA NÃ‰CESSITÃ‰ DE PRENDRE LE VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Il ne moura pas du virus", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("important", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("néc", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("nec", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("n'est pas grave", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("je suis en bonne", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("je suis encore fort", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("pas d'importance", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("pas d importance", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("pas urgent", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PAS  L IMPORTANCE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("N'EST PAS DANS L'URGENCE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("NE TROUVE PAS LA NÃ‰CESSITÃ‰ DE PRENDRE LE VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Il ne moura pas du virus", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- "REASS"
  
  # doesn't like needles (9)
  Consented$vaccines_den_reasons_9[grepl('Je n aime pas les aiguilles', Consented$Other.Den.Reasons, ignore.case = TRUE)] <- 1
  Consented$vaccines_den_reasons_13[grepl('Je n aime pas les aiguilles', Consented$Other.Den.Reasons, ignore.case = TRUE)] <- 0
  Consented$Other.Den.Reasons[grepl('Je n aime pas les aiguilles', Consented$Other.Den.Reasons, ignore.case = TRUE)] <- 'REASS'
  
  # wait until others take the vaccine (10)
  Consented$vaccines_den_reasons_10[grepl("attends que les autres prennent", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("attends que le pr", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LORS QUE TOUS LES AUTORIT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NOUS OBSERVONS ENCORE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("donne d'abord au pr", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PARCE QUE LES GENS  N ONT  PAS  PRIS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PRENNENT D ABORD", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("DONNER D'ABORD AU PR", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JUSTE  APRES LES PR", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NOUS ATTENDONS QUE LE PRESIDENT DE LA REPUBLIQUE PUISSE DONNER LE FEU VERT", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 1
  Consented$vaccines_den_reasons_13[grepl("attends que les autres prennent", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("attends que le pr", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LORS QUE TOUS LES AUTORIT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NOUS OBSERVONS ENCORE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("donne d'abord au pr", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PARCE QUE LES GENS  N ONT  PAS  PRIS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PRENNENT D ABORD", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("DONNER D'ABORD AU PR", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JUSTE  APRES LES PR", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NOUS ATTENDONS QUE LE PRESIDENT DE LA REPUBLIQUE PUISSE DONNER LE FEU VERT", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("attends que les autres prennent", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("attends que le pr", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LORS QUE TOUS LES AUTORIT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("NOUS OBSERVONS ENCORE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("donne d'abord au pr", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PARCE QUE LES GENS  N ONT  PAS  PRIS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PRENNENT D ABORD", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("DONNER D'ABORD AU PR", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("JUSTE  APRES LES PR", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("NOUS ATTENDONS QUE LE PRESIDENT DE LA REPUBLIQUE PUISSE DONNER LE FEU VERT", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- "REASS"
  
  # There will be other effective treatments soon (11)
  Consented$vaccines_den_reasons_11[grepl("NOUVEAU VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 1
  Consented$vaccines_den_reasons_13[grepl("NOUVEAU VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("NOUVEAU VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)] <- "REASS"
  
  # Vaccine is too expsneive(16)
  Consented$vaccines_den_reasons_16[grepl("PAYANT", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 1
  Consented$vaccines_den_reasons_13[grepl("PAYANT", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("PAYANT", Consented$Other.Den.Reasons, ignore.case=TRUE)] <- "REASS"
  
  # need more information/medical advice (17)
  Consented$vaccines_den_reasons_17[grepl("L AVIS  DE MON  MÃ‰DECIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("ENCORE EN PÃ‰RIODE D'ESSAI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LA PHASE EXPÃ‰RIMENTALE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SAVOIR  LES  CONSÃ‰QUENCES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Attendre l' avis de son medecin", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("ENCORE EN PHASE S'ESSAIE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("RIODE D'ESSAI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("ENCORE AU STADE  DE L ESSAIE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("C ENCORE UN ESSAIE CLINIQUE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("RIMENTALE PROPRE AUX ANIMAUX PAS AUX HUMAINS NORMALEMENT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JE D ABORD  SAVOIR  LES  CONS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JE VAIS  DEMANDER  L AVIS  DE MON  M", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NOUS VOULONS UNE BONNE SIGNIFICATION DU  VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)
  ]<- 1
  Consented$vaccines_den_reasons_13[grepl("L AVIS  DE MON  MÃ‰DECIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("ENCORE EN PÃ‰RIODE D'ESSAI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LA PHASE EXPÃ‰RIMENTALE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SAVOIR  LES  CONSÃ‰QUENCES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Attendre l' avis de son medecin", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("ENCORE EN PHASE S'ESSAIE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("RIODE D'ESSAI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("ENCORE AU STADE  DE L ESSAIE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("C ENCORE UN ESSAIE CLINIQUE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("RIMENTALE PROPRE AUX ANIMAUX PAS AUX HUMAINS NORMALEMENT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JE D ABORD  SAVOIR  LES  CONS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JE VAIS  DEMANDER  L AVIS  DE MON  M", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NOUS VOULONS UNE BONNE SIGNIFICATION DU  VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("L AVIS  DE MON  MÃ‰DECIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("ENCORE EN PÃ‰RIODE D'ESSAI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LA PHASE EXPÃ‰RIMENTALE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("SAVOIR  LES  CONSÃ‰QUENCES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Attendre l' avis de son medecin", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("ENCORE EN PHASE S'ESSAIE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("RIODE D'ESSAI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("ENCORE AU STADE  DE L ESSAIE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("C ENCORE UN ESSAIE CLINIQUE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("RIMENTALE PROPRE AUX ANIMAUX PAS AUX HUMAINS NORMALEMENT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("JE D ABORD  SAVOIR  LES  CONS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("JE VAIS  DEMANDER  L AVIS  DE MON  M", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("NOUS VOULONS UNE BONNE SIGNIFICATION DU  VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- "REASS"
  
  # pregnant or breastfeeding (18)
  Consented$vaccines_den_reasons_18[grepl('ALLAITTE', Consented$Other.Den.Reasons, ignore.case=TRUE)] <- 1
  Consented$vaccines_den_reasons_13[grepl('ALLAITTE', Consented$Other.Den.Reasons, ignore.case=TRUE)] <- 0
  Consented$Other.Den.Reasons[grepl('ALLAITTE', Consented$Other.Den.Reasons, ignore.case=TRUE)] <- "REASS"
  
  # Consent of some sort (19)
  Consented$vaccines_den_reasons_19[grepl("parent", Consented$Other.Den.Reasons, ignore.case=TRUE) |
                                      grepl("maman", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("autorisation de son mari", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("M'ENTRETENIR D'ABORD AVEC MA FEMME", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("autorisation maritale", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("consentement", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("MA TÃŠTE  N A  PAS  ENCORE  CONSENTI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SA FEMME A REFUSÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Avis de sa femme", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("CES ENFANTS NE SONT PAS D'ACCORD", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PAPA A INTERDIT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("TE  N A  PAS  ENCORE  CONSENTI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("CA DEPENDRA DE MON MARI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LE FIANCÃ‰  NE VEUT PAS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SA FEMME A REFUSÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 1
  Consented$vaccines_den_reasons_13[grepl("parent", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("maman", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("autorisation de son mari", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("M'ENTRETENIR D'ABORD AVEC MA FEMME", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("autorisation maritale", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("consentement", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("MA TÃŠTE  N A  PAS  ENCORE  CONSENTI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SA FEMME A REFUSÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Avis de sa femme", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("CES ENFANTS NE SONT PAS D'ACCORD", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("PAPA A INTERDIT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("TE  N A  PAS  ENCORE  CONSENTI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("CA DEPENDRA DE MON MARI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LE FIANCÃ‰  NE VEUT PAS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SA FEMME A REFUSÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("parent", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("maman", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("autorisation de son mari", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("M'ENTRETENIR D'ABORD AVEC MA FEMME", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("autorisation maritale", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("consentement", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("MA TÃŠTE  N A  PAS  ENCORE  CONSENTI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("SA FEMME A REFUSÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Avis de sa femme", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("CES ENFANTS NE SONT PAS D'ACCORD", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("PAPA A INTERDIT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("TE  N A  PAS  ENCORE  CONSENTI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("CA DEPENDRA DE MON MARI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LE FIANCÃ‰  NE VEUT PAS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("SA FEMME A REFUSÃ‰", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- "REASS"
  
  # Protect myself in other ways (20)
  Consented$vaccines_den_reasons_20[grepl("GESTES BARRIÃˆRES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("GESTES BARRIeRES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("son vaccin", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("tisane", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("bain de soleil", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("utilise les plantes", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("traitement traditionnel", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("sais  comment  me protÃ©ger", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SE PROTÃˆGE DÃ‰JÃ€", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("IL PREND DÃ‰JÃ€ SOINS DE LUI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SE PROTÃ‰GE DÃ‰JÃ€", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("les herbes mÃ©dicinales", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("traitements traditionnels", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("les herbes  medecinales", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("son propre vaccin", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JE PRENDS D", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("IL FAUT FAIRE RESPECTER LES GESTES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LES PLANTES NATURELS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("IL SE PROTÃ‰GE DÃ‰JÃ€ ET Ã‡A SERT Ã€ RIEN DE PRENDRE LE VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("IL SE PROTEGE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Je fais confiance aux produits locaux", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JE ME PROTÃˆGE EN METTANT EN PRATIQUE LES GESTES BARRIÃˆRES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JE ME PROTEGE DEJA", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LES GESTES BARRIÃˆRES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Je sais  comment  me protÃ©ger", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Je sais me protÃ©ger", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NE VA PAS PRENDRE LE VACCIN PARCE QU'IL SE PROT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SOINS DE LUI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NE VAS PAS PRENDRE LE VACCIN PARCE QU'ELLE SE PROT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Nous portons le masque", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("les plantes  mÃ©dicinales", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("les plantes m", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("les herbes m", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Produit traditionnel", Consented$Other.Den.Reasons, ignore.case=TRUE)
  ]<- 1
  Consented$vaccines_den_reasons_13[grepl("GESTES BARRIÃˆRES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("GESTES BARRIeRES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("son vaccin", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("tisane", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("bain de soleil", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("utilise les plantes", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("traitement traditionnel", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("sais  comment  me protÃ©ger", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SE PROTÃˆGE DÃ‰JÃ€", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("IL PREND DÃ‰JÃ€ SOINS DE LUI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SE PROTÃ‰GE DÃ‰JÃ€", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("les herbes mÃ©dicinales", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("traitements traditionnels", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("les herbes  medecinales", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("son propre vaccin", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JE PRENDS D", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("IL FAUT FAIRE RESPECTER LES GESTES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LES PLANTES NATURELS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("IL SE PROTÃ‰GE DÃ‰JÃ€ ET Ã‡A SERT Ã€ RIEN DE PRENDRE LE VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("IL SE PROTEGE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Je fais confiance aux produits locaux", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JE ME PROTÃˆGE EN METTANT EN PRATIQUE LES GESTES BARRIÃˆRES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("JE ME PROTEGE DEJA", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LES GESTES BARRIÃˆRES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Je sais  comment  me protÃ©ger", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Je sais me protÃ©ger", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NE VA PAS PRENDRE LE VACCIN PARCE QU'IL SE PROT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("SOINS DE LUI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("NE VAS PAS PRENDRE LE VACCIN PARCE QU'ELLE SE PROT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Nous portons le masque", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("les plantes  mÃ©dicinales", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("les plantes m", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("les herbes m", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("Produit traditionnel", Consented$Other.Den.Reasons, ignore.case=TRUE)
  ]<- 0
  Consented$Other.Den.Reasons[grepl("GESTES BARRIÃˆRES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("GESTES BARRIeRES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("son vaccin", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("tisane", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("bain de soleil", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("utilise les plantes", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("traitement traditionnel", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("sais  comment  me protÃ©ger", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("SE PROTÃˆGE DÃ‰JÃ€", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("IL PREND DÃ‰JÃ€ SOINS DE LUI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("SE PROTÃ‰GE DÃ‰JÃ€", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("les herbes mÃ©dicinales", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("traitements traditionnels", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("les herbes  medecinales", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("son propre vaccin", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("JE PRENDS D", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("IL FAUT FAIRE RESPECTER LES GESTES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LES PLANTES NATURELS", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("IL SE PROTÃ‰GE DÃ‰JÃ€ ET Ã‡A SERT Ã€ RIEN DE PRENDRE LE VACCIN", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("IL SE PROTEGE", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Je fais confiance aux produits locaux", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("JE ME PROTÃˆGE EN METTANT EN PRATIQUE LES GESTES BARRIÃˆRES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("JE ME PROTEGE DEJA", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LES GESTES BARRIÃˆRES", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Je sais  comment  me protÃ©ger", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Je sais me protÃ©ger", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("NE VA PAS PRENDRE LE VACCIN PARCE QU'IL SE PROT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("SOINS DE LUI", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("NE VAS PAS PRENDRE LE VACCIN PARCE QU'ELLE SE PROT", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Nous portons le masque", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("les plantes  mÃ©dicinales", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("les plantes m", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("les herbes m", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("Produit traditionnel", Consented$Other.Den.Reasons, ignore.case=TRUE)
  ]<- "REASS"
  
  
  # --- Additional variables
  # Add new variables - rumour, Info, Pointless, pregnant, no confidence
  Consented$vaccines_den_reasons_17[grepl("sur", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("sûr", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("info", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("explication", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("sensibilis", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LA sensation ne pas bien fait", Consented$Other.Den.Reasons, ignore.case=TRUE)] <- 1
  Consented$vaccines_den_reasons_13[grepl("sur", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("sûr", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("info", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("explication", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("sensibilis", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                      grepl("LA sensation ne pas bien fait", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("sur", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("sûr", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("info", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("explication", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("sensibilis", Consented$Other.Den.Reasons, ignore.case=TRUE)|
                                grepl("LA sensation ne pas bien fait", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- "REASS"
  
  #they are pregnant
  Consented$vaccines_den_reasons_18[grepl("ENCEINTE", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 1
  Consented$vaccines_den_reasons_13[grepl("ENCEINTE", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Den.Reasons[grepl("ENCEINTE", Consented$Other.Den.Reasons, ignore.case=TRUE)]<- "REASS"
  
  ############################################################
  ############################################################
  ############################################################
  #REASONS THEY WOULD OR DID TAKE THE VACCINE
  
  # TO PROTECT OTHERS
  Consented$vaccines_yes_reasons_4[grepl("SAUVER L HUMANITE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("PROTEGER MES AUTRES", Consented$Other.Yes.Reasons, ignore.case=TRUE)] <- 1
  Consented$vaccines_yes_reasons_9[grepl("SAUVER L HUMANITE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("PROTEGER MES AUTRES", Consented$Other.Yes.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Yes.Reasons[grepl("SAUVER L HUMANITE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("PROTEGER MES AUTRES", Consented$Other.Yes.Reasons, ignore.case=TRUE)]<- "REASS"
  
  # To protect against Against reinfection (1)
  Consented$vaccines_yes_reasons_1[grepl("POUR AVOIR UNE IMMUNITÉ", Consented$Other.Yes.Reasons, ignore.case=TRUE)] <- 1
  Consented$vaccines_yes_reasons_9[grepl("POUR AVOIR UNE IMMUNITÉ", Consented$Other.Yes.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Yes.Reasons[grepl("POUR AVOIR UNE IMMUNITÉ", Consented$Other.Yes.Reasons, ignore.case=TRUE)]<- "REASS"
  
  
  # Believe it's my responsibility (5)
  Consented$vaccines_yes_reasons_5[grepl("Pcq c est une obligation", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Pour le service", Consented$Other.Yes.Reasons, ignore.case=TRUE)] <- 1
  Consented$vaccines_yes_reasons_9[grepl("Pcq c est une obligation", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Pour le service", Consented$Other.Yes.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Yes.Reasons[grepl("Pcq c est une obligation", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Pour le service", Consented$Other.Yes.Reasons, ignore.case=TRUE)]<- "REASS"
  
  
  # For travel (13)
  Consented$vaccines_yes_reasons_13[grepl("voyage", Consented$Other.Yes.Reasons, ignore.case=TRUE)
                                    #  |grepl("travai", Consented$Other.Yes.Reasons, ignore.case=TRUE)
  ] <- 1
  Consented$vaccines_yes_reasons_9[grepl("voyage", Consented$Other.Yes.Reasons, ignore.case=TRUE)
                                   #|grepl("travai", Consented$Other.Yes.Reasons, ignore.case=TRUE)
  ]<- 0
  Consented$Other.Yes.Reasons[grepl("voyage", Consented$Other.Yes.Reasons, ignore.case=TRUE)
                              # |grepl("travai", Consented$Other.Yes.Reasons, ignore.case=TRUE)
  ]<- "REASS"
  
  # to protect myself (15)
  Consented$vaccines_yes_reasons_15[grepl("MES PROT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("SE PROT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("se prot", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("Se prot", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("S'EPARGNER DU COVID 19", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("SE PREVENIR COVID 19", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("S'ABSTENIR DU COVID 19", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("POUR MES PROT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("PROTECTION", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("DU VIRUS", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("POUR SE PRÃ‰VENIR", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("ME PROT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("Pour me garantir", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("ME  PROTEGER", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("POUR MA SANT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("POUR MA PROPRE SANTE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("POUR EVITE LA MALADI", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                      grepl("POUR ETRE EN BONNE SANTE", Consented$Other.Yes.Reasons, ignore.case=TRUE)] <- 1
  Consented$vaccines_yes_reasons_9[grepl("MES PROTEGE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("SE PROT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("se prot", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Se prot", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("S'EPARGNER DU COVID 19", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("SE PREVENIR COVID 19", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("S'ABSTENIR DU COVID 19", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("POUR MES PROT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("PROTECTION", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("DU VIRUS", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("POUR SE PRÃ‰VENIR", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("ME PROT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Pour me garantir", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("ME  PROTEGER", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("POUR MA SANT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("POUR MA PROPRE SANTE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("POUR EVITE LA MALADI", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("POUR ETRE EN BONNE SANTE", Consented$Other.Yes.Reasons, ignore.case=TRUE)]<- 0
  Consented$Other.Yes.Reasons[grepl("MES PROTEGE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("SE PROT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("se prot", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Se prot", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("S'EPARGNER DU COVID 19", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("SE PREVENIR COVID 19", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("S'ABSTENIR DU COVID 19", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("POUR MES PROT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("PROTECTION", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("DU VIRUS", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("POUR SE PRÃ‰VENIR", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("ME PROT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Pour me garantir", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("ME  PROTEGER", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("POUR MA SANT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("POUR MA PROPRE SANTE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("POUR EVITE LA MALADI", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("POUR ETRE EN BONNE SANTE", Consented$Other.Yes.Reasons, ignore.case=TRUE)]<- "REASS"
  
  
  # Misassigned -  create separate category of likelihood (category 6 which is part of unlikely)
  Consented$Vaccine.Likelihood[grepl("Pour le moment c est pas important", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Ce n'est pas  important", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("JE  SUIS  PAS INFORME  POUR LES VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Pas confiance", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("PAS DES INFORMATIONS POUR  LES VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Pas de vaccin", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Des mauvais rumeurs contre les vaccins", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("IL YA BEAUCOUP DES RUMEURS  SUR LE VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("PAS DES INFORMATIONS  SUR LES VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("JE N AI PAS CONFIANCE AU VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("LE COVID N EXISTE  PAS", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("LE COVID 19N EXISTE PAS", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("PARCE QUE C PAYANT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Doute sue l efficacite du vaccin vaccin n est pas fiable", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Pas convaincu  sur le vaccin", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Ne crois pas en l existence de la maladie", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Elle a besoin d'une explication", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Pas  trop sure avec le vaccin", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Il ne pas pret", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("N  a pas d information sur le vacci", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("EMPOISONNE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Le vaccin cause la mort", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Nous voulons  beaucoup  d explication sur", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("PEUR DES EFFETS SECONDAIRES", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("PEUR DE EFFETS SECONDAIRES", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("JE NE CROIS PAS À L EXISTENCE DE LA MALADIE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("N'est pas sur du vaccin", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                 grepl("Le vaccin est un juste un montage du gouvernement la maladie n existe pas", Consented$Other.Yes.Reasons, ignore.case=TRUE)] <- 6
  
  Consented$vaccines_yes_reasons_9[grepl("Pour le moment c est pas important", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Ce n'est pas  important", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("JE  SUIS  PAS INFORME  POUR LES VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Pas confiance", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("PAS DES INFORMATIONS POUR  LES VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Pas de vaccin", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Des mauvais rumeurs contre les vaccins", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("IL YA BEAUCOUP DES RUMEURS  SUR LE VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("PAS DES INFORMATIONS  SUR LES VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("JE N AI PAS CONFIANCE AU VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("LE COVID N EXISTE  PAS", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("LE COVID 19N EXISTE PAS", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("PARCE QUE C PAYANT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Doute sue l efficacite du vaccin vaccin n est pas fiable", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Pas convaincu  sur le vaccin", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Ne crois pas en l existence de la maladie", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Elle a besoin d'une explication", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Pas  trop sure avec le vaccin", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Il ne pas pret", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("N  a pas d information sur le vacci", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("EMPOISONNE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Le vaccin cause la mort", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Nous voulons  beaucoup  d explication sur", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("PEUR DES EFFETS SECONDAIRES", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("PEUR DE EFFETS SECONDAIRES", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("JE NE CROIS PAS À L EXISTENCE DE LA MALADIE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("N'est pas sur du vaccin", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                     grepl("Le vaccin est un juste un montage du gouvernement la maladie n existe pas", Consented$Other.Yes.Reasons, ignore.case=TRUE)] <- 0
  
  Consented$Other.Yes.Reasons[grepl("Pour le moment c est pas important", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Ce n'est pas  important", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("JE  SUIS  PAS INFORME  POUR LES VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Pas confiance", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("PAS DES INFORMATIONS POUR  LES VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Pas de vaccin", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Des mauvais rumeurs contre les vaccins", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("IL YA BEAUCOUP DES RUMEURS  SUR LE VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("PAS DES INFORMATIONS  SUR LES VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("JE N AI PAS CONFIANCE AU VACCIN", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("LE COVID N EXISTE  PAS", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("LE COVID 19N EXISTE PAS", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("PARCE QUE C PAYANT", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Doute sue l efficacite du vaccin vaccin n est pas fiable", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Pas convaincu  sur le vaccin", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Ne crois pas en l existence de la maladie", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Elle a besoin d'une explication", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Pas  trop sure avec le vaccin", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Il ne pas pret", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("N  a pas d information sur le vacci", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("EMPOISONNE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Le vaccin cause la mort", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Nous voulons  beaucoup  d explication sur", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("PEUR DES EFFETS SECONDAIRES", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("PEUR DE EFFETS SECONDAIRES", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("JE NE CROIS PAS À L EXISTENCE DE LA MALADIE", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("N'est pas sur du vaccin", Consented$Other.Yes.Reasons, ignore.case=TRUE)|
                                grepl("Le vaccin est un juste un montage du gouvernement la maladie n existe pas", Consented$Other.Yes.Reasons, ignore.case=TRUE)] <- "REASS"
  
  # Dataset to share
  data_sml <- data %>%
    dplyr::select(c(caseid, enumerator, Date.Interview, month.interview, Total, starts_with('Resp'), call_num, Outcome2, 
                    starts_with('ssh'), source, Source.number,Source.prov,  Elig.Time, Arrangement.Time, Consent.Time,
                    Background.Time, C19.Time, HH.Time,phone_call_duration,
                    PS.Time, SSH.Time, DB.Time, Phone.duration,Location_call, OwnPhone, E4a_lab, water_source_lab,other_waterSource, electricity_status_lab,
                    Education.Prim, Education.Sec, Education.Hig, HHsize, U5Death, U5, O5, O5Death, starts_with('Sis'), starts_with('Bro'),
                    starts_with('M.Dead'), starts_with('F.Dead'), starts_with('M.ali'), starts_with('F.ali'), agegp,
                    starts_with('S_CD'), starts_with('CD'), starts_with('Vac'),starts_with('vac'),starts_with('Othe'),starts_with('ps'),
                    D1,starts_with('call_feedback'),Respondent_cooperation))
  cons_sml <- Consented %>%
    dplyr::select(c(caseid, enumerator, Date.Interview, month.interview, Total, starts_with('Resp'), call_num, Outcome2, 
                    starts_with('ssh'), source, Source.number,Source.prov,  Elig.Time, Arrangement.Time, Consent.Time,
                    Background.Time, C19.Time, HH.Time,phone_call_duration,
                    PS.Time, SSH.Time, DB.Time, Phone.duration,Location_call, OwnPhone, E4a_lab, water_source_lab,other_waterSource, electricity_status_lab,
                    Education.Prim, Education.Sec, Education.Hig, HHsize, U5Death, U5, O5, O5Death, starts_with('Sis'), starts_with('Bro'),
                    starts_with('M.Dead'), starts_with('F.Dead'), starts_with('M.ali'), starts_with('F.ali'), agegp,
                    starts_with('S_CD'), starts_with('CD'), starts_with('Vac'),starts_with('vac'),starts_with('Othe'),starts_with('ps'),
                    D1,starts_with('call_feedback'),Respondent_cooperation))
  
  ##########
  # # Output .csvs of Consented, dedup long , and dat.wide
  write.csv(dat.wide, paste0(dir.output, "Data_Wide-",Sys.Date(),".csv"), row.names = F)
  write.csv(Consented, paste0(dir.output,"Consented-",Sys.Date(),".csv"))
  write.csv(cons_sml, paste0(dir.gen2,"Consented_sml",Sys.Date(),".csv"))
  write.csv(data, paste0(dir.output, "Data_Long",Sys.Date(),".csv"))
  write.csv(data_sml, paste0(dir.gen2, "Data_Long_sml",Sys.Date(),".csv"))
  write.csv(dat.wide, paste0(dir.gen2, "Data_Wide",Sys.Date(),".csv"))
  
  write_parquet(cons_sml, paste0(dir.gen2,"Consented_sml",Sys.Date(),".parquet")) 
  write_parquet(data_sml, paste0(dir.gen2,"Data_Long_sml",Sys.Date(),".parquet"))
  write_parquet(dat.wide, paste0(dir.gen2,"Data_Wide_sml",Sys.Date(),".parquet"))
  
  return(list(Consented, data, dat.wide))
} #end of function

