# Cleaning script
# 3 Nov 2021

function(data_raw_dta, feroxus, ivr){
  # if (Sys.info()['sysname'] == 'Darwin') {
  #   r.functions.dir <- '/Users/lshsl8/Documents/GitHub/rammps/'
  # } else if (Sys.info()['sysname'] == 'Windows') {
  #   r.functions.dir <- 'https://raw.githubusercontent.com/kellymccain28/rammps/main/'
  # }
  # Pulling in the load package function R file
  # Load function to install list of packages
  #ldpkg <- dget("ldpkg.R")
  
  
  # Load/install packages 
  pkgs <- c('plyr','tidyverse', 'readxl','date',  'tidyselect', 'httr', 'jsonlite','sjlabelled', 'lubridate')
  # ldpkg(pkgs)
  lapply(pkgs, require, character.only = TRUE)
  
  #API Request with digest authentication
  # request <-
  #   GET("https://rammps.surveycto.com/api/v2/forms/data/wide/json/cati_lang_reassign?date=0",
  #       authenticate("kelly.mccain@lshtm.ac.uk", "rammpS!CTOsurvey"))
  # 
  # #retrieve the contents of a request as a character vector
  # data_text <- content(request, "text")
  # 
  #convert from JSON data to R object
 # data_raw <- fromJSON(data_text, flatten = TRUE) 
  
  
    # Read in the data
    dir.input <- paste0(Sys.getenv('USERPROFILE'),"/OneDrive - London School of Hygiene and Tropical Medicine/Documents/General/Partners/UNIKIN/SurveyCTO Audits/")
    dir.output <- paste0(Sys.getenv('USERPROFILE'),"/OneDrive - London School of Hygiene and Tropical Medicine/Documents/General/Partners/UNIKIN/SurveyCTO Audits/Clean data/")
    dir.gen <- paste0(Sys.getenv('USERPROFILE'),"/OneDrive - London School of Hygiene and Tropical Medicine/Documents/General/Partners/UNIKIN/")
    dir.gen2 <- paste0(Sys.getenv('USERPROFILE'),"/London School of Hygiene and Tropical Medicine/rammps/RAMMPS_DRCDashboard/")
  
  `%notin%` <- Negate(`%in%`)
  
  data_raw1 <- data_raw_dta %>%
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
  
  # Long data with only essential variables 
  data_raw1 <- data_raw1 %>%
    dplyr::select(vars_select(names(data_raw1), !starts_with('log') & !starts_with('next') & !starts_with('find') 
                              & !starts_with('add') & !starts_with('call_end') & !starts_with('timestamp') &
                                !starts_with('call_info') & !starts_with('length_s') & !starts_with('start_') &
                                !starts_with('end_') & !starts_with('pn') & !starts_with('call_simp') & 
                                !starts_with('all_') & !starts_with('all_pn')&!starts_with('num_') 
    )) %>%
    dplyr::filter(!is.na(devicephonenum))
  
  # adding date variable 
  data_raw1$Date_Interview <- substr(data_raw1$endtime, 1, 12)#gsub('.{11}$', '', data_raw1$endtime)
  
  data_raw1 <- data_raw1 %>%
    dplyr::mutate(Date_Interview1 = ifelse(grepl('2017', Date_Interview), substr(submissiondate, 1, 12), Date_Interview)) %>%
    dplyr::mutate(Date_Interview2 = as.POSIXct(Date_Interview1, format="%h %d, %Y"),
                  Date_Interview = format(Date_Interview2, "%Y-%m-%d"),
                  Date_Interview = as.Date(Date_Interview)
                  # Date_Interview = as_POSIXct(Date_Interview, '%Y-%m-%d',tz = "GMT")
    ) 
  
  data_raw1 <- data_raw1 %>%
    dplyr::mutate(month_interview = lubridate::floor_date(as.Date(Date_Interview), 'month'),#month(Date_Interview),
                  month_interview = case_when(month_interview == '2022-01-01'~ 'Jan-22',
                                              month_interview == '2022-02-01'~ 'Feb-22',
                                              month_interview == '2022-03-01'~ 'Mar-22',
                                              month_interview == '2022-04-01'~'Apr-22',
                                              month_interview == '2022-05-01'~'May-22',
                                              month_interview == '2022-06-01'~'Jun-22',
                                              month_interview == '2022-07-01'~'Jul-22',
                                              month_interview == '2022-08-01'~'Aug-22',
                                              month_interview == '2021-08-01'~'Aug-21',
                                              month_interview == '2021-09-01'~'Sep-21',
                                              month_interview == '2021-10-01'~'Oct-21',
                                              month_interview == '2021-11-01'~'Nov-21',
                                              month_interview == '2021-12-01'~'Dec-21'),
                  # month.interview = factor(month.interview, levels = c('Aug-21','Sep-21','Oct-21','Nov-21','Dec-21','Jan-22','Feb-22',
                  #                                                      'Mar-22','Apr-22', 'May-22','Jun-22', 'Jul-22','Aug-22')),
                  compandcons = ifelse(call_status == 1 & now_complete=='Yes'& consent ==1, 1,0)
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
    temp %>%# if there is a duplicated consented interview, then remove later one
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
  
  # # Long data with only essential variables 
  data_raw1 <- data_raw1 %>%
    dplyr::filter(!is.na(devicephonenum)) %>%
    group_by(caseid) %>%
    dplyr::mutate(caseidnew_ = cur_group_id()) %>%
                    ungroup()
  ids <- data_raw1 %>%
    select(caseidnew_, caseid_original, caseid) %>% distinct() #Caseidnew_ is encrypted; caseid_original is the original id, caseid is the ID-Phone number
  write_csv(ids, paste0(dir.input, 'Datasets to Share/CaseidLookup/CaseidLookupTable', Sys.Date(),'.csv'))

  data_raw1 <- data_raw1 %>%
    select(-c(caseid_original)) %>%
    dplyr::mutate(caseid = caseidnew_)
  
  
  # Change the names of the dataset
  data_clean <- data_raw1 %>% 
    dplyr::mutate(region = case_when(as.character(kinshasa) == '1'~ '1',
                                      as.character(nordkivu) == '1'~'2',
                                      as.character(region) == '3' ~ '3',
                                      as.character(region) == '4' ~ '4',
                                      as.character(region) == '5'~'5',
                                      as.character(region) == as.character(e6) & ((call_status == '1' & consent == 1)
                                                                                  |(as.character(e7) == '1'& e7_b > 90) | (as.character(e7) == '2' & e7_b >3) |
                                           (as.character(e7) == '3' & e7_b >0) | as.character(e7) == '4') ~ as.character(region),
                                      #as.character(region) == '2' & as.character(e6) == '2' & ((as.character(e7) == '1'& e7_b > 90) | (as.character(e7) == '2' & e7_b >3) | (as.character(e7) == '3' & e7_b >0) | as.character(e7) == '4') ~ '2',
                                      as.character(region) != as.character(e6) & ((as.character(e7) == '1'& e7_b > 90) | (as.character(e7) == '2' & e7_b >3) | (as.character(e7) == '3' & e7_b >0) | as.character(e7) == '4') ~ as.character(e6),
                                      as.character(e5) == '3' ~ '3',
                                      is.na(region) ~ '',
                                      TRUE ~ '66'),
                  region = as.integer(region)) %>%
    dplyr::rename(Resp_Language = l1_a_1,
                  Resp_Age = e3,
                  Resp_Consent = consent,
                  Resp_Region = region,
                  Resp_Educ = education_level,
                  Resp_Sex = e2,
                  Resp_Marriage = marriage_status) %>%
    dplyr::mutate(Resp_Age = ifelse(!is.na(corr_respagemomalive_1), corr_respagemomalive_1, 
                                    ifelse(!is.na(corr_respagemomdead_1), corr_respagemomdead_1,
                                           ifelse(!is.na(corr_respagedadalive_1), corr_respagedadalive_1,
                                                  ifelse(!is.na(corr_respagedaddead_1), corr_respagedaddead_1, Resp_Age)))),
                  # Resp.Region.name = case_when(Resp_Region == '1' ~ 'Kinshasa', Resp_Region == '2' ~ 'Nord Kivu', Resp_Region == '3' ~ 'Other', Resp.Region == '4'~ "Don't Know"),
                  Resp_Sex = ifelse(Resp_Sex == 1, 'Male',
                                    ifelse(Resp_Sex ==2, "Female",NA)),
                  # Resp.Age.grp = cut(as.numeric(Resp_Age), c(17,40,100)),
                  # Resp.Age.pyr = cut(as.numeric(Resp_Age), c(14,19,29,39,49,59,64))
                  ) %>%
    dplyr::mutate(call_status = ifelse((Date_Interview < '2021-09-08' & call_status == '2'), '9', call_status),
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
    dplyr::rename(Sisters = numbersistersbrothers_1,
                  Brothers = numbersistersbrothers_bro,
                  # Sis1_Dead = ssh2_died_1,
                  # Sis2_Dead = ssh2_died_2,
                  # Sis1_Dead_2019 = ssh3_died_1,
                  # Sis2_Dead_2019 = ssh3_died_2,
                  # Bro1_Dead = ssh2_died_bro,
                  # Bro1_Dead_Yr = ssh3_died_bro,
                  M_Dead = ps1_biological_parents_1,
                  M_dead_Yr = ps6_pryeardied_1,
                  M_dead_Age = ps5_p_died_age_1,
                  M_alive_age = ps2_parents_age_1,
                  F_Dead = ps1_biological_parents2_1,
                  F_dead_Yr = ps6_pryeardied2_1,         
                  F_dead_Age = ps5_p_died_age2_1,
                  F_alive_age = ps2_parents_age2_1) %>%
    dplyr::mutate(M_alive_age = ifelse(!is_na(corr_momagealive_1), corr_momagealive_1, M_alive_age),
                  M_dead_Age = ifelse(!is_na(corr_momagedead_1), corr_momagedead_1, M_dead_Age),
                  F_alive_age = ifelse(!is_na(corr_dadagealive_1), corr_dadagealive_1, F_alive_age),
                  F_dead_Age = ifelse(!is_na(corr_dadagedead_1), corr_deadagedead_1, F_dead_Age),
                  M_dead_Yr = ifelse(!is_na(corr_momyeardead_1), corr_momyeardead_1, M_dead_Yr),
                  F_dead_Yr = ifelse(!is_na(corr_dadyeardead_1), corr_dadyeardead_1, F_dead_Yr)) %>%
    # modifications for hhsize density and cdr
    dplyr::rename(U5Death = hd4_b,
                  O5Death = hd6_b,
                  U5 = hd2_b,
                  O5 = hd3_b,
                  OwnPhone = b4) %>%
    mutate(HHsize = as.numeric(U5)+as.numeric(O5)+1,
           #Sib1_Dead = as.numeric(Sis1_Dead) + as.numeric(Bro1_Dead)#,
           # U5Death.Age = hd5b_0_1,# need multiple for each loop
           # O5Death.Age = hd7b_age_1
           ) %>%# need multiple for each loop) 
    # modifications for duration 
    # dplyr::rename(Total = duration,
    #               Elig.Time = elig_time,
    #               Consent.Time = consent_time,
    #               Arrangement.Time = arrangements_time,
    #               Background.Time = background_time,
    #               C19.Time = vax_time,
    #               HH.Time = hh_time,
    #               PS.Time = ps_time,
    #               SSH.Time = ssh_time,
    #               DB.Time = db_time#,
    #               #Phone.duration = #phone_call_duration
    #               ) %>%
    # rowwise() %>%
    # dplyr::mutate(Phone.duration1= sum(Elig.Time, Consent.Time, Arrangement.Time, Background.Time, C19.Time, HH.Time, PS.Time, SSH.Time, DB.Time, na.rm=T),
    #               Phone.duration = ifelse(is.na(Phone.duration1), phone_call_duration, Phone.duration1)) %>%
    # ungroup() %>%
    dplyr::rename(#Vaccine.Dose = vaccine_dose,
                  vaccine_likelihood = dose_frequency
    #               Vaccine.Name = vaccine_names,
    #               Vaccine.Den.Reasons = vaccines_den_reasons,
    #               Other.Den.Reasons = other_den_reasons,
    #               Vaccine.Yes.Reasons = vaccines_yes_reasons,
    #               Other.Yes.Reasons = other_yes_reasons
    ) %>%
    dplyr::rename(S_CD_Preg1 = cd1_copy_1_1,
                  S_CD_Preg2 = cd1_copy_1_2,
                  S_CD_Preg3 = cd1_copy_1_3,
                  S_CD_Preg4 = cd1_copy_1_4,
                  S_CD_Cbirth1 = cd2_copy_1_1,
                  S_CD_Cbirth2 = cd2_copy_1_2,
                  S_CD_Cbirth3 = cd2_copy_1_3,
                  S_CD_Cbirth4 = cd2_copy_1_4,                
                  S_CD_2M1 = cd3_copy_1_1,
                  S_CD_2M2 = cd3_copy_1_2,
                  S_CD_2M3 = cd3_copy_1_3,
                  S_CD_2M4 = cd3_copy_1_4) %>%
    dplyr::mutate(M_VisitHF = cd8_1,
                  M_AdmittedHF = cd9_1,
                  M_DieHF = cd10_1,
                  F_VisitHF = cd8_2_1,
                  F_AdmittedHF = cd9_2_1,
                  F_DieHF = cd10_2_1,
                  S_VisitHF1 = cd8_copy_1_1,
                  S_VisitHF2 = cd8_copy_1_2,
                  S_VisitHF3 = cd8_copy_1_3,
                  S_VisitHF4 = cd8_copy_1_4,
                  B_VisitHF1 = cd8_copy_bro_1,
                  B_VisitHF2 = cd8_copy_bro_2,
                  B_VisitHF3 = cd8_copy_bro_3,
                  B_VisitHF4 = cd8_copy_bro_4,
                  B_VisitHF5 = cd8_copy_bro_5,
                  B_VisitHF6 = cd8_copy_bro_6,
                  S_AdmitHF1 = cd9_copy_1_1,
                  S_AdmitHF2 = cd9_copy_1_2,
                  S_AdmitHF3 = cd9_copy_1_3,
                  S_AdmitHF4 = cd9_copy_1_4,
                  B_AdmitHF1 = cd9_copy_bro_1,
                  B_AdmitHF2 = cd9_copy_bro_2,
                  B_AdmitHF3 = cd9_copy_bro_3,
                  B_AdmitHF4 = cd9_copy_bro_4,
                  B_AdmitHF5 = cd9_copy_bro_5,
                  #B_AdmitHF6 = cd9_copy_bro_6,
                  S_DieHF1 = cd10_copy_1_1,
                  S_DieHF2 = cd10_copy_1_2,
                  S_DieHF3 = cd10_copy_1_3,
                  S_DieHF4 = cd10_copy_1_4,
                  B_DieHF1 = cd10_copy_bro_1,
                  B_DieHF2 = cd10_copy_bro_2,
                  B_DieHF3 = cd10_copy_bro_3,
                  B_DieHF4 = cd10_copy_bro_4,
                  B_DieHF5 = cd10_copy_bro_5,
                  #B_DieHF6 = cd10_copy_bro_6
    ) %>%
    # dplyr::mutate(F.Dead.COVID = case_when(cd14_2_1 == '1' ~ 'Very likely',
    #                                        cd14_2_1 =="2" ~ 'Somewhat likely',
    #                                        cd14_2_1 =="3" ~ 'Somewhat unlikely',
    #                                        cd14_2_1 =="4" ~ 'Very unlikely',
    #                                        cd14_2_1 =="5" ~ 'Dont know',
    #                                        cd14_2_1 =="R" ~ 'R'),
    #               F.Dead.Viol = case_when(cd5_2_1 == "1" ~ 'Y',
    #                                       cd5_2_1 =="2" ~ 'N',
    #                                       cd5_2_1 =="3" ~ 'Dk',
    #                                       cd5_2_1 =="R" ~ 'R'),
    #               F.Dead.Acc = case_when(cd4_2_1 == "1" ~ 'Y',
    #                                      cd4_2_1 =="2" ~ 'N',
    #                                      cd4_2_1 =="3" ~ 'Dk',
    #                                      cd4_2_1 =="R" ~ 'R')) %>%
    # dplyr::mutate(F.Dead.External = if_else(F.Dead.Viol=="Y"|F.Dead.Acc=="Y", "Y", "N")) %>%
    # dplyr::mutate(M.Dead.COVID = case_when(cd14_1 == "1" ~ 'Very likely',
    #                                        cd14_1 =="2" ~ 'Somewhat likely',
    #                                        cd14_1 =="3" ~ 'Somewhat unlikely',
    #                                        cd14_1 =="4" ~ 'Very unlikely',
    #                                        cd14_1 =="5" ~ 'Dont know',
    #                                        cd14_1 =="R" ~ 'R'),
    #               M.Dead.Viol = case_when(cd5_1 == "1" ~ 'Y',
    #                                       cd5_1 =="2" ~ 'N',
    #                                       cd5_1 =="3" ~ 'Dk',
    #                                       cd5_1 =="R" ~ 'R'),
    #               M.Dead.Acc = case_when(cd4_1 == "1" ~ 'Y',
    #                                      cd4_1 =="2" ~ 'N',
    #                                      cd4_1 =="3" ~ 'Dk',
    #                                      cd4_1 =="R" ~ 'R'),
    #               M.Dead.Preg = case_when(cd1_1 == "1" ~ 'Y',
    #                                       cd1_1 =="2" ~ 'N',
    #                                       cd1_1 =="3" ~ 'Dk',
    #                                       cd1_1 =="R" ~ 'R'),
    #               M.Dead.CB = case_when(cd2_1 == "1" ~ 'Y',
    #                                     cd2_1 =="2" ~ 'N',
    #                                     cd2_1 =="3" ~ 'Dk',
    #                                     cd2_1 =="R" ~ 'R'),
    #               M.Dead.post.Preg = case_when(cd3_1 == "1" ~ 'Y',
    #                                            cd3_1 =="2" ~ 'N',
    #                                            cd3_1 =="3" ~ 'Dk',
    #                                            cd3_1 =="R" ~ 'R')) %>%
    # dplyr::mutate(M.Dead.External = if_else(M.Dead.Viol=="Y"|M.Dead.Acc=="Y", "Y", "N")) %>%
    # dplyr::mutate(M.Dead.mat = if_else(M.Dead.CB=="Y"|M.Dead.Preg=="Y"|M.Dead.post.Preg=="Y", "Y", "N")) %>%
    # dplyr::mutate(Vaccine.Name = case_when(Vaccine.Name == "1" ~ 'AstraZeneca',
    #                                        Vaccine.Name == "2" ~ 'Pfizer-BioNTech',
    #                                        Vaccine.Name == "3" ~ 'Moderna',
    #                                        Vaccine.Name == "4" ~ "Johnson & Johnson's Janssen",
    #                                        Vaccine.Name == "5" ~ "Don't Know",
    #                                        Vaccine.Name == "6" ~ 'Sinovac',
    #                                        Vaccine.Name == "7" ~ 'Sputnik',
    #                                        Vaccine.Name == 'R' ~ 'Refuse'
    # )
    # ) %>%
    dplyr::mutate(
      # Resp.Region.name = case_when(Resp.Region == 1 ~ 'Kinshasa', 
                                               # Resp.Region == 2 ~ 'Nord Kivu', 
                                               # Resp.Region == 3 ~ 'Other', 
                                               # Resp.Region == 4~ "Don't Know"),
      # vaccine_dose = case_when(vaccine_dose == 1 ~ 'One Dose',
      #                          vaccine_dose == 2 ~ 'Two Doses',
      #                          vaccine_dose == 3 ~ 'No vaccine',
      #                          vaccine_dose == 4 ~ "Don't Know",
      #                          is.na(vaccine_dose) ~ 'Refuse'
      # ),
      OwnPhone = case_when(OwnPhone ==1 ~ "Owns phone",
                                       OwnPhone ==2 ~ "Belongs to somebody else",
                                       OwnPhone ==4 ~ "Refuse to answer")) %>%
    # dplyr::mutate(F.Dead.setting.death = case_when(cd16_2_1 == "1" ~ 'Health Facility',
    #                                                cd16_2_1 == "2"  ~ 'Home',
    #                                                cd16_2_1 == "3"  ~ 'On road to health facility',
    #                                                cd16_2_1 == "4"  ~ 'Other',
    #                                                cd16_2_1 == "5"  ~ 'DK',
    #                                                is.na(cd16_2_1)   ~ 'missing'),
    #               F.Dead.Buried.Prov = case_when(cd17_2_1 == "1" ~ 'Kinshasa',
    #                                              cd17_2_1 == "2"  ~ 'NK',
    #                                              cd17_2_1 == "3"  ~ 'Dk',
    #                                              cd17_2_1 == "4"  ~ 'Refuse',
    #                                              is.na(cd17_2_1)   ~ 'missing'),
    #               F.Dead.Buried.plot = case_when(cd18_2_1 == "1" ~ 'Cemetery',
    #                                              cd18_2_1 == "2"  ~ 'Family plot',
    #                                              cd18_2_1 == "3"  ~ 'Was not buried',
    #                                              cd18_2_1 == "4"  ~ 'Other',
    #                                              cd18_2_1 == "5"  ~ 'DK',
    #                                              cd18_2_1 == "R"  ~ 'R',
    #                                              is.na(cd18_2_1)   ~ 'missing'),
    #               F.Dead.Location.Death = case_when(cd15_2_1 == "1" ~ 'Kinshasa',
    #                                                 cd15_2_1 == "2"  ~ 'NK',
    #                                                 cd15_2_1 == "3"  ~ 'Dk',
    #                                                 cd15_2_1 == "4"  ~ 'Refuse',
    #                                                 is.na(cd15_2_1)   ~ 'missing'),
    #               F.Dead.LiveB4 = case_when(live_before_passing2_1 == "1" ~ 'Same HH',
    #                                         live_before_passing2_1 == "2"  ~ 'Same Village/town',
    #                                         live_before_passing2_1 == "3"  ~ 'Same Prov',
    #                                         live_before_passing2_1 == "4"  ~ 'Diff Prov',
    #                                         live_before_passing2_1 == "R"  ~ 'Refuse',
    #                                         is.na(live_before_passing2_1)   ~ 'missing'),
    #               F.Dead.Who.buried = case_when(cd19_2_1 == "1" ~ 'Health personnel',
    #                                             cd19_2_1 == "2"  ~ 'Family members only',
    #                                             cd19_2_1 == "3"  ~ 'Both Family members plus selected people from community',
    #                                             cd19_2_1 == "4"  ~ 'Dont know',
    #                                             cd19_2_1 == "R"  ~ 'Refuse',
    #                                             is.na(cd19_2_1)   ~ 'missing'),
    #               F.Dead.Local.auth = case_when(cd20_2_1 == "1" ~ 'Y',
    #                                             cd20_2_1 =="2" ~ 'N',
    #                                             cd20_2_1 =="3" ~ 'Dk',
    #                                             cd20_2_1 =="R" ~ 'R')) %>%
    # dplyr:: mutate(M.Dead.setting.death = case_when(cd16_1 == "1" ~ 'Health Facility',
    #                                                 cd16_1 == "2"  ~ 'Home',
    #                                                 cd16_1 == "3"  ~ 'On road to health facility',
    #                                                 cd16_1 == "4"  ~ 'Other',
    #                                                 cd16_1 == "5"  ~ 'DK',
    #                                                 is.na(cd16_1)   ~ 'missing'),
    #                M.Dead.Buried.Prov = case_when(cd17_1 == "1" ~ 'Kinshasa',
    #                                               cd17_1 == "2"  ~ 'NK',
    #                                               cd17_1 == "3"  ~ 'Dk',
    #                                               cd17_1 == "4"  ~ 'Refuse',
    #                                               is.na(cd17_1)   ~ 'missing'),
    #                M.Dead.Buried.plot = case_when(cd18_1 == "1" ~ 'Cemetery',
    #                                               cd18_1 == "2"  ~ 'Family plot',
    #                                               cd18_1 == "3"  ~ 'Was not buried',
    #                                               cd18_1 == "4"  ~ 'Other',
    #                                               cd18_1 == "5"  ~ 'DK',
    #                                               cd18_1 == "R"  ~ 'R',
    #                                               is.na(cd18_1)   ~ 'missing'),
    #                M.Dead.Location.Death = case_when(cd15_1 == "1" ~ 'Kinshasa',
    #                                                  cd15_1 == "2"  ~ 'NK',
    #                                                  cd15_1 == "3"  ~ 'Dk',
    #                                                  cd15_1 == "4"  ~ 'Refuse',
    #                                                  is.na(cd15_1)   ~ 'missing'),
    #                M.Dead.LiveB4 = case_when(live_before_passing_1 == "1" ~ 'Same HH',
    #                                          live_before_passing_1 == "2"  ~ 'Same Village/town',
    #                                          live_before_passing_1 == "3"  ~ 'Same Prov',
    #                                          live_before_passing_1 == "4"  ~ 'Diff Prov',
    #                                          live_before_passing_1 == "R"  ~ 'Refuse',
    #                                          is.na(live_before_passing_1)   ~ 'missing'),
    #                M.Dead.Who.buried = case_when(cd19_1 == "1" ~ 'Health personnel',
    #                                              cd19_1 == "2"  ~ 'Family members only',
    #                                              cd19_1 == "3"  ~ 'Both Family members plus selected people from community',
    #                                              cd19_1 == "4"  ~ 'Dont know',
    #                                              cd19_1 == "R"  ~ 'Refuse',
    #                                              is.na(cd19_1)   ~ 'missing'),
    #                M.Dead.Local.auth = case_when(cd20_1 == "1" ~ 'Y',
    #                                              cd20_1 =="2" ~ 'N',
    #                                              cd20_1 =="3" ~ 'Dk',
    #                                              cd20_1 =="R" ~ 'R')) %>%
    # dplyr:: mutate(M.Dead.C19.Symp = if_else(cd7_1_1!="", "Y", "N"),                                           
    #                F.Dead.C19.Symp = if_else(cd7_2_1!="", "Y", "N")) %>%
    # dplyr::mutate(water_source_lab = case_when(water_source =="1" ~ 'Piped into dwelling',
    #                                            water_source =="2" ~ 'Piped to yard/plot',
    #                                            water_source =="3" ~ 'Public tap',
    #                                            water_source =="4" ~ 'Tubewell/Borehole',
    #                                            water_source =="5" ~ 'Protected well',
    #                                            water_source =="6" ~ 'Unprotected well',
    #                                            water_source =="7" ~ 'Protected spring',
    #                                            water_source =="8" ~ 'Unprotected spring',
    #                                            water_source =="9" ~ 'Rainwater',
    #                                            water_source =="10" ~ 'Bottled water',
    #                                            water_source =="11" ~ 'Cart with small tank',
    #                                            water_source =="12" ~ 'Tank/Drum',
    #                                            water_source =="13" ~ 'Tanker-truck',
    #                                            water_source =="14" ~ 'Surface water',
    #                                            water_source =="15" ~ 'Other',
    #                                            water_source == 'R' ~ 'Refuse')) %>%
    dplyr::mutate(#Resp.Age.grp_lab = case_when(Resp.Age.grp =="(17,40]" ~ '18-39',
                   #                            Resp.Age.grp =="(40,100]" ~ '40+'),
                  # Resp.Region_lab = case_when(Resp.Region =="1" ~ 'Kinshasa',
                  #                             Resp.Region =="2" ~ 'Nord Kivu'),
                  # Resp.Educ_lab = case_when(Resp.Educ =="" ~ 'No Education',
                  #                           Resp.Educ =="PR" ~ 'Primary',
                  #                           Resp.Educ =="SE" ~ 'Secondary',
                  #                           Resp.Educ =="HI" ~ 'Higher',
                  #                           Resp.Educ == '4' ~ 'Refuse'),
                  #Resp.Educ_lab = ordered(Resp.Educ_lab, levels = c('No Education','Primary','Secondary','Higher','Refuse')),
                  Resp_Marriage_lab = case_when(Resp_Marriage ==1 ~ 'Married',
                                                Resp_Marriage ==2 ~ 'Cohabiting',
                                                Resp_Marriage ==3 ~ 'Not in Union',
                                                Resp_Marriage ==4 ~ 'Refuse'),
                  Resp_Marriage_lab = case_when(marital_status ==1 ~ 'Widowed/(cohabiting) partner passed away',
                                                marital_status ==3 ~ 'Divorced/Separated',
                                                Resp_Marriage_lab == 'Cohabiting' | Resp_Marriage_lab == 'Married' ~ 'Married/Cohabiting',
                                                marriage_formal == 3 ~ 'Never married',
                                                marital_status == 4 | marriage_formal == 4  ~ 'Refuse')#,
                  # E4a_lab = case_when(e4a =="1" ~ 'City',
                  #                     e4a =="2" ~ 'Town/Trading Centre',
                  #                     e4a =="3" ~ 'Rural',
                  #                     e4a =="4" ~ 'Refuse'),
                  #E4a_lab = ordered(E4a_lab, levels = c('City','Town/Trading Centre','Rural','Refuse')),
                  # electricity_status_lab = case_when(electricity_status =="1" ~ 'Access to electricity',
                  #                                    electricity_status =="2" ~ 'No access to electricity',
                  #                                    electricity_status == '3' ~ "Don't know",
                                                     # electricity_status == 'R' ~ 'Refuse')
  ) %>%
    # dplyr::mutate(Education.Prim = case_when(highest_grades_a =="10" ~ "less than one year",
    #                                          highest_grades_a =="11" ~ "Première",
    #                                          highest_grades_a =="12" ~ "Deuxième",
    #                                          highest_grades_a =="13" ~ "Troisième",
    #                                          highest_grades_a =="14" ~ "Quatrième",
    #                                          highest_grades_a =="15" ~ "Cinquième",
    #                                          highest_grades_a =="16" ~ "Sixième",
    #                                          highest_grades_a =="98" ~ "Don't know", 
    #                                          highest_grades_a == "-99" ~ "Prefer not to say"),
    #               Education.Sec = case_when(highest_grades_b =="20" ~ "less than one year",
    #                                         highest_grades_b =="21" ~ "Première",
    #                                         highest_grades_b =="22" ~ "Deuxième",
    #                                         highest_grades_b =="23" ~ "Troisième",
    #                                         highest_grades_b =="24" ~ "Quatrième",
    #                                         highest_grades_b =="25" ~ "Cinquième",
    #                                         highest_grades_b =="26" ~ "Sixième",
    #                                         highest_grades_b =="98" ~ "Don't know", 
    #                                         highest_grades_b == "-99" ~ "Prefer not to say"),
    #               Education.Hig = case_when(highest_grades_c =="30" ~ "less than one year",
    #                                         highest_grades_c =="31" ~ "1er Graduat",
    #                                         highest_grades_c =="32" ~ "2ème Graduat",
    #                                         highest_grades_c =="33" ~ "3ème Graduat",
    #                                         highest_grades_c =="34" ~ "1ère Licence",
    #                                         highest_grades_c =="35" ~ "2ème Licence",
    #                                         highest_grades_c =="98" ~ "Don't know", 
    #                                         highest_grades_c == "-99" ~ "Prefer not to say")) %>%
    # filter(call_num %notin% c(0,6)) %>%
    # dplyr::mutate(Roof = case_when(roofing_material =="1" ~ 'No roof',
    #                                roofing_material =="2" ~ 'Grass/Thatch/Palm/Straw',
    #                                roofing_material =="3" ~ 'Cardboard',
    #                                roofing_material =="4" ~ 'Plastic/Tarpaulin',
    #                                roofing_material =="5" ~ 'Wood',
    #                                roofing_material =="6" ~ 'Metal (corrugated iron)',
    #                                roofing_material =="7" ~ 'Cement/Tiles',
    #                                roofing_material =="8" ~ 'Other',
    #                                roofing_material =="9" ~ 'Dont Know',
    #                                roofing_material =="10" ~ 'Refuse')) %>%
    dplyr::mutate(
                  # Location_call = case_when(call_location =="1" ~ 'Home',
                  #                           call_location =="2" ~ 'Work/Uni',
                  #                           call_location =="3" ~ 'Other',
                  #                           call_location =="4" ~ 'Refuse'),
                  # Roof = roofing_material, 
                  agegp = cut(Resp_Age, breaks=c(17, 30, 40, 50, 60, 70),
                                         labels=c("17-29","30-39", "40-49", "50-59", "60+"))) 
  

#filterin g out even more variables
# data_clean <- data_clean %>%
#   select(c(SubmissionDate, call_num, caseid, phone_1,response_1,E10, L1_A_1, L1_B_1, L2_1, pull_label, pull_IVRprovince, enumerator,Resp.Age,Resp.Consent,Resp.Region,Resp.Region.name,Resp.Educ,Resp.Sex,
#            Resp.Marriage,water_source_lab,Education.Prim,Education.Sec,Education.Hig,Sisters,Brothers,Sis1.Dead,Sis2.Dead,Sis1.Dead.2019,Sis2.Dead.2019,Bro1.Dead,
#            Bro1.Dead.Yr,M.Dead,M.dead.Yr,M.dead.Age,M.alive.age,F.Dead,F.dead.Yr,F.dead.Age,F.alive.age,U5Death,hd5b_0_1,hd5b_0_2,hd7b_age_1,hd7b_age_2,hd7b_age_3,hd7b_age_4,
#            O5Death,O5Death.Age,U5,O5,OwnPhone,HHsize,
#            Total,Elig.Time,Consent.Time,Arrangement.Time,Background.Time,C19.Time,HH.Time,PS.Time,SSH.Time,DB.Time,Phone.duration,Vaccine.Dose,Vaccine.Likelihood,
#            Vaccine.Name,Vaccine.Den.Reasons,Other.Den.Reasons,Vaccine.Yes.Reasons,Other.Yes.Reasons,Resp.Age.grp,Resp.Age.pyr,call_status_label_eng,hd1_householdrel,
#            call_status,enumerator,S_CD_Preg1,S_CD_Preg2,S_CD_Preg3,S_CD_Preg4,S_CD_Cbirth1,S_CD_Cbirth2,S_CD_Cbirth3,S_CD_Cbirth4,S_CD_2M1,S_CD_2M2,S_CD_2M3,S_CD_2M4,
#            M_VisitHF,M_AdmittedHF,M_DieHF,F_VisitHF,F_AdmittedHF,F_DieHF,S_VisitHF1,S_VisitHF2,S_VisitHF3,S_VisitHF4,B_VisitHF1,B_VisitHF2,B_VisitHF3,B_VisitHF4,
#            B_VisitHF5,B_VisitHF6,S_AdmitHF1,S_AdmitHF2,S_AdmitHF3,S_AdmitHF4,B_AdmitHF1,B_AdmitHF2,B_AdmitHF3,B_AdmitHF4,B_AdmitHF5,S_DieHF1,S_DieHF2,
#            S_DieHF3,S_DieHF4,B_DieHF1,B_DieHF2,B_DieHF3,B_DieHF4,B_DieHF5,mom2019die_1,momspecificyear_1,momdie10yrs_1,momageatbirth_alive_1,momageatbirth_dead_1,
#            note_momagealive_1,note_momagedead_1,dad2019die_1,dadspecificyear_1,daddie10yrs_copy_1,dadageatbirth_dead_1,dadageatbirth_alive_1,note_dadagedead_1,note_dadagealive_1,
#            F.Dead.COVID,F.Dead.Viol,F.Dead.Acc,F.Dead.External,M.Dead.COVID,M.Dead.Viol,M.Dead.Acc,M.Dead.Preg,M.Dead.CB,M.Dead.post.Preg,M.Dead.External,M.Dead.mat,
#            F.Dead.setting.death,F.Dead.Buried.Prov,F.Dead.Buried.plot,F.Dead.Location.Death,F.Dead.LiveB4,F.Dead.Who.buried,F.Dead.Local.auth,M.Dead.setting.death,
#            M.Dead.Buried.Prov,M.Dead.Buried.plot,M.Dead.Location.Death,M.Dead.LiveB4,M.Dead.Who.buried,M.Dead.Local.auth,M.Dead.C19.Symp,F.Dead.C19.Symp,Roof,
#            other_waterSource,Location_call,specify_location, Elig,D1,roothing_other, now_complete,specify_location,vaccines_den_reasons_1,
#            vaccines_den_reasons_2,vaccines_den_reasons_3,vaccines_den_reasons_4,vaccines_den_reasons_5,vaccines_den_reasons_6,vaccines_den_reasons_7,vaccines_den_reasons_8,
#            vaccines_den_reasons_9,vaccines_den_reasons_10,vaccines_den_reasons_11,vaccines_den_reasons_12,vaccines_den_reasons_13,vaccines_den_reasons_14,
#            vaccines_den_reasons_15,vaccines_den_reasons_16,vaccines_den_reasons_17,vaccines_den_reasons_18,vaccines_den_reasons_19, vaccines_den_reasons_20,vaccines_yes_reasons_1,
#            vaccines_yes_reasons_2,vaccines_yes_reasons_3,vaccines_yes_reasons_4,vaccines_yes_reasons_5,vaccines_yes_reasons_6,vaccines_yes_reasons_7,vaccines_yes_reasons_8,
#            vaccines_yes_reasons_9,vaccines_yes_reasons_10,vaccines_yes_reasons_11, endtime,call_location, Resp.Educ_lab,Resp.Age.grp_lab,Resp.Region_lab,Resp.Marriage_lab,E4a, E4a_lab,electricity_status_lab,
#            Resp.Language,CD5_copy_1_1,CD5_copy_1_2,CD5_copy_1_3,CD5_copy_1_4,CD5_copy_bro_1,CD5_copy_bro_2,CD5_copy_bro_3,CD5_copy_bro_4,CD5_copy_bro_5,CD5_copy_bro_6,
#            CD7_copy_1_1,CD7_copy_1_2,CD7_copy_1_3,CD7_copy_1_4,CD7_copy_bro_1,CD7_copy_bro_2,CD7_copy_bro_3,CD7_copy_bro_4,
#            CD7_copy_bro_5,CD7_copy_bro_6,ssh4_yearDied_bro_1,ssh4_yearDied_bro_2,ssh4_yearDied_bro_3,ssh4_yearDied_bro_4,ssh4_yearDied_bro_5,ssh4_yearDied_bro_6,
#            ssh4_yearDied_copy_1_1,ssh4_yearDied_copy_1_2,ssh4_yearDied_copy_1_3,ssh4_yearDied_copy_1_4,S_CD_Cbirth1,S_CD_Cbirth2,S_CD_Cbirth3,S_CD_Cbirth4,S_CD_2M1,
#            S_CD_2M2,S_CD_2M3,S_CD_2M4,CD4_copy_1_1,CD4_copy_1_2,CD4_copy_1_3,CD4_copy_1_4,CD4_copy_bro_1,CD4_copy_bro_2,CD4_copy_bro_3,CD4_copy_bro_4,CD4_copy_bro_5,
#            CD4_copy_bro_6,CD16_copy_bro_1,CD16_copy_bro_2,CD16_copy_bro_3,CD16_copy_bro_4,CD16_copy_bro_5,CD16_copy_bro_6,CD16_copy_1_1,CD16_copy_1_2,CD16_copy_1_3,
#            CD16_copy_1_4,CD18_copy_bro_1,CD18_copy_bro_2,CD18_copy_bro_3,CD18_copy_bro_4,CD18_copy_bro_5,CD18_copy_bro_6,CD18_copy_1_1,CD18_copy_1_2,CD18_copy_1_3,
#            CD18_copy_1_4,CD20_copy_bro_1,CD20_copy_bro_2,CD20_copy_bro_3,CD20_copy_bro_4,CD20_copy_bro_5,CD20_copy_bro_6,CD20_copy_1_1,CD20_copy_1_2,CD20_copy_1_3,
#            CD20_copy_1_4,CD7_copy_bro_1_1,CD7_copy_bro_1_2,CD7_copy_bro_1_3,CD7_copy_bro_1_4,CD7_copy_bro_1_5,CD7_copy_bro_1_6,CD7_copy_bro_2_1,CD7_copy_bro_2_2,
#            CD7_copy_bro_2_3,CD7_copy_bro_2_4,CD7_copy_bro_2_5,CD7_copy_bro_2_6,CD7_copy_bro_3_1,CD7_copy_bro_3_2,CD7_copy_bro_3_3,CD7_copy_bro_3_4,CD7_copy_bro_3_5,
#            CD7_copy_bro_3_6,CD7_copy_bro_4_1,CD7_copy_bro_4_2,CD7_copy_bro_4_3,CD7_copy_bro_4_4,CD7_copy_bro_4_5,CD7_copy_bro_4_6,CD7_copy_bro_5_1,CD7_copy_bro_5_2,
#            CD7_copy_bro_5_3,CD7_copy_bro_5_4,CD7_copy_bro_5_5,CD7_copy_bro_5_6,CD7_copy_bro_6_1,CD7_copy_bro_6_2,CD7_copy_bro_6_3,CD7_copy_bro_6_4,CD7_copy_bro_6_5,
#            CD7_copy_bro_6_6,CD7_copy_bro_7_1,CD7_copy_bro_7_2,CD7_copy_bro_7_3,CD7_copy_bro_7_4,CD7_copy_bro_7_5,CD7_copy_bro_7_6,CD7_copy_bro_8_1,CD7_copy_bro_8_2,
#            CD7_copy_bro_8_3,CD7_copy_bro_8_4,CD7_copy_bro_8_5,CD7_copy_bro_8_6,CD7_copy_bro_9_1,CD7_copy_bro_9_2,CD7_copy_bro_9_3,CD7_copy_bro_9_4,CD7_copy_bro_9_5,
#            CD7_copy_bro_9_6,CD7_copy_bro_10_1,CD7_copy_bro_10_2,CD7_copy_bro_10_3,CD7_copy_bro_10_4,CD7_copy_bro_10_5,CD7_copy_bro_10_6,CD7_copy_bro_11_1,CD7_copy_bro_11_2,
#            CD7_copy_bro_11_3,CD7_copy_bro_11_4,CD7_copy_bro_11_5,CD7_copy_bro_11_6,CD7_copy_bro_12_1,CD7_copy_bro_12_2,CD7_copy_bro_12_3,CD7_copy_bro_12_4,CD7_copy_bro_12_5,
#            CD7_copy_bro_12_6,CD7_copy_bro_R_1,CD7_copy_bro_R_2,CD7_copy_bro_R_3,CD7_copy_bro_R_4,CD7_copy_bro_R_5,CD7_copy_bro_R_6,CD7_copy_1_1_1,CD7_copy_1_1_2,CD7_copy_1_1_3,
#            CD7_copy_1_1_4,CD7_copy_2_1_1,CD7_copy_2_1_2,CD7_copy_2_1_3,CD7_copy_2_1_4,CD7_copy_3_1_1,CD7_copy_3_1_2,CD7_copy_3_1_3,CD7_copy_3_1_4,CD7_copy_4_1_1,CD7_copy_4_1_2,
#            CD7_copy_4_1_3,CD7_copy_4_1_4,CD7_copy_5_1_1,CD7_copy_5_1_2,CD7_copy_5_1_3,CD7_copy_5_1_4,CD7_copy_6_1_1,CD7_copy_6_1_2,CD7_copy_6_1_3,CD7_copy_6_1_4,CD7_copy_7_1_1,
#            CD7_copy_7_1_2,CD7_copy_7_1_3,CD7_copy_7_1_4,CD7_copy_8_1_1,CD7_copy_8_1_2,CD7_copy_8_1_3,CD7_copy_8_1_4,CD7_copy_9_1_1,CD7_copy_9_1_2,CD7_copy_9_1_3,CD7_copy_9_1_4,
#            CD7_copy_10_1_1,CD7_copy_10_1_2,CD7_copy_10_1_3,CD7_copy_10_1_4,CD7_copy_11_1_1,CD7_copy_11_1_2,CD7_copy_11_1_3,CD7_copy_11_1_4,CD7_copy_12_1_1,CD7_copy_12_1_2,
#            CD7_copy_12_1_3,CD7_copy_12_1_4,CD7_copy_R_1_1,CD7_copy_R_1_2,CD7_copy_R_1_3,CD7_copy_R_1_4,CD7_2_1_1,CD7_2_2_1,CD7_2_3_1,CD7_2_4_1,CD7_2_5_1,CD7_2_6_1,CD7_2_7_1,
#            CD7_2_8_1,CD7_2_9_1,CD7_2_10_1,CD7_2_11_1,CD7_2_12_1,CD7_2_R_1,CD7_1_1,CD7_2_1,CD7_3_1,CD7_4_1,CD7_5_1,CD7_6_1,CD7_7_1,CD7_8_1,CD7_9_1,CD7_10_1,CD7_11_1,CD7_12_1,
#            CD7_R_1,ps4_parents_jab_1,ps4_parents_jab2_1,Respondent_cooperation, Elig,NordKivu, Kinshasa,  A18to39, A40to64, to_be_deferred, ps7_months_1, ps7_months2_1,
#            hd5_a0_1, hd5_a_1, hd7a_0_1, hd7a_1, agegp, E11_copy, E11, E10, E9_1, e14_b, e14_b_copy, dadageatbirth_dead_1,momageatbirth_dead_1,dadageatbirth_alive_1,
#            momageatbirth_alive_1, mistake_momdead_1, mistake_momalive_1, mistake_daddead_1, mistake_dadalive_1, corr_respagemomdead_1, corr_respagedaddead_1,
#            corr_momyeardead_1,corr_momagedead_1,corr_dadagedead_1,corr_dadyeardead_1, corr_momagealive_1, corr_respagemomalive_1,corr_dadagealive_1, corr_respagedadalive_1,
#            call_feedback,call_feedback_1,call_feedback_2,call_feedback_3,call_feedback_4,call_feedback_5, phone_call_duration,deviceid, formdef_version, KEY, phone.call.log,caseid_original  ))


# if (Sys.info()['sysname'] == 'Darwin') {
#       ### For mac
#       data_clean$Date.Interview <- gsub('.{6}$', '', data_clean$instance_time)
#       data_clean$Date.Interview <- as.Date(as.date(data_clean$Date.Interview, order = "mdy"))
#       
#       data_clean2 <- data_clean %>%
#         mutate(Resp.Age.grp = cut(Resp.Age, c(17,40,100)),
#                Resp.Age.pyr = cut(Resp.Age, c(14,19,29,39,49,59,64))
#         )
#     } else if (Sys.info()['sysname'] == 'Windows') {
#       # For windows
#       data_clean$Date.Interview <- gsub('.{6}$', '', data_clean$instance_time)
#       
#       data_clean2 <- data_clean %>%
#         mutate(Date.Interview1 = as.POSIXct(Date.Interview, format="%h %d, %Y"),
#                Date.Interview = format(Date.Interview1, "%Y-%m-%d")
#         ) %>%
#         dplyr::select(-Date.Interview1)
#     }
#   
  
  

#data_clean2$Gr.Interviewer <- if_else(data_clean2$enumerator=='E1'|data_clean2$enumerator=='E2'|data_clean2$enumerator=='E3'|data_clean2$enumerator=='E4'|
 #                                       data_clean2$enumerator=='E5'|data_clean2$enumerator=='E6'|data_clean2$enumerator=='E7'|data_clean2$enumerator=='E8',"Original", "New")


# ---- Get a clean set of ultimate outcomes for the data in LONG FORMAT ie attempts
data_clean3 <- data_clean %>%
  dplyr::mutate(Outcome2 = ifelse(call_status == '7', 'NNU', # used to be DYS
                                  ifelse(call_status == "1" & Resp_Consent == 1, 'COMP',
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
              
data_clean3$Outcome2[which(data_clean3$call_status ==1 & is.na(data_clean3$Resp_Consent))] <- 'INCO'

# Reassignment of call outcomes
# data_clean3$Outcome2[which(!is.na(data_clean3$elig) & 
#                              data_clean3$Outcome2 =="INEL" & 
#                              data_clean3$Resp_Consent ==1 & 
#                           (data_clean3$d1 ==1 | data_clean3$d1 ==0))] <- "COMP"

data_clean3$Outcome2[which(!is.na(data_clean3$elig) & 
                          data_clean3$Outcome2 =="INEL" & 
                          data_clean3$Resp_Consent ==1 & 
                          (data_clean3$d1 ==3))] <- "REFU"

data_clean3$Outcome2[which(!is.na(data_clean3$elig) & 
                             data_clean3$Outcome2 =="INEL" & 
                          is.na(data_clean3$Resp_Consent) & 
                          is.na(data_clean3$d1))] <- "INCO"

# Reassign incomplete cases witha refusal for the initial language question to refused
data_clean3$Outcome2[which(data_clean3$Outcome2=="INCO" & data_clean3$Resp_Language==888)] <- "REFU"

# Reassign Truly ineligible cases which have not been marked as such appropriately  
data_clean3$Outcome2[which((data_clean3$Outcome2=="REFU"| 
                              data_clean3$Outcome2=="INCO"|
                              data_clean3$Outcome2=="Other") &
                          !is.na(data_clean3$e10))] <-"INEL"


# merging with ivr numbers to ID them
data_clean3test <- left_join(data_clean3, ivr, by = 'phone_1') 
# merging with both sets of feroxus numbers to ID them 
testing123 <- left_join(data_clean3test, feroxus, by = 'phone_1') %>%
  dplyr::mutate(fer = ferox) %>% distinct()

 data_clean3 <- testing123 %>% 
   dplyr::mutate(Source_number = ifelse(source == 'IVR group 1' & is.na(fer),'IVR group 1',
                              ifelse(source == 'IVR group 2' & is.na(fer),'IVR group 2',
                                     ifelse(is.na(source) & (fer == 'Kin' | fer =='NK'), 'Feroxus', 'Feroxus'))),
          Source_prov = ifelse(fer == 'Kin' & is.na(Province), 'Feroxus-Kin',
                               ifelse(fer == 'NK'& is.na(Province), 'Feroxus-NK',
                                      ifelse(Province == 'Kinshasa'&is.na(fer),'IVR group 1-Kin',
                                             ifelse(Province == 'Nord Kivu'&is.na(fer), "IVR group 1-NK", 'IVR group 2'))))) %>%
   dplyr::mutate(Source_prov = ifelse(!is.na(Source_prov), Source_prov,'IVR group 2'))

 # de-duplication of the data 
  # Removed this part below - could be that we do need it, but i don't see how it is helping
 data <- rbind(
   data_clean3 %>%
   dplyr::filter(call_status==1) %>%
   distinct(caseid, .keep_all = TRUE) %>%
   dplyr::mutate(Outcome2 = ifelse(Resp_Region == 3, 'INEL',
                                   ifelse(is.na(Resp_Consent),'INCO', Outcome2)))%>%
   filter(call_num %notin% c('0','6')),
   data_clean3 %>%
   dplyr::filter(!(call_status== 1)))

 
# Limit to data cases and those who consent/those who collect in DRC
 Consented <- data[which(data$compandcons==1),]

###### Assignment of Roofing materials to defined groups

# Reallocate roof materials appropriately 
Consented$roofing_material[grepl("bois", Consented$roothing_other, ignore.case=TRUE) ] <- 5
Consented$roothing_other[grepl("bois", Consented$roothing_other, ignore.case=TRUE) ] <- NA

Consented$roofing_material[grepl("planche", Consented$roothing_other, ignore.case=TRUE) ] <- 5
Consented$roothing_other[grepl("planche", Consented$roothing_other, ignore.case=TRUE) ] <- NA

Consented$roofing_material[grepl("beton", Consented$roothing_other, ignore.case=TRUE) ] <- 7
Consented$roothing_other[grepl("beton", Consented$roothing_other, ignore.case=TRUE) ] <- NA

Consented$roofing_material[grepl("paille", Consented$roothing_other, ignore.case=TRUE) ] <- 2
Consented$roothing_other[grepl("paille", Consented$roothing_other, ignore.case=TRUE) ] <- NA

Consented$roofing_material[grepl("chaume", Consented$roothing_other, ignore.case=TRUE) ] <- 2
Consented$roothing_other[grepl("chaume", Consented$roothing_other, ignore.case=TRUE) ] <- NA

######
# Assignment of water sources to predefined groups
# send the other into another predefined group and remove it from the list of 'other'

# Reallocate roof materials appropriately 
Consented$water_source[grepl("CANNAU", Consented$other_watersource, ignore.case=TRUE) ] <- 14
Consented$other_watersource[grepl("CANNAU", Consented$other_watersource, ignore.case=TRUE) ] <- NA

Consented$water_source[grepl("RIVIERE", Consented$other_watersource, ignore.case=TRUE) ] <- 14
Consented$other_watersource[grepl("RIVIERE", Consented$other_watersource, ignore.case=TRUE) ] <- NA

Consented$water_source[grepl("Lac", Consented$other_watersource, ignore.case=TRUE) ] <- 14
Consented$other_watersource[grepl("Lac", Consented$other_watersource, ignore.case=TRUE) ] <- NA

Consented$water_source[grepl("FONTAINE", Consented$other_watersource, ignore.case=TRUE) ] <- 3
Consented$other_watersource[grepl("FONTAINE", Consented$other_watersource, ignore.case=TRUE) ] <- NA

# ===== Location of call

# --- identify words related to University or course
Consented$call_location[grepl("univer", Consented$specify_location, ignore.case=TRUE) |
                          grepl("unikin", Consented$specify_location, ignore.case=TRUE) |
                          grepl("classe", Consented$specify_location, ignore.case=TRUE)|
                          grepl("cours", Consented$specify_location, ignore.case=TRUE)|
                          grepl("stage", Consented$specify_location, ignore.case=TRUE)] <- 2

# --- Remove from the 'other'variable
Consented$specify_location[grepl("univer", Consented$specify_location, ignore.case=TRUE) |
                             grepl("unikin", Consented$specify_location, ignore.case=TRUE) |
                             grepl("classe", Consented$specify_location, ignore.case=TRUE) |
                             grepl("cours", Consented$specify_location, ignore.case=TRUE) |
                             grepl("stage", Consented$specify_location, ignore.case=TRUE) ] <- NA

# --- identify words suggesting out and about 
# Consented$call_location[grepl("route", Consented$specify_location, ignore.case=TRUE) |
#                           grepl("salon", Consented$specify_location, ignore.case=TRUE) |
#                           grepl("bus", Consented$specify_location, ignore.case=TRUE) |
#                           grepl("studio", Consented$specify_location, ignore.case=TRUE) |
#                           grepl("chez", Consented$specify_location, ignore.case=TRUE) |
#                           grepl("marche", Consented$specify_location, ignore.case=TRUE)|
#                           grepl("champ", Consented$specify_location, ignore.case=TRUE)|
#                           grepl("centre de sante", Consented$specify_location, ignore.case=TRUE)|
#                           grepl("eglise", Consented$specify_location, ignore.case=TRUE)|
#                           grepl("Hôpital", Consented$specify_location, ignore.case=TRUE)|
#                           grepl("Hôtel", Consented$specify_location, ignore.case=TRUE)|
#                           grepl("Rue", Consented$specify_location, ignore.case=TRUE)|
#                           grepl("Shop", Consented$specify_location, ignore.case=TRUE)|
#                           grepl("AVENUE", Consented$specify_location, ignore.case=TRUE)|
#                           grepl("ville", Consented$specify_location, ignore.case=TRUE)|
#                           grepl("aeroport", Consented$specify_location, ignore.case=TRUE)|
#                           grepl("ville", Consented$specify_location, ignore.case=TRUE)|
#                           grepl("promenade", Consented$specify_location, ignore.case=TRUE)] <- "Out and about"

# --- remove words containing this from the 'specify location' variable
# Consented$specify_location[grepl("route", Consented$specify_location, ignore.case=TRUE) |
#                              grepl("salon", Consented$specify_location, ignore.case=TRUE) |
#                              grepl("bus", Consented$specify_location, ignore.case=TRUE) |
#                              grepl("studio", Consented$specify_location, ignore.case=TRUE) |
#                              grepl("chez", Consented$specify_location, ignore.case=TRUE) |
#                              grepl("marche", Consented$specify_location, ignore.case=TRUE)|
#                              grepl("champ", Consented$specify_location, ignore.case=TRUE)|
#                              grepl("centre de sante", Consented$specify_location, ignore.case=TRUE)|
#                              grepl("eglise", Consented$specify_location, ignore.case=TRUE)|
#                              grepl("Hôpital", Consented$specify_location, ignore.case=TRUE)|
#                              grepl("Hôtel", Consented$specify_location, ignore.case=TRUE)|
#                              grepl("Rue", Consented$specify_location, ignore.case=TRUE)|
#                              grepl("Shop", Consented$specify_location, ignore.case=TRUE)|
#                              grepl("AVENUE", Consented$specify_location, ignore.case=TRUE)|
#                              grepl("ville", Consented$specify_location, ignore.case=TRUE)|
#                              grepl("aeroport", Consented$specify_location, ignore.case=TRUE)|
#                              grepl("ville", Consented$specify_location, ignore.case=TRUE)|
#                              grepl("promenade", Consented$specify_location, ignore.case=TRUE)] <- NA

# Convert to wide dataset
dat1 <- data.frame(call_num=data$call_num,
                   Outcome=data$Outcome2,
                   caseid = data$caseid,
                   phone_1 = data$phone_1,
                   inel_age = data$e10,
                   lang = data$l2_1,
                   Source = data$Source_number,
                   Source_prov = data$Source_prov,
                   call_num_2 = data$call_num,
                   pull_label = data$pull_label,
                   pull_ivrprovince = data$pull_ivrprovince,
                   time = data$Date_Interview,
                   enum = data$enumerator, 
                   region = data$Resp_Region) #%>%
# dat1$num <- seq(1, nrow(dat1))
#   mutate(caseidphone=paste0(caseid,phone_1, call_num))
# dat1$group <- as.numeric(factor(dat1$caseidphone))


dat1 <- dat1 %>% 
  group_by(phone_1, call_num) 
dat1 <- dat1[!duplicated(dat1[c('phone_1','call_num','caseid')]),]
dat.wide <- pivot_wider(dat1, names_from = call_num, values_from = c('Outcome', 'inel_age', 'lang', 'Source','Source_prov','call_num_2',
                                                                     'pull_label','pull_ivrprovince','time', 'enum','region')) %>%
  #reshape(dat1, idvar = "caseid", timevar = "call_num", direction = "wide")%>%
  dplyr::filter(caseid %notin% c('12','51','57','56','54','58','53','52','59')) %>%
  # selects the last nonNA outcome for the respondent over the specified columns
  dplyr::mutate(latest = coalesce(Outcome_5, Outcome_4, Outcome_4, Outcome_3, Outcome_2, Outcome_1),
                inel_age = coalesce(inel_age_5, inel_age_4, inel_age_3, inel_age_2, inel_age_1),
                lang = coalesce(lang_5, lang_4, lang_3, lang_2, lang_1),
                call_num = coalesce(call_num_2_5, call_num_2_4, call_num_2_3, call_num_2_2, call_num_2_1)) %>%
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
                                             |!is.na(inel_age)) ~ "PART",
                                          #Outcome_1 == 'COMP' | Outcome_2 == 'COMP' |Outcome_3 == 'COMP' |Outcome_4 == 'COMP' |Outcome_5 == 'COMP'~'COMP',
                                          TRUE ~ "PEND")#,
                # --- Coalesce variables so we have complete data in the dat.wide data
                # Source.1 = coalesce(Source_1, Source_2, Source_3, Source_4, Source_5) ,
                # Source.prov.1 = coalesce(Source.prov_1, Source.prov_2, Source.prov_3, Source.prov_4, Source.prov_5),
                # pull_label = coalesce(pull_label_1, pull_label_2, pull_label_3, pull_label_4, pull_label_5),
                # pull_ivrprovince = coalesce(pull_ivrprovince_1, pull_ivrprovince_2, pull_ivrprovince_3, pull_ivrprovince_4, pull_ivrprovince_5),
                # enum = coalesce(enum_1, enum_2, enum_3, enum_4, enum_5),
                # time = coalesce(time_5, time_4, time_3, time_2, time_1
                                )


### INFORMATION ON COVID 19 VAX REASONS FOR TAKING/NOT
##############################

# Vaccine is not safe/causes harm/side effects (3)
Consented$vaccines_den_reasons_3[grepl("Le vaccin change les groupes sanguin", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("N A PAS ETE EXPERIMENTE AVANT DE", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("ne connaissons pas son origine", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("risque", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("effet", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("peur", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("confuse", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("tue", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("dangereux", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("DÃ‰CÃ‰DÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("dÃ©cÃ©dÃ©", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("INFECOND", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("abÃ®me", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("pas bien", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("mauvais", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("donne des allergies", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("allergique", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("COMORBIDITÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("risquant", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("coagule le sang", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("poison", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("asme", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("Asthmatique", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("JE TOMBE DIFFICILEMENT MALADE", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("J'ATTENDS QUE MON Ã‰TAT DE SANTÃ‰ PUISSE SE STABILISER", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("NTÃ‰ APRÃˆS PAR CE QUE JE SUIS EN TRÃˆS BONNE SANTÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("IL  Y'A  QUELQUE CHOSE  CACHÃ‰E  DERRIÃˆRE  LE VACCIN", Consented$other_den_reasons, ignore.case=TRUE)]<- 1
Consented$vaccines_den_reasons_13[grepl("Le vaccin change les groupes sanguin", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("N A PAS ETE EXPERIMENTE AVANT DE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("ne connaissons pas son origine", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("risque", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("effet", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("peur", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("confuse", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("tue", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("dangereux", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("DÃ‰CÃ‰DÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("dÃ©cÃ©dÃ©", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("INFECOND", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("abÃ®me", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("pas bien", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("mauvais", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("mauvais", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("donne des allergies", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("allergique", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("COMORBIDITÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("risquant", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("coagule le sang", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("poison", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("asme", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("Asthmatique", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("JE TOMBE DIFFICILEMENT MALADE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("J'ATTENDS QUE MON Ã‰TAT DE SANTÃ‰ PUISSE SE STABILISER", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("NTÃ‰ APRÃˆS PAR CE QUE JE SUIS EN TRÃˆS BONNE SANTÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("IL  Y'A  QUELQUE CHOSE  CACHÃ‰E  DERRIÃˆRE  LE VACCIN", Consented$other_den_reasons, ignore.case=TRUE)]<- 0
Consented$other_den_reasons[grepl("Le vaccin change les groupes sanguin", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("N A PAS ETE EXPERIMENTE AVANT DE", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("ne connaissons pas son origine", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("risque", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("effet", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("peur", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("confuse", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("tue", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("dangereux", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("DÃ‰CÃ‰DÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("dÃ©cÃ©dÃ©", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("INFECOND", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("abÃ®me", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("pas bien", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("mauvais", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("mauvais", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("donne des allergies", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("allergique", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("COMORBIDITÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("risquant", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("coagule le sang", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("poison", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("asme", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("Asthmatique", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("JE TOMBE DIFFICILEMENT MALADE", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("J'ATTENDS QUE MON Ã‰TAT DE SANTÃ‰ PUISSE SE STABILISER", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("NTÃ‰ APRÃˆS PAR CE QUE JE SUIS EN TRÃˆS BONNE SANTÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("IL  Y'A  QUELQUE CHOSE  CACHÃ‰E  DERRIÃˆRE  LE VACCIN", Consented$other_den_reasons, ignore.case=TRUE)] <- "REASS"

# There will be other effective treatments soon (11)
Consented$vaccines_den_reasons_11[which(Consented$other_den_reasons=="NOUVEAU VACCIN")]<- 1
Consented$vaccines_den_reasons_13[which(Consented$other_den_reasons=="NOUVEAU VACCIN")]<- 0
Consented$other_den_reasons[which(Consented$other_den_reasons=="NOUVEAU VACCIN")] <- "REASS"

# Chance of contracting COVID-19 is small (6)
Consented$vaccines_den_reasons_6[grepl("exist", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("expos", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("PAS COVID 19 EN RDC", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("ne connait pas si la  maladie", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("YA PAS ASSEZ DES CAS DE COVID", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("CROIS", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("CROIT", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("Ici chez nous il y a pas de covid", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("N' YA PAS DE COVID", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("ne se propage pas", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("PAS ATTRAPER", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("POUR LES BLAMCS", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("pas de cas de covid", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("personne n'a souffert de Ã‡A DANS MA VILLE", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("pas de covid ici", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("la maladie est deja termine", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("POUR LES BLANCS", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("noir", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("NE  ME  CONCERNE PAS", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("N'AI PAS BESOIN DU VACCIN", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("C EST POUR LES  CHINOIS", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("PAS DE COVID", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("PAS DE CORONA", Consented$other_den_reasons, ignore.case=TRUE)
                                   ]<- 1
Consented$vaccines_den_reasons_13[grepl("exist", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("expos", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PAS COVID 19 EN RDC", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("ne connait pas si la  maladie", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("YA PAS ASSEZ DES CAS DE COVID", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("CROIS", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("CROIT", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("Ici chez nous il y a pas de covid", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("N' YA PAS DE COVID", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("ne se propage pas", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PAS ATTRAPER", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("POUR LES BLAMCS", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("pas de cas de covid", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("personne n'a souffert de Ã‡A DANS MA VILLE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("pas de covid ici", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("la maladie est deja termine", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("POUR LES BLANCS", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("noir", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("NE  ME  CONCERNE PAS", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("N'AI PAS BESOIN DU VACCIN", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("C EST POUR LES  CHINOIS", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PAS DE COVID", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PAS DE CORONA", Consented$other_den_reasons, ignore.case=TRUE)]<- 0
Consented$other_den_reasons[grepl("exist", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("expos", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("PAS COVID 19 EN RDC", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("ne connait pas si la  maladie", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("YA PAS ASSEZ DES CAS DE COVID", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("CROIS", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("CROIT", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("Ici chez nous il y a pas de covid", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("N' YA PAS DE COVID", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("ne se propage pas", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("PAS ATTRAPER", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("POUR LES BLAMCS", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("pas de cas de covid", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("personne n'a souffert de Ã‡A DANS MA VILLE", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("pas de covid ici", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("la maladie est deja termine", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("POUR LES BLANCS", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("noir", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("NE  ME  CONCERNE PAS", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("N'AI PAS BESOIN DU VACCIN", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("C EST POUR LES  CHINOIS", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("PAS DE COVID", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("PAS DE CORONA", Consented$other_den_reasons, ignore.case=TRUE)]<- "REASS"


# COVID-19 is not serious or life threatening (for me) (7)
Consented$vaccines_den_reasons_7[grepl("important", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("néc", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("nec", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("n'est pas grave", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("je suis en bonne", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("je suis encore fort", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("pas d'importance", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("pas d importance", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("pas urgent", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("PAS  L IMPORTANCE", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("N'EST PAS DANS L'URGENCE", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("NE TROUVE PAS LA NÃ‰CESSITÃ‰ DE PRENDRE LE VACCIN", Consented$other_den_reasons, ignore.case=TRUE)]<- 1
Consented$vaccines_den_reasons_13[grepl("important", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("néc", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("nec", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("n'est pas grave", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("je suis en bonne", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("je suis encore fort", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("pas d'importance", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("pas d importance", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("pas urgent", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PAS  L IMPORTANCE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("N'EST PAS DANS L'URGENCE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("NE TROUVE PAS LA NÃ‰CESSITÃ‰ DE PRENDRE LE VACCIN", Consented$other_den_reasons, ignore.case=TRUE)]<- 0
Consented$other_den_reasons[grepl("important", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("néc", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("nec", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("n'est pas grave", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("je suis en bonne", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("je suis encore fort", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("pas d'importance", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("pas d importance", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("pas urgent", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("PAS  L IMPORTANCE", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("N'EST PAS DANS L'URGENCE", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("NE TROUVE PAS LA NÃ‰CESSITÃ‰ DE PRENDRE LE VACCIN", Consented$other_den_reasons, ignore.case=TRUE)]<- "REASS"

# Vaccine is not effective (2)
Consented$vaccines_den_reasons_2[grepl("effic", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("pas efocase", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("aucune utilit", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("pas utile", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("pas de garantie", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("garanti rien", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("guaranti rien", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("sont encore infect", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("attrape toujours", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("encore atteinte du covid", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("convai", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("prendre le vaccin ou pas il va mourir", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("CONTINUE  Ã€ TOMBER  MALADE", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("fiable", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("L'ENQUETÃ‰ N'AS PAS LA MOTIVATION DE PRENDRE LE VACCIN. 
                                         QU'IL LE PRENNE OÃ™ PAS ,IL SERA INFECTÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PAS PRÃ‰VENTIF", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PRENDRE LE VACCIN  OU PAS  RIEN NE  CHANGE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("POUVEZ TOUJOURS FAIRE UNE FORME GRAVE DE COVID", Consented$other_den_reasons, ignore.case=TRUE)]<- 1
Consented$vaccines_den_reasons_13[grepl("effic", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("pas efocase", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("aucune utilit", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("pas utile", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("pas de garantie", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("garanti rien", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("guaranti rien", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("sont encore infect", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("attrape toujours", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("encore atteinte du covid", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("convai", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("prendre le vaccin ou pas il va mourir", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("CONTINUE  Ã€ TOMBER  MALADE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("fiable", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("L'ENQUETÃ‰ N'AS PAS LA MOTIVATION DE PRENDRE LE VACCIN. 
                                         QU'IL LE PRENNE OÃ™ PAS ,IL SERA INFECTÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PAS PRÃ‰VENTIF", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PRENDRE LE VACCIN  OU PAS  RIEN NE  CHANGE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("POUVEZ TOUJOURS FAIRE UNE FORME GRAVE DE COVID", Consented$other_den_reasons, ignore.case=TRUE)]<- 0
Consented$other_den_reasons[grepl("effic", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("pas efocase", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("aucune utilit", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("pas utile", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("pas de garantie", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("garanti rien", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("guaranti rien", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("sont encore infect", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("attrape toujours", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("encore atteinte du covid", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("convai", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("prendre le vaccin ou pas il va mourir", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("CONTINUE  Ã€ TOMBER  MALADE", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("fiable", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("L'ENQUETÃ‰ N'AS PAS LA MOTIVATION DE PRENDRE LE VACCIN. 
                                         QU'IL LE PRENNE OÃ™ PAS ,IL SERA INFECTÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("PAS PRÃ‰VENTIF", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("PRENDRE LE VACCIN  OU PAS  RIEN NE  CHANGE", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("POUVEZ TOUJOURS FAIRE UNE FORME GRAVE DE COVID", Consented$other_den_reasons, ignore.case=TRUE)]<- "REASS"

# Already had COVID-19 and believe I am immune (1)
Consented$vaccines_den_reasons_1[grepl("immun", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("ANTICORPS", Consented$other_den_reasons, ignore.case=TRUE)]<- 1
Consented$vaccines_den_reasons_13[grepl("immun", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("ANTICORPS", Consented$other_den_reasons, ignore.case=TRUE)]<- 0
Consented$other_den_reasons[grepl("immun", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("ANTICORPS", Consented$other_den_reasons, ignore.case=TRUE)]<- "REASS"


# Religious objections/traditional beliefs (specify) (5)
Consented$vaccines_den_reasons_5[grepl("dieu", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("conviction religieuse", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("PRIÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("religion", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("DÃ‰MONIAQUE", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("SCIENCES OCCULTES", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("satanique", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("le signe de la fin du monde", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("religieu", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("tÃ©moin de jehovah", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("OCCULTISME", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("pasteur", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("chrÃ©tien", Consented$other_den_reasons, ignore.case=TRUE)]<- 1
Consented$vaccines_den_reasons_13[grepl("dieu", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("conviction religieuse", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PRIÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("religion", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("DÃ‰MONIAQUE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("SCIENCES OCCULTES", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("satanique", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("le signe de la fin du monde", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("religieu", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("tÃ©moin de jehovah", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("OCCULTISME", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("pasteur", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("chrÃ©tien", Consented$other_den_reasons, ignore.case=TRUE)]<- 0
Consented$other_den_reasons[grepl("dieu", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("conviction religieuse", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("PRIÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("religion", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("DÃ‰MONIAQUE", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("SCIENCES OCCULTES", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("satanique", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("le signe de la fin du monde", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("religieu", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("tÃ©moin de jehovah", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("OCCULTISME", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("pasteur", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("chrÃ©tien", Consented$other_den_reasons, ignore.case=TRUE)]<- "REASS"


# wait until others take the vaccine (10)
Consented$vaccines_den_reasons_10[grepl("attends que les autres prennent", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("attends que le pr", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("LORS QUE TOUS LES AUTORIT", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("NOUS OBSERVONS ENCORE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("donne d'abord au pr", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PARCE QUE LES GENS  N ONT  PAS  PRIS", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PRENNENT D ABORD", Consented$other_den_reasons, ignore.case=TRUE)]<- 1
Consented$vaccines_den_reasons_13[grepl("attends que les autres prennent", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("attends que le pr", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("LORS QUE TOUS LES AUTORIT", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("NOUS OBSERVONS ENCORE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("donne d'abord au pr", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PARCE QUE LES GENS  N ONT  PAS  PRIS", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("PRENNENT D ABORD", Consented$other_den_reasons, ignore.case=TRUE)]<- 0
Consented$other_den_reasons[grepl("attends que les autres prennent", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("attends que le pr", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("LORS QUE TOUS LES AUTORIT", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("NOUS OBSERVONS ENCORE", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("donne d'abord au pr", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("PARCE QUE LES GENS  N ONT  PAS  PRIS", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("PRENNENT D ABORD", Consented$other_den_reasons, ignore.case=TRUE)]<- "REASS"

# May get covid from the vaccine (4)
Consented$vaccines_den_reasons_4[grepl("provoque la maladie", Consented$other_den_reasons, ignore.case=TRUE)]<- 1
Consented$vaccines_den_reasons_13[grepl("provoque la maladie", Consented$other_den_reasons, ignore.case=TRUE)]<- 0
Consented$other_den_reasons[grepl("provoque la maladie", Consented$other_den_reasons, ignore.case=TRUE)]<- "REASS"

# need more information/medical advice (17)
Consented$vaccines_den_reasons_17[grepl("L AVIS  DE MON  MÃ‰DECIN", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("ENCORE EN PÃ‰RIODE D'ESSAI", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("LA PHASE EXPÃ‰RIMENTALE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("SAVOIR  LES  CONSÃ‰QUENCES", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("Attendre l' avis de son medecin", Consented$other_den_reasons, ignore.case=TRUE)]<- 1
Consented$vaccines_den_reasons_13[grepl("L AVIS  DE MON  MÃ‰DECIN", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("ENCORE EN PÃ‰RIODE D'ESSAI", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("LA PHASE EXPÃ‰RIMENTALE", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("SAVOIR  LES  CONSÃ‰QUENCES", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("Attendre l' avis de son medecin", Consented$other_den_reasons, ignore.case=TRUE)]<- 0
Consented$other_den_reasons[grepl("L AVIS  DE MON  MÃ‰DECIN", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("ENCORE EN PÃ‰RIODE D'ESSAI", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("LA PHASE EXPÃ‰RIMENTALE", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("SAVOIR  LES  CONSÃ‰QUENCES", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("Attendre l' avis de son medecin", Consented$other_den_reasons, ignore.case=TRUE)]<- "REASS"

# Consent of some sort (19)
Consented$vaccines_den_reasons_19[grepl("parent", Consented$other_den_reasons, ignore.case=TRUE) |
                                   grepl("maman", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("autorisation de son mari", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("M'ENTRETENIR D'ABORD AVEC MA FEMME", Consented$other_den_reasons, ignore.case=TRUE)|
                                   grepl("autorisation maritale", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("consentement", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("MA TÃŠTE  N A  PAS  ENCORE  CONSENTI", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("SA FEMME A REFUSÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("Avis de sa femme", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("CES ENFANTS NE SONT PAS D'ACCORD", Consented$other_den_reasons, ignore.case=TRUE)]<- 1
Consented$vaccines_den_reasons_13[grepl("parent", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("maman", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("autorisation de son mari", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("M'ENTRETENIR D'ABORD AVEC MA FEMME", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("autorisation maritale", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("consentement", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("MA TÃŠTE  N A  PAS  ENCORE  CONSENTI", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("SA FEMME A REFUSÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("Avis de sa femme", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("CES ENFANTS NE SONT PAS D'ACCORD", Consented$other_den_reasons, ignore.case=TRUE)]<- 0
Consented$other_den_reasons[grepl("parent", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("maman", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("autorisation de son mari", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("M'ENTRETENIR D'ABORD AVEC MA FEMME", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("autorisation maritale", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("consentement", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("MA TÃŠTE  N A  PAS  ENCORE  CONSENTI", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("SA FEMME A REFUSÃ‰", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("Avis de sa femme", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("CES ENFANTS NE SONT PAS D'ACCORD", Consented$other_den_reasons, ignore.case=TRUE)]<- "REASS"

# Protect myself in other ways (20)
Consented$vaccines_den_reasons_20[grepl("GESTES BARRIÃˆRES", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("GESTES BARRIeRES", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("son vaccin", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("tisane", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("bain de soleil", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("utilise les plantes", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("traitement traditionnel", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("sais  comment  me protÃ©ger", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("SE PROTÃˆGE DÃ‰JÃ€", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("IL PREND DÃ‰JÃ€ SOINS DE LUI", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("SE PROTÃ‰GE DÃ‰JÃ€", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("les herbes mÃ©dicinales", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("traitements traditionnels", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("les herbes  medecinales", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("son propre vaccin", Consented$other_den_reasons, ignore.case=TRUE)#|
                                   # grepl("JE PRENDS DÃ‰JÃ€ LES MANA COVID (MÃ‰DICAMENT CONTRE LA COVID", Consented$other_den_reasons, ignore.case=TRUE)
                                  ]<- 1
Consented$vaccines_den_reasons_13[grepl("GESTES BARRIÃˆRES", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("GESTES BARRIeRES", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("son vaccin", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("tisane", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("bain de soleil", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("utilise les plantes", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("traitement traditionnel", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("sais  comment  me protÃ©ger", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("SE PROTÃˆGE DÃ‰JÃ€", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("IL PREND DÃ‰JÃ€ SOINS DE LUI", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("SE PROTÃ‰GE DÃ‰JÃ€", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("les herbes mÃ©dicinales", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("traitements traditionnels", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("les herbes  medecinales", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("son propre vaccin", Consented$other_den_reasons, ignore.case=TRUE)#|
                                    #grepl("JE PRENDS DÃ‰JÃ€ LES MANA COVID (MÃ‰DICAMENT CONTRE LA COVID", Consented$other_den_reasons, ignore.case=TRUE)
                                  ]<- 0
Consented$other_den_reasons[grepl("GESTES BARRIÃˆRES", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("GESTES BARRIeRES", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("son vaccin", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("tisane", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("bain de soleil", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("utilise les plantes", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("traitement traditionnel", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("sais  comment  me protÃ©ger", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("SE PROTÃˆGE DÃ‰JÃ€", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("IL PREND DÃ‰JÃ€ SOINS DE LUI", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("SE PROTÃ‰GE DÃ‰JÃ€", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("les herbes mÃ©dicinales", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("traitements traditionnels", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("les herbes  medecinales", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("son propre vaccin", Consented$other_den_reasons, ignore.case=TRUE)#|
                              #grepl("JE PRENDS DÃ‰JÃ€ LES MANA COVID (MÃ‰DICAMENT CONTRE LA COVID", Consented$other_den_reasons, ignore.case=TRUE)
                            ]<- "REASS"


# --- Additional variables
# Add new variables - rumour, Info, Pointless, pregnant, no confidence
Consented$vaccines_den_reasons_17[grepl("sur", Consented$other_den_reasons, ignore.case=TRUE)|
                                      grepl("sûr", Consented$other_den_reasons, ignore.case=TRUE)|
                                      grepl("info", Consented$other_den_reasons, ignore.case=TRUE)|
                                      grepl("explication", Consented$other_den_reasons, ignore.case=TRUE)|
                                      grepl("sensibilis", Consented$other_den_reasons, ignore.case=TRUE)|
                                      grepl("LA sensation ne pas bien fait", Consented$other_den_reasons, ignore.case=TRUE)] <- 1

Consented$vaccines_den_reasons_18[grepl("ENCEINTE", Consented$other_den_reasons, ignore.case=TRUE)]<- 1

Consented$vaccines_den_reasons_13[grepl("sur", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("sûr", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("info", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("explication", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("sensibilis", Consented$other_den_reasons, ignore.case=TRUE)|
                                    grepl("LA sensation ne pas bien fait", Consented$other_den_reasons, ignore.case=TRUE)]<- 0

Consented$vaccines_den_reasons_13[grepl("ENCEINTE", Consented$other_den_reasons, ignore.case=TRUE)]<- 0

Consented$other_den_reasons[grepl("sur", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("sûr", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("info", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("explication", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("sensibilis", Consented$other_den_reasons, ignore.case=TRUE)|
                              grepl("LA sensation ne pas bien fait", Consented$other_den_reasons, ignore.case=TRUE)]<- "REASS"

Consented$other_den_reasons[grepl("ENCEINTE", Consented$other_den_reasons, ignore.case=TRUE)]<- "REASS"

############################################################
############################################################
############################################################
#REASONS THEY WOULD OR DID TAKE THE VACCINE

# * new category - to protect myself
Consented$vaccines_yes_reasons_15[grepl("POUR SE PROT", Consented$other_yes_reasons, ignore.case=TRUE)|
                                    grepl("ME PROT", Consented$other_yes_reasons, ignore.case=TRUE)] <- 1
Consented$vaccines_yes_reasons_9[grepl("POUR SE PROT", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("ME PROT", Consented$other_yes_reasons, ignore.case=TRUE)]<- 0
Consented$other_yes_reasons[grepl("POUR SE PROT", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("ME PROT", Consented$other_yes_reasons, ignore.case=TRUE)]<- "REASS"

# To protect against Against reinfection
Consented$vaccines_yes_reasons_1[grepl("POUR AVOIR UNE IMMUNITÉ", Consented$other_yes_reasons, ignore.case=TRUE)] <- 1
Consented$vaccines_yes_reasons_9[grepl("POUR AVOIR UNE IMMUNITÉ", Consented$other_yes_reasons, ignore.case=TRUE)]<- 0
Consented$other_yes_reasons[grepl("POUR AVOIR UNE IMMUNITÉ", Consented$other_yes_reasons, ignore.case=TRUE)]<- "REASS"


# Believe it's my responsibility
Consented$vaccines_yes_reasons_5[grepl("Pcq c est une obligation", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Pour le service", Consented$other_yes_reasons, ignore.case=TRUE)] <- 1
Consented$vaccines_yes_reasons_9[grepl("Pcq c est une obligation", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Pour le service", Consented$other_yes_reasons, ignore.case=TRUE)]<- 0
Consented$other_yes_reasons[grepl("Pcq c est une obligation", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Pour le service", Consented$other_yes_reasons, ignore.case=TRUE)]<- "REASS"


# For travel
Consented$vaccines_yes_reasons_13[grepl("voyage", Consented$other_yes_reasons, ignore.case=TRUE)
                                  #  |grepl("travai", Consented$other_yes_reasons, ignore.case=TRUE)
                                    ] <- 1
Consented$vaccines_yes_reasons_9[grepl("voyage", Consented$other_yes_reasons, ignore.case=TRUE)
                                 #|grepl("travai", Consented$other_yes_reasons, ignore.case=TRUE)
                                 ]<- 0
Consented$other_yes_reasons[grepl("voyage", Consented$other_yes_reasons, ignore.case=TRUE)
                           # |grepl("travai", Consented$other_yes_reasons, ignore.case=TRUE)
                            ]<- "REASS"

# Misassigned -  create separate category of likelihood (category 6 which is part of unlikely)
Consented$vaccine_likelihood[grepl("Pour le moment c est pas important", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Ce n'est pas  important", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("JE  SUIS  PAS INFORME  POUR LES VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Pas confiance", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("PAS DES INFORMATIONS POUR  LES VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Pas de vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Des mauvais rumeurs contre les vaccins", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("IL YA BEAUCOUP DES RUMEURS  SUR LE VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("PAS DES INFORMATIONS  SUR LES VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("JE N AI PAS CONFIANCE AU VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("LE COVID N EXISTE  PAS", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("LE COVID 19N EXISTE PAS", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("PARCE QUE C PAYANT", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Doute sue l efficacite du vaccin vaccin n est pas fiable", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Pas convaincu  sur le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Ne crois pas en l existence de la maladie", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Elle a besoin d'une explication", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Pas  trop sure avec le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Il ne pas pret", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("N  a pas d information sur le vacci", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("EMPOISONNE", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Le vaccin cause la mort", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Nous voulons  beaucoup  d explication sur", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("PEUR DES EFFETS SECONDAIRES", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("PEUR DE EFFETS SECONDAIRES", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("JE NE CROIS PAS À L EXISTENCE DE LA MALADIE", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("N'est pas sur du vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
                               grepl("Le vaccin est un juste un montage du gouvernement la maladie n existe pas", Consented$other_yes_reasons, ignore.case=TRUE)] <- 6

Consented$vaccines_yes_reasons_9[grepl("Pour le moment c est pas important", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Ce n'est pas  important", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("JE  SUIS  PAS INFORME  POUR LES VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Pas confiance", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("PAS DES INFORMATIONS POUR  LES VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Pas de vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Des mauvais rumeurs contre les vaccins", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("IL YA BEAUCOUP DES RUMEURS  SUR LE VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("PAS DES INFORMATIONS  SUR LES VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("JE N AI PAS CONFIANCE AU VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("LE COVID N EXISTE  PAS", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("LE COVID 19N EXISTE PAS", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("PARCE QUE C PAYANT", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Doute sue l efficacite du vaccin vaccin n est pas fiable", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Pas convaincu  sur le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Ne crois pas en l existence de la maladie", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Elle a besoin d'une explication", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Pas  trop sure avec le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Il ne pas pret", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("N  a pas d information sur le vacci", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("EMPOISONNE", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Le vaccin cause la mort", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Nous voulons  beaucoup  d explication sur", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("PEUR DES EFFETS SECONDAIRES", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("PEUR DE EFFETS SECONDAIRES", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("JE NE CROIS PAS À L EXISTENCE DE LA MALADIE", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("N'est pas sur du vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
                                   grepl("Le vaccin est un juste un montage du gouvernement la maladie n existe pas", Consented$other_yes_reasons, ignore.case=TRUE)] <- 0

Consented$other_yes_reasons[grepl("Pour le moment c est pas important", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Ce n'est pas  important", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("JE  SUIS  PAS INFORME  POUR LES VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Pas confiance", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("PAS DES INFORMATIONS POUR  LES VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Pas de vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Des mauvais rumeurs contre les vaccins", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("IL YA BEAUCOUP DES RUMEURS  SUR LE VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("PAS DES INFORMATIONS  SUR LES VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("JE N AI PAS CONFIANCE AU VACCIN", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("LE COVID N EXISTE  PAS", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("LE COVID 19N EXISTE PAS", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("PARCE QUE C PAYANT", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Doute sue l efficacite du vaccin vaccin n est pas fiable", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Pas convaincu  sur le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Ne crois pas en l existence de la maladie", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Elle a besoin d'une explication", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Pas  trop sure avec le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Il ne pas pret", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("N  a pas d information sur le vacci", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("EMPOISONNE", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Le vaccin cause la mort", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Nous voulons  beaucoup  d explication sur", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("PEUR DES EFFETS SECONDAIRES", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("PEUR DE EFFETS SECONDAIRES", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("JE NE CROIS PAS À L EXISTENCE DE LA MALADIE", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("N'est pas sur du vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
                              grepl("Le vaccin est un juste un montage du gouvernement la maladie n existe pas", Consented$other_yes_reasons, ignore.case=TRUE)] <- "REASS"

# BC good info exists on the vaccine
# Consented$vaccines_yes_reasons_Good_info[grepl("Avoir de bonnes INFORMATION", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                            grepl("Avoir des bonnes information sur le", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                            grepl("Avoir les explications claires sur le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                            grepl("BIEN EXPLIQUÉ POUR PRADRE", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                            grepl("Avoir des informations claires sur le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                            grepl("Explications pour le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                            grepl("Avoir les information sûre sur  le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                            grepl("Explications pour le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)] <- 1
# 
# Consented$vaccines_yes_reasons_9[grepl("Avoir de bonnes INFORMATION", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                    grepl("Avoir des bonnes information sur le", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                    grepl("Avoir les explications claires sur le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                    grepl("BIEN EXPLIQUÉ POUR PRADRE", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                    grepl("Avoir des informations claires sur le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                    grepl("Explications pour le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                    grepl("Avoir les information sûre sur  le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
#                                    grepl("Explications pour le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)]<- 0
# 
# Consented$other_yes_reasons[grepl("Avoir de bonnes INFORMATION", Consented$other_yes_reasons, ignore.case=TRUE)|
#                               grepl("Avoir des bonnes information sur le", Consented$other_yes_reasons, ignore.case=TRUE)|
#                               grepl("Avoir les explications claires sur le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
#                               grepl("BIEN EXPLIQUÉ POUR PRADRE", Consented$other_yes_reasons, ignore.case=TRUE)|
#                               grepl("Avoir des informations claires sur le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
#                               grepl("Explications pour le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
#                               grepl("Avoir les information sûre sur  le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)|
#                               grepl("Explications pour le vaccin", Consented$other_yes_reasons, ignore.case=TRUE)]<- "REASS"
# Dataset to share
dfshare <- data %>%
  select(-c(phone_1, pull_label, pull_ivrprovince))
# dat.widepenny <- dat.wide %>%
#   naniar::replace_with_na_all(condition = ~.x == 'R')
##########
# # Output .csvs of Consented, dedup long , and dat.wide
#dir.output <- paste0(Sys.getenv('USERPROFILE'),"/London School of Hygiene and Tropical Medicine/RAMMPS_LSHTM_Group - Documents/General/Partners/UNIKIN/SurveyCTO Audits/Clean data/")

# write.csv(Complete, paste0(dir.output, "Complete",Sys.Date(),".csv"), row.names = F)
# # 
# data <- data[which(!is.na(data$devicephonenum)),]
# 
# write.csv(data, paste0(dir.output, "data",Sys.Date(),".csv"))


return(list(Consented, data))
} #end of function

# # Data for Penny
# penny1 <- data_essential3[,-c(2:9, 11:14, 18:2570)]
# penny2 <- penny1[,c(2:5,11, 84, 87,88,89,91,138,140:148,218,241:256,259:291,294:416,999:1310)]
# penny3 <- penny2[,c(1:193,460:505)]
# names(penny3)


#write.csv(penny3, paste0("C:/Users/KellyMcCain/London School of Hygiene and Tropical Medicine/Data for Penny Webster/Submissions_",Sys.Date(),".csv"), row.names = FALSE)

