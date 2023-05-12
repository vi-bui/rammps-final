clean_bf <- function(){
  # Cleaning script for Burkina data just for RDD
  library(haven)
  library(readstata13)
  # dir.input <- paste0(Sys.getenv('USERPROFILE'),"/OneDrive - London School of Hygiene and Tropical Medicine/Documents/General/Partners/Burkina/Survey Data/")
  dir.input <- ('/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/Burkina/Survey Data/')
  
  
  rddt1 <- read_dta(paste0(dir.input, 'base_appel_rdd_wide_T1.dta')) %>%
    select(c(phone_1,	nbr_appel,	dernier_appel,	response_1,	last_sta,	instance_time, response_other_1, phone_call_duration, phone_call_log, users,
             nomregion, e2, e3, b3, b4, cv1, e6_1, caseid, e8, e10))%>% # region, gender, age, education, marital status 
    mutate(e6_1 = case_when(e6_1 == 1 ~ 'Urban',
                            e6_1 == 2 ~ 'Urban',
                            e6_1 == 3 ~ 'Urban',
                            e6_1 == 4 ~ 'Rural'))
  
  colnames(rddt1)[17] <- "e6"
  
  
  rddt2 <- read_dta(paste0(dir.input, 'base_appel_rdd_wide_T2.dta'))%>%
    select(c(phone_1,	nbr_appel,	dernier_appel,	response_1,	last_sta,	instance_time, response_other_1, phone_call_duration, phone_call_log, users,
             nomregion, e2, e3, b3, b4, cv1, e6, caseid, e8, e10)) %>%
    mutate(e6 = case_when(e6 == 3 ~ 'Urban',
                          e6 == 4 ~ 'Rural'))
  
  
  rddt3 <- read_dta(paste0(dir.input, 'base_appel_rdd_wide_T3.dta'))%>%
    select(c(phone_1,	nbr_appel,	dernier_appel,	response_1,	last_sta,	instance_time, response_other_1, phone_call_duration, phone_call_log, users,
             nomregion, e2, e3, b3, b4, cv1, e6, caseid, e8, e10))%>%
    mutate(e6 = case_when(e6 == 3 ~ 'Urban',
                          e6 == 4 ~ 'Rural'))
  
  rddt4 <- read_dta(paste0(dir.input, 'base_appel_rdd_wide_T4.dta'))%>%
    select(c(phone_1,	nbr_appel,	dernier_appel,	response_1,	last_sta,	instance_time, response_other_1, phone_call_duration, phone_call_log, users,
             nomregion, e2, e3, b3, b4, cv1, e6, caseid, e8, e10))%>%
    mutate(e6 = case_when(e6 == 3 ~ 'Urban',
                          e6 == 4 ~ 'Rural'))
  
  # Combine all rdd
  rdd <- rbind(rddt1, rddt2, rddt3, rddt4) %>%
    mutate(users = case_when(str_detect(users, 'adjaratou') ~ 'adjaratou',
                             str_detect(users, 'lydia') ~ 'lydia',
                             str_detect(users, 'aminata') ~ 'aminata',
                             str_detect(users, 'angelique') ~ 'angelique',
                             str_detect(users, 'benedicte') ~ 'benedicte',
                             str_detect(users, 'yvette') ~ 'yvette',
                             str_detect(users, 'mariatou') ~ 'mariatou',
                             str_detect(users, 'nadege') ~ 'nadege',
                             str_detect(users, 'nina') ~ 'nina',
                             str_detect(users, 'balkissa') ~ 'balkissa',
                             str_detect(users, 'farida') ~ 'farida',
                             str_detect(users, 'martinienne') ~ 'martinienne',
                             str_detect(users, 'sali') ~ 'sali',
                             str_detect(users, 'habibou') ~ 'habibou',
                             str_detect(users, 'jacqueline') ~ 'jacqueline',
                             str_detect(users, 'zahissa') ~ 'zahissa',
                             str_detect(users, 'bane') ~ 'bane',
                             str_detect(users, 'abibata') ~ 'abibata',
                             str_detect(users, 'maimouna') ~ 'maimouna',
                             str_detect(users, 'kadidjatou')|str_detect(users, 'kady') ~ 'kadidjatou',
                             TRUE ~ 'Unknown'),
           enumerator = case_when(users ==  'adjaratou' ~ 'E1',
                                  users ==  'lydia' ~ 'E2',
                                  users ==  'aminata' ~ 'E3',
                                  users ==  'angelique' ~ 'E4',
                                  users ==  'benedicte' ~ 'E5',
                                  users ==  'yvette' ~ 'E6',
                                  users ==  'mariatou' ~ 'E7',
                                  users ==  'nadege' ~ 'E8',
                                  users ==  'nina' ~ 'E9',
                                  users ==  'balkissa' ~ 'E10',
                                  users ==  'farida' ~ 'E11',
                                  users ==  'martinienne' ~ 'E12',
                                  users ==  'sali' ~ 'E13',
                                  users ==  'habibou' ~ 'E14',
                                  users ==  'jacqueline' ~ 'E15',
                                  users ==  'zahissa' ~ 'E16',
                                  users ==  'bane' ~ 'E17',
                                  users ==  'abibata' ~ 'E18',
                                  users ==  'maimouna' ~ 'E19',
                                  users ==  'kadidjatou' ~ 'E20')) %>%
    dplyr::rename(region = nomregion, 
                  gender = e2, 
                  age = e3, 
                  education = b3, 
                  marital = b4,
                  vax = cv1,
                  urban_rural = e6)
  
  
  list_bf <- list(rdd)
  
  list_bf2 <- list_bf %>% map(
    ~ .x %>%
      mutate(response = case_when(response_1 == 1 ~ 'Décroche même langue',
                                  response_1 == 2 ~ 'Répondu, mais pas le répondant ideal', #
                                  response_1 == 3 ~ 'Ne décroche pas', #NNA
                                  response_1 == 4 ~ 'Le numéro ne passe pas', #NNU
                                  response_1 == 5 ~ 'Décroche mais pas même langue (ne se comprenne pas)', #lang
                                  response_1 == 6 ~ 'Autres')) %>% 
      mutate(last_status = case_when(last_sta == 'Complété'| last_sta =='1' ~ 'COMP',
                                     last_sta == 'Déjà enquêté'| last_sta == '2'~ 'Other', #already interviewed so no longer eligible 
                                     last_sta == 'Partiellement rempli' | last_sta =='4'~ 'INCO',
                                     last_sta == 'Refus'| last_sta =='3' ~ "REFU",
                                     last_sta == 'Rendez-vous'| last_sta =='5' ~ 'INCO', # rdv for later 
                                     last_sta == 'Transferé'| last_sta =='6'  ~ 'REFER',
                                     last_sta == 'Transfert' & response == 'Décroche mais pas même langue (ne se comprenne pas)' ~ 'REASS',
                                     last_sta == 'Transfert trimestriel' ~ 'DEFER',
                                     last_sta == 'Autre (préciser)'|last_sta == 'Autres'| last_sta =='96' ~ 'Other')) %>%
      mutate(Outcome = ifelse(response == 'Ne décroche pas', 'NNA or NR',
                              ifelse(response == 'Décroche mais pas même langue (ne se comprenne pas)' & last_sta != 'Transfert', 'LANG',
                                     ifelse(response == 'Le numéro ne passe pas', 'NNU',
                                            ifelse(response == 'Autres', 'Other',
                                                   ifelse(response == 'Répondu, mais pas le répondant ideal' & is.na(last_status), 'INCOR',
                                                          last_status))))),
             Outcome = ifelse(is.na(Outcome), 'Missing',Outcome)) %>%
      filter(Outcome!= 'Missing') %>%
      mutate(Date.Interview1 = gsub('.{9}$', '', instance_time),
             Date.Interview = as.POSIXct(Date.Interview1, format="%Y-%m-%d"),
             # Date.Interview = format(Date.Interview2, "%Y-%m-%d"),
             # Date.Interview = as.Date(Date.Interview),
             month.interview = lubridate::floor_date(as.Date(Date.Interview), 'month'),#month(Date.Interview),
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
    
  )
  rdd <- list_bf2[[1]]
  
  r <- rdd %>% 
    mutate(ID_Men = NA,
           nbr = NA,
           survey = 'RDD') %>%
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
                                              month.interview == '2023-06-01'~'Jun-23'))
  bf <- rbind(r)
  
  
  # table(rdd2$Outcome, rdd2$response)
  
  # Then, limit to those with dernier_appel == 1 for wide version 
  rdd_wide <- rdd %>% 
    filter(dernier_appel == 1)
  
  # Clean up the outcome categories to try to match to MW and DRC in WIDE
  list_widebf <- list(rdd_wide)
  
  list_widebf2 <- list_widebf %>% map(
    ~ .x %>%
      mutate(Outcome.FINAL = ifelse(nbr_appel < 5 & (Outcome == 'NNA or NR' | Outcome == 'REASS' | Outcome == 'REFER'), 'PEND',
                                    ifelse(Outcome == 'INCOR', 'PEND',
                                           ifelse(Outcome == 'INCO', 'PART', Outcome))))
  )
  rdd_wide <- list_widebf2[[1]]
  r <- rdd_wide %>% 
    mutate(ID_Men = NA,
           nbr = NA,
           survey = 'RDD')
  
  bf_wide <- rbind(r)
  
  return(list(rdd, rdd_wide, bf, bf_wide))
  # Save to file to be used elsewhere
  # write_parquet(rdd, paste0(dir.input, 'RDD_all.parquet'))
  # write_parquet(ehcvm, paste0(dir.input, 'EHCVM_all.parquet'))
  # write_parquet(rdd_wide, paste0(dir.input, 'RDD_all_wide.parquet'))
  # write_parquet(ehcvm_wide, paste0(dir.input, 'EHCVM_all_wide.parquet'))
}
