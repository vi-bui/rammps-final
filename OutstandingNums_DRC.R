# List of outstanding numbers 
pending <- dat.wide %>%
  dplyr::select(vars_select(names(dat.wide), !starts_with('lang') &!starts_with('inel')&!starts_with('region')&!starts_with('pull') &!starts_with('enum_')) ) %>%
  dplyr::select(-c(Source_1,Source_2, Source_3, Source_4, Source_5,Source.prov_1, Source.prov_2,Source.prov_3,Source.prov_4,Source.prov_5,time, Source.prov.1)) %>%
  dplyr::select(c(phone_1, enum, call_num, latest, Source.1, everything())) %>%
                              #caseid,phone_1,enumerator, Date.Interview, call_num, Outcome_1, Outcome_2, Outcome_3, Outcome_4, Outcome_5, Outcome_FINAL, Source.1,time) %>%
  dplyr::filter(Outcome.FINAL=='PEND') %>%
  mutate(enum= case_when(enum == 'E1' ~ "drc.5004@pma2020.org",
                                      enum == 'E2'~"drc.5006@pma2020.org",
                                      enum ==  'E3'~ "drc.5008@pma2020.org",
                                      enum == 'E4'~ "drc.5013@pma2020.org" ,
                                      enum ==  'E5'~ "drc.5312@pma2020.org",
                                      enum ==  'E6'~ "drc.5432@pma2020.org",
                                      enum == 'E7'~ "drc.5578@pma2020.org" ,
                                      enum == 'E8'~ "drc.5580@pma2020.org" ,
                                      enum == 'E9' ~ "rammps.5001@gmail.com",
                                      enum == 'E10'~ "rammps.5003@gmail.com" ,
                                      enum == 'E11'~ "rammps.5004@gmail.com" ,
                                      enum == 'E12'~ "rammps.5005@gmail.com" ,
                                      enum == 'E13' ~ "rammps.5006@gmail.com",
                                      enum == 'E14'~ "rammps.5007@gmail.com" ,
                                      enum ==  'E15'~ "rammps.5008@gmail.com",
                                      enum == 'E16'~ "rammps.5009@gmail.com" ,
                                      enum == 'E17' ~ "rammps.5010@gmail.com",
                                      enum == 'E18'~ "rammps.5011@gmail.com" ,
                                      enum == 'E19'~ "rammps.5012@gmail.com" ,
                                      enum == 'E20'~ "rammps.5013@gmail.com" ))
#write.csv(pending,'C:/Users/KellyMcCain/London School of Hygiene and Tropical Medicine/RAMMPS_LSHTM_Group - Documents/General/Partners/UNIKIN/SurveyCTO Audits/PendingNumbers.csv', row.names = FALSE)
cases <- read.csv('C:/Users/KellyMcCain/Downloads/cases (83).csv')

test <- left_join(pending, cases, by = 'phone_1')
