library(haven)
library(tidyverse)
library(labelled)
library(expss)
library(lubridate)
library(tidyselect)
# Code for exporting DRC data to be downloaded online
r.functions.dir <- 'C:/Users/KellyMcCain/London School of Hygiene and Tropical Medicine/rammps/RAMMPS_DRCDashboard/'
dir.gen <- paste0(Sys.getenv('USERPROFILE'),"/London School of Hygiene and Tropical Medicine/RAMMPS_LSHTM_Group - Documents/General/Partners/UNIKIN/IVR Numbers/")
dir.inputdata <- paste0(Sys.getenv('USERPROFILE'),"/OneDrive - London School of Hygiene and Tropical Medicine/Documents/General/Partners/UNIKIN/SurveyCTO Audits/Stata/")
dir.inputdata2 <- paste0(Sys.getenv('USERPROFILE'),"/OneDrive - London School of Hygiene and Tropical Medicine/Documents/General/Partners/UNIKIN/SurveyCTO Audits/")

# will actually have to get the data from stata - and update the cleaning script or make a new one so that it will work with the stata file 
# data_raw_csv <- read.csv(paste0(dir.inputdata2,"RAMMPS DRC 20_07_WIDE.csv")) 
# data_raw_csv1 <- data_raw_csv %>%
#   dplyr::select(vars_select(names(data_raw_csv), !starts_with('log') & !starts_with('next') & !starts_with('find') 
#                             & !starts_with('add') & !starts_with('call_end') & !starts_with('timestamp') &
#                               !starts_with('call_info') & !starts_with('length_s') & !starts_with('start') &
#                               !starts_with('end_') & !starts_with('pn') & !starts_with('call_simp') & 
#                               !starts_with('all_') & !starts_with('all_pn')&!starts_with('num_') 
#   ))
data_raw_dta1 <- haven::read_dta(paste0(dir.inputdata,"RAMMPS DRC 20_07.dta"))
# data_raw_dta1 <- read.csv(paste0(dir.inputdata2,"RAMMPS DRC 20_07_WIDE.csv"))
data_raw_dta <- data_raw_dta1 %>%
  dplyr::select(vars_select(names(data_raw_dta1), !starts_with('log') & !starts_with('next') & !starts_with('find') 
                            & !starts_with('add') & !starts_with('call_end') & !starts_with('timestamp') &
                              !starts_with('call_info') & !starts_with('length_s') & 
                              !starts_with('end_') & !starts_with('pn') & !starts_with('call_simp') & 
                              !starts_with('all_') & !starts_with('all_pn')&!starts_with('num_') &!starts_with('time_availabil') &
                              !starts_with('north_') & !starts_with('kin_')
  )) %>%
  dplyr::select(-c(quota_id, quota_totalb4, quota_totalnew)) 
# data_raw_dta_labs <- data_raw_dta %>%
#   expss::apply_labels(
#              highest_grades_a = "What is the highest standard you completed at primary level?",
#              highest_grades_b = "What is the highest standard you completed at seconday level?",
#              highest_grades_c = 'What is the highest standard you completed at higher educ level?',
#              roofing_material = "What is the main construction material of the roof of your house?",
#              water_source = "What is the main source of drinking-water for members of your household?",
#              hd1_householdrel = "What is your relationship to the head of the household?"
#              ) 
# data_raw_dta_labs2 <- data_raw_dta_labs %>%
#   labelled::add_value_labels(highest_grades_a = c("less than one year"=10, "Première"=11,"Deuxième"=12,"Troisième"=13,
#                                         "Quatrième"=14, "Cinquième"=15,"Sixième"=16,"NE SAIT PAS"=98,"Prefer not to say"=-99),
#                    highest_grades_b = c("less than one year"=20, "Première"=21,"Deuxième"=22,"Troisième"=23,
#                                         "Quatrième"=24, "Cinquième"=25,"Sixième"=26,"NE SAIT PAS"=98,"Prefer not to say"=-99),
#                    highest_grades_c = c("less than one year"=30,"1er Graduat"=31, "2ème Graduat" =32, "3ème Graduat"=33,
#                                         "1ère Licence"=34,"2ème Licence"=25, "NE SAIT PAS" =98,"Prefer not to say"=-99),
#                    roofing_material = c("No roof" =1,	"Grass/Thatch/Palm"=2, "Cardboard"=3,	"Plastic/Tarpaulin"=4, "Wood"=5,
#                                         "Metal (corrugated iron)"=6, "Cement/Concrete/Tiles" =7, "Other (specify)"=8, "Don't Know"=9,	"Refuse"='R'),
#                    water_source = c("Piped into dwelling"=1,"Piped to yard/plot"=2, "Public tap"=3, "Tubewell/Borehole"=4, "Protected well"=5,
#                                     "Unprotected well"=6,	"Protected spring" =7, "Unprotected spring"=8, "Rainwater"=9, "Bottled water"=10,
#                                     "Cart with small tank"=11, "Tank/Drum"=12, "Tanker-truck"=13, "Surface water" =14,"Other (specify)"=15,
#                                     "Refuse" ="R"),
#                    hd1_householdrel = c("Head"=1, "Wife or Husband"=2, "Son or Daughter"=3, "SON/DAUGHTER-IN-LAW"=4, "Grandchild"=5,	"Parent"=6,
#                                         "Parent-in-law"=7, "Brother or Sister"=8, "Other relative"=9, "Adopted/Foster/Stepchild"=10, "Not related"=11,
#                                         "Don't know"=12,"Refuse"="R"),
#                    L1_a_1 = c("French"=6, "Lingala"=2, "Swahili"=3, "Kikongo"=4, "Tshiluba" =5, "Other"=888, "Refuse"=7),
#                    call_status = c("Completed"="1", "Partially completed (no callback)"="2a", "Refusal"="6","Number does not work/is not functional" = "7",
#                                    "Ineligible (no referral )"="9",	"Incomprehensible"="4a", "Incomplete"="2", "No answer"="5",	"Busy tone"="10",
#                                    "Phone number is functional but inaccessible"="7a", "Referral"="8", "Deferral"="11",	"Answered, but not by the respondent"='3',
#                                    "Reassigned (e.g. didn't speak the same language)"="4","Other"='Oth'
# ))

ivr <- read.csv(paste0(dir.gen,"engageSPARK_contacts_ALL_20211021.csv")) %>%
  dplyr::select(c(Phone.Number, Province)) %>%
  mutate(Province = ifelse(Province == '1'|Province=='Kinshasa', 'Kinshasa',
                           ifelse(Province =='2'|Province=='Nord Kivu', 'Nord Kivu',
                                  ifelse(Province == 'Other Province','Other Province',NA))),
         source = ifelse(is.na(Province) | Province == 'Other Province','IVR group 2', 'IVR group 1'),
         Phone.Number = as.numeric(substring(Phone.Number, 4))) %>%
  dplyr::rename(phone_1 = Phone.Number)
feroxus <- read.csv(paste0(dir.gen, 'FeroxusNumbers.csv'))

getdata <- dget(paste0(r.functions.dir,'CleaningScript_func_Stata.R'))
# getdata <- dget(paste0(r.functions.dir,'CleaningScript_func.R'))
listdfs<- getdata(data_raw_dta, feroxus, ivr)

Consented <- listdfs[[1]] %>%
  expss::apply_labels(highest_grades_a = "What is the highest standard you completed at primary level?",
                 highest_grades_b = "What is the highest standard you completed at seconday level?",
                 highest_grades_c = 'What is the highest standard you completed at higher educ level?',
                 roofing_material = "What is the main construction material of the roof of your house?",
                 water_source = "What is the main source of drinking-water for members of your household?",
                 hd1_householdrel = "What is your relationship to the head of the household?"
                 ) %>%
  labelled::copy_labels_from(data_raw_dta) %>%
  labelled::add_value_labels(highest_grades_a = c("less than one year"=10, "Première"=11,"Deuxième"=12,"Troisième"=13,
                                                  "Quatrième"=14, "Cinquième"=15,"Sixième"=16,"NE SAIT PAS"=98,"Prefer not to say"=-99),
                             highest_grades_b = c("less than one year"=20, "Première"=21,"Deuxième"=22,"Troisième"=23,
                                                  "Quatrième"=24, "Cinquième"=25,"Sixième"=26,"NE SAIT PAS"=98,"Prefer not to say"=-99),
                             highest_grades_c = c("less than one year"=30,"1er Graduat"=31, "2ème Graduat" =32, "3ème Graduat"=33,
                                                  "1ère Licence"=34,"2ème Licence"=25, "NE SAIT PAS" =98,"Prefer not to say"=-99),
                             roofing_material = c("No roof" =1,	"Grass/Thatch/Palm"=2, "Cardboard"=3,	"Plastic/Tarpaulin"=4, "Wood"=5,
                                                  "Metal (corrugated iron)"=6, "Cement/Concrete/Tiles" =7, "Other (specify)"=8, "Don't Know"=9,	"Refuse"='R'),
                             water_source = c("Piped into dwelling"=1,"Piped to yard/plot"=2, "Public tap"=3, "Tubewell/Borehole"=4, "Protected well"=5,
                                              "Unprotected well"=6,	"Protected spring" =7, "Unprotected spring"=8, "Rainwater"=9, "Bottled water"=10,
                                              "Cart with small tank"=11, "Tank/Drum"=12, "Tanker-truck"=13, "Surface water" =14,"Other (specify)"=15,
                                              "Refuse" ="R"),
                             hd1_householdrel = c("Head"=1, "Wife or Husband"=2, "Son or Daughter"=3, "SON/DAUGHTER-IN-LAW"=4, "Grandchild"=5,	"Parent"=6,
                                                  "Parent-in-law"=7, "Brother or Sister"=8, "Other relative"=9, "Adopted/Foster/Stepchild"=10, "Not related"=11,
                                                  "Don't know"=12,"Refuse"="R"),
                             Resp_Language = c("French"=6, "Lingala"=2, "Swahili"=3, "Kikongo"=4, "Tshiluba" =5, "Other"=888, "Refuse"=7),
                             call_status = c("Completed"="1", "Partially completed (no callback)"="2a", "Refusal"="6","Number does not work/is not functional" = "7",
                                             "Ineligible (no referral )"="9",	"Incomprehensible"="4a", "Incomplete"="2", "No answer"="5",	"Busy tone"="10",
                                             "Phone number is functional but inaccessible"="7a", "Referral"="8", "Deferral"="11",	"Answered, but not by the respondent"='3',
                                             "Reassigned (e.g. didn't speak the same language)"="4","Other"='Oth'
                             ))

data <- listdfs[[2]]%>%
  expss::apply_labels(highest_grades_a = "What is the highest standard you completed at primary level?",
                      highest_grades_b = "What is the highest standard you completed at seconday level?",
                      highest_grades_c = 'What is the highest standard you completed at higher educ level?',
                      roofing_material = "What is the main construction material of the roof of your house?",
                      water_source = "What is the main source of drinking-water for members of your household?",
                      hd1_householdrel = "What is your relationship to the head of the household?"
  ) %>%
  labelled::copy_labels_from(data_raw_dta) %>%
  labelled::add_value_labels(highest_grades_a = c("less than one year"=10, "Première"=11,"Deuxième"=12,"Troisième"=13,
                                                  "Quatrième"=14, "Cinquième"=15,"Sixième"=16,"NE SAIT PAS"=98,"Prefer not to say"=-99),
                             highest_grades_b = c("less than one year"=20, "Première"=21,"Deuxième"=22,"Troisième"=23,
                                                  "Quatrième"=24, "Cinquième"=25,"Sixième"=26,"NE SAIT PAS"=98,"Prefer not to say"=-99),
                             highest_grades_c = c("less than one year"=30,"1er Graduat"=31, "2ème Graduat" =32, "3ème Graduat"=33,
                                                  "1ère Licence"=34,"2ème Licence"=25, "NE SAIT PAS" =98,"Prefer not to say"=-99),
                             roofing_material = c("No roof" =1,	"Grass/Thatch/Palm"=2, "Cardboard"=3,	"Plastic/Tarpaulin"=4, "Wood"=5,
                                                  "Metal (corrugated iron)"=6, "Cement/Concrete/Tiles" =7, "Other (specify)"=8, "Don't Know"=9,	"Refuse"='R'),
                             water_source = c("Piped into dwelling"=1,"Piped to yard/plot"=2, "Public tap"=3, "Tubewell/Borehole"=4, "Protected well"=5,
                                              "Unprotected well"=6,	"Protected spring" =7, "Unprotected spring"=8, "Rainwater"=9, "Bottled water"=10,
                                              "Cart with small tank"=11, "Tank/Drum"=12, "Tanker-truck"=13, "Surface water" =14,"Other (specify)"=15,
                                              "Refuse" ="R"),
                             hd1_householdrel = c("Head"=1, "Wife or Husband"=2, "Son or Daughter"=3, "SON/DAUGHTER-IN-LAW"=4, "Grandchild"=5,	"Parent"=6,
                                                  "Parent-in-law"=7, "Brother or Sister"=8, "Other relative"=9, "Adopted/Foster/Stepchild"=10, "Not related"=11,
                                                  "Don't know"=12,"Refuse"="R"),
                             Resp_Language = c("French"=6, "Lingala"=2, "Swahili"=3, "Kikongo"=4, "Tshiluba" =5, "Other"=888, "Refuse"=7),
                             call_status = c("Completed"="1", "Partially completed (no callback)"="2a", "Refusal"="6","Number does not work/is not functional" = "7",
                                             "Ineligible (no referral )"="9",	"Incomprehensible"="4a", "Incomplete"="2", "No answer"="5",	"Busy tone"="10",
                                             "Phone number is functional but inaccessible"="7a", "Referral"="8", "Deferral"="11",	"Answered, but not by the respondent"='3',
                                             "Reassigned (e.g. didn't speak the same language)"="4","Other"='Oth'
                             ))

labels <- labelled::var_label(Consented)
vallabels <- labelled::val_labels(Consented)
# write.csv 
# and write to .dta file 
# figure out how to label the variables well 

######## Divide into modules
# each will have misc+derived, module timing, intro, consent+interview, and background variables
varsforeach <- c(# misc + derived removed: 'starttime',
                 'caseid', 'submissiondate',  'starttime','endtime', 'Date_Interview', 'month_interview', 'call_num', 'enumerator',
                 # module timing 
                 'duration', 'elig_time', 'consent_time', 'arrangements_time', 'background_time', 'vax_time', 'hh_time', 'ps_time', 'ssh_time',
                 'db_time', 'phone_call_duration',
                 # intro
                 'response_1', 'Resp_Language', 'l1_b_1', 'Resp_Sex', 'e4a', 'Resp_Region', 'e5', 'e6', 'e7', 'e7_b', 'Resp_Age', 'agegp', 'e3a', 
                 # consent + interview
                 'Resp_Consent', 'call_location', 'specify_location', 'continue_discontinue', 'speakerphone',
                 # background
                 'school_attendance', 'Resp_Educ', 'highest_grades_a', 'highest_grades_b', 'highest_grades_c', 'Resp_Marriage', 'marriage_formal',
                 'marital_status', 'Resp_Marriage_lab', 'OwnPhone', 'b5', 'roofing_material', 'electricity_status', 'water_source',
                 'other_watersource',
                 #debrief
                 'd1','call_status', 'call_feedback', 'call_feedback_1', 'call_feedback_2', 'call_feedback_3', 'call_feedback_4',
                 'call_feedback_5', 'call_feedback_other', 'respondent_cooperation'
                 )

base_df <- data %>%
  select(c(all_of(c(varsforeach))))

# Covid Vaccination
covidvars <- c('vaccine_dose', 'vaccine_names', 'vaccine_likelihood', 'vaccines_den_reasons', 'vaccines_den_reasons_1', 'vaccines_den_reasons_2', 
               'vaccines_den_reasons_3', 'vaccines_den_reasons_4', 'vaccines_den_reasons_5', 'vaccines_den_reasons_6', 'vaccines_den_reasons_7', 
               'vaccines_den_reasons_8', 'vaccines_den_reasons_9', 'vaccines_den_reasons_10', 'vaccines_den_reasons_11', 'vaccines_den_reasons_12',
               'vaccines_den_reasons_13', 'vaccines_den_reasons_14', 'vaccines_den_reasons_15', 'vaccines_den_reasons_16', 'vaccines_den_reasons_17',
               'vaccines_den_reasons_18', 'vaccines_den_reasons_19', 'vaccines_den_reasons_20', 'vaccines_den_reasons_r', 'other_den_reasons',
               'vaccines_yes_reasons', 'vaccines_yes_reasons_1', 'vaccines_yes_reasons_2', 'vaccines_yes_reasons_3', 'vaccines_yes_reasons_4',
               'vaccines_yes_reasons_5', 'vaccines_yes_reasons_6', 'vaccines_yes_reasons_7','vaccines_yes_reasons_8', 'vaccines_yes_reasons_9', 
               'vaccines_yes_reasons_10', 'vaccines_yes_reasons_11', 'vaccines_yes_reasons_12', 'vaccines_yes_reasons_13', 'vaccines_yes_reasons_14',
               'vaccines_yes_reasons_15', 'vaccines_yes_reasons_r', 'other_yes_reasons'
)
covid_df <- Consented %>%
  select(c(all_of(c(varsforeach, covidvars))))

# Household deaths 
hhvars <- c('hd1_householdrel', 'U5', 'O5', 'HHsize', 'hd4', 'U5Death', 'hd6', 'O5Death')
           
hh_df <- Consented %>%
  select(c(all_of(c(varsforeach, hhvars)),starts_with('hd5'), starts_with('hd7'))) 

# Parental Survival
parvars <- c('M_Dead', 'M_alive_age', 'note_momagealive_1', 'mistake_momalive_1', 'corr_respagemomalive_1', 'corr_momagealive_1',
             'ps3_parent_home_1', 'otherloc_copy_1', 'ps4_parents_jab_1', 'M_dead_Yr', 'mom2019die_1', 'momspecificyear_1', 
             'momdie10yrs_1', 'ps7_months_1', 'ps5_p_died_agecat_1','M_dead_Age', 'note_momagedead_1', 'mistake_momdead_1', 
             'corr_respagemomdead_1', 'corr_momagedead_1', 'corr_momyeardead_1', 'live_before_passing_1','otherloc_1',
             'cd1_1', 'cd2_1', 'cd3_1', 'cd4_1', 'cd5_1', 'cd6_1', 'cd7_1', 'cd7_1_1', 'cd7_2_1', 'cd7_3_1', 'cd7_4_1', 'cd7_5_1', 
             'cd7_6_1', 'cd7_7_1', 'cd7_8_1', 'cd7_9_1', 'cd7_10_1', 'cd7_11_1', 'cd7_12_1', 'cd7_r_1', 'cd7other_1', 'cd8_1', 
             'cd9_1', 'cd11_1', 'cd12_1', 'cd13_1', 'cd14_1', 'cd16_1', 'cd18_1', 'cd19_1', 'cd20_1',
             'F_Dead', 'F_alive_age', 'note_dadagealive_1', 'mistake_dadalive_1', 'corr_respagedadalive_1', 'corr_dadagealive_1', 
             'ps3_parent_home2_1', 'otherloc_copy2_1', 'ps4_parents_jab2_1', 'F_dead_Yr', 'dad2019die_1', 'dadspecificyear_1', 
             'daddie10yrs_copy_1', 'ps7_months2_1', 'ps5_p_died_agecat2_1', 'F_dead_Age', 'note_dadagedead_1', 'mistake_daddead_1', 
             'corr_respagedaddead_1', 'corr_dadagedead_1', 'corr_dadyeardead_1', 'live_before_passing2_1', 'otherloc2_1',
             'cd4_2_1', 'cd5_2_1', 'cd6_2_1', 'cd7_2_1', 'cd7_2_1_1', 'cd7_2_2_1', 'cd7_2_3_1', 'cd7_2_4_1', 'cd7_2_5_1', 'cd7_2_6_1',
             'cd7_2_7_1', 'cd7_2_8_1', 'cd7_2_9_1', 'cd7_2_10_1', 'cd7_2_11_1', 'cd7_2_12_1', 'cd7_2_r_1', 'cd7_2other_1', 'cd8_2_1', 
             'cd9_2_1', 'cd11_2_1', 'cd12_2_1', 'cd13_2_1', 'cd14_2_1', 'cd16_2_1', 'cd18_2_1', 'cd19_2_1', 'cd20_2_1')

par_df <- Consented %>%
  select(all_of(c(varsforeach, parvars)))

# Sibling survival
sibsvars <- c('Sisters', 'ssh2_died_1', 'ssh3_died_1', 
              'Brothers', 'ssh2_died_bro', 'ssh3_died_bro')

sibs_df <- Consented %>%
  select(c(all_of(c(varsforeach, sibsvars)), starts_with('ssh4_yearDied') | 
             starts_with('ssh4_months') | starts_with('cd1_copy') | starts_with('cd2_copy') | starts_with('cd3_copy') | 
             starts_with('cd4_copy') | starts_with('cd5_copy') | starts_with('cd6_copy') |starts_with('cd7_copy') | 
             starts_with('cd7copyother') | starts_with('cd8_copy') | starts_with('cd9_copy') | starts_with('cd11_copy') | 
             starts_with('cd12_copy') | starts_with('cd13_copy') | starts_with('cd14_copy') |starts_with('cd15_copy') |
             starts_with('cd16_copy') | starts_with('cd18_copy') | starts_with('cd19_copy') | starts_with('cd20_copy'))) 


# Save all data
listofdfs <- list(base_df, covid_df, hh_df, par_df, sibs_df, Consented, data) %>%
  set_names(c('Base_Dataset','COVID_Module','Household_Module','ParentalSurvival_Module','SiblingSurvival_Module', 'Consented_all', 'Full_Dataset'))



names(listofdfs) %>%
  map(.f = ~write_csv(listofdfs[[.x]], paste0(dir.inputdata2, "Datasets to Share/", .x, ".csv")))
# fixing names for .dta file
names(listofdfs) %>% 
  gsub(x = names(listofdfs[[.x]]), pattern = "\\.", replacement = "_")
names(listofdfs) %>% 
  map(.f = ~write_dta(listofdfs[[.x]], paste0(dir.inputdata2, "Datasets to Share/", .x, ".dta"))) 

