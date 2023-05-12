# Get birth history dataset for Julio
# Load in packages
funcbirths <- function(Consentedmw){
  pkgs <- c('plyr','tidyverse', 'plotrix','lubridate','kableExtra','hrbrthemes','ggplot2','extrafont','float','reshape','gridExtra','rsvg','png','devtools','readxl','date', 'ggpubr', 'tidyselect', 'httr', 'jsonlite', 'extrafont','janitor',
          'haven')
lapply(pkgs, require, character.only = TRUE)

`%notin%` <- Negate(`%in%`)

r.functions.dir <- '/Users/lshvb5/Documents/rammps/RaMMPS_MWApp/'
dir.outputdata <- "/Users/lshvb5/OneDrive - London School of Hygiene and Tropical Medicine/General/Partners/IPOR/SurveyData/"
# dir.inputdata <- paste0(Sys.getenv('USERPROFILE'),"/OneDrive - London School of Hygiene and Tropical Medicine/Documents/General/Partners/IPOR/SurveyData/")
# 
# pull_data <- dget(paste0(r.functions.dir,'pull_data_func.R'))
# mwdata_rawt3 <- pull_data('malawirammps', 'ipormw', 'kelly.mccain@lshtm.ac.uk', 'rammpS!CTOsurvey')
# mwdata_rawt1 <- read.csv(paste0(dir.inputdata,'MalawiRAMMPS_WIDE_t1.csv'))
# mwdata_rawt2 <- read.csv(paste0(dir.inputdata,'MalawiRAMMPS_WIDE_t2.csv'))
# mwdata_raw <- plyr::rbind.fill(list(mwdata_rawt1, mwdata_rawt2, mwdata_rawt3), fill = TRUE)

# getmwdata <- dget(paste0(r.functions.dir,'Malawi_CleaningScript.R'))
# listdfsMW<- getmwdata(mwdata_raw)

# Consentedmw <- listdfsMW[[1]] # Consented

# Select out only the birth history variables and resp/HH characteristics
births <- Consentedmw %>%
  select(c(Date.Interview, starttime, endtime,
    caseid, users, Resp.Age, e2_label_1, e3_a_1, e3_exact_estimate_1, region_1, E4_1, e4b_1, e4c_1, C1_1, i1_1, i2_1, 
    b1_1, b2_1, b3_1, b4_1, b5_1, b5_number_1, b6_1, b6b_1, b7_1, b8_1, b8b_1, 
    starts_with('hd'), f_htotal_1, s_htotal_1, correct1_1, correct2_1, starts_with("bab"),
    # hd1_1, hd2_1, hd2_number_1, hd2b_1, hd2b_number_1, hd2c_1, hd2c_number_1, hd3_1, hd3_number_1, hd3b_1, hd3b_number_1,
    # hd3c_1, hd4_1, hd4_number_1, interviewer_gender_1, 
    # ph1_1, ph1_male_1, ph2_1, ph3_1, ph4a_1, ph4b_1, ph4t_1, ph5_1, ph6a_1, ph6b_1, ph6t_1,
    # ph7_1, ph8a_1, ph8b_1, ph8t_1, tbirths_1, ph9_1, ph10_1, ph11_1, ph11_months_1, ph11_weeks_1, ph12_1, ph13_1,
    # ph14a_1, ph14b_1, ph15a_1, ph16_1_1, ph16_1_2, ph17_1_1, ph17_1_2, ph18_1_1, ph18_1_2, ph18b_1_1, ph18b_1_2,
    # ph19_1_1, ph19_1_2, ph20_1_1, ph20_1_2, ph20b_1_1, ph20b_1_2, ph21_1_1, ph21_1_2, ph21a_1_1, ph21b_1_1, ph21b_1_2,
    # ph22_1_1, ph22_1_2, ph23_1_1, ph23_1_2, ph23b_1_1, ph23c_1_1, ph23c_1_2, ph23d_1_1, ph23d_1_2, ph24_1_1, ph24_1_2, 
    # ph24b_1_1, ph24c_1_1, ph24c_1_2, ph24d_1_1, ph25_1, ph25b_1, ph26_1, ph26b_1, ph26c_1, 
    starts_with('ph'),
    starts_with('fph'),
    d1_1, d2_1, d3_1, d3_specify_1, d4_1, d5_1,
    call_status
  )) %>%
  select(-c(starts_with('phone')))
birthsdta <- births
names(birthsdta) <- gsub(x = names(birthsdta), pattern = "\\.", replacement = "_")
write.csv(births, paste0(dir.outputdata,'BirthHistory_MW_',Sys.Date(),'.csv'))
names(birthsdta)
write_dta(birthsdta, paste0(dir.outputdata, 'BirthHistory_MW_',Sys.Date(),'.dta'))
}

# dtabirths <- read_dta(paste0(dir.outputdata, 'BirthHistory_MW_',Sys.Date(),'.dta'))

# source(paste0(r.functions.dir,'MW_Preg_hist_TPH.R'))
# Live.births <- mwpregfunc(Consentedmw)
# 
# source(paste0(r.functions.dir,'MW_Preg_hist_FPH.R'))
# Live.births_fph <- mwpregfunc_fph(Consentedmw)

