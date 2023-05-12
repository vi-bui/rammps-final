# socio demographic table function 

function(Consented, Kin, NK){
  # === Table of characteristics
  library(questionr)
  SDT <- function(var1, var2, var3){
    D1 <- questionr::freq(var1, cum = FALSE,  total = F)[,-3] 
    D2<- questionr::freq(var2, cum = FALSE,  total = F)[,-3]
    D3 <- questionr::freq(var3, cum = FALSE,  total = F)[,-3]
    D <- cbind(D1, D2, D3)
    D[is.na(D)] <- 0   #; rownames(D) <- D$Row.names ; D$Row.names <- NULL ; D$Row.names  <- NULL         # replace NA values
    colnames(D) <- c("Freq", "%", "Freq", "%", "Freq", "%")
    return(D)
  }
  #re-ordering vars
  #resp education
  Consented$Resp.Educ_lab <- ordered(Consented$Resp.Educ_lab, levels = c("No Education" ,"Primary","Secondary", "Higher", 'Refuse'))
  Kin$Resp.Educ_lab <- ordered(Kin$Resp.Educ_lab, levels = c("No Education" ,"Primary","Secondary", "Higher",'Refuse'))
  NK$Resp.Educ_lab <- ordered(NK$Resp.Educ_lab, levels = c("No Education" ,"Primary","Secondary", "Higher", 'Refuse'))
  #resp marriage
  Consented$Resp.Marriage_lab <- ordered(Consented$Resp.Marriage_lab, levels = c('Never married','Married/Cohabiting','Divorced/Separated','Widowed/(cohabiting) partner passed away','Refuse'))
  Kin$Resp.Marriage_lab <- ordered(Kin$Resp.Marriage_lab, levels = c('Never married','Married/Cohabiting','Divorced/Separated','Widowed/(cohabiting) partner passed away','Refuse'))
  NK$Resp.Marriage_lab <- ordered(NK$Resp.Marriage_lab, levels = c('Never married','Married/Cohabiting','Divorced/Separated','Widowed/(cohabiting) partner passed away','Refuse'))
  #resp residence
  Consented$E4a_lab <- ordered(Consented$E4a_lab, levels = c('City','Town/Trading Centre','Rural','Refuse'))
  Kin$E4a_lab <- ordered(Kin$E4a_lab, levels = c('City','Town/Trading Centre','Rural','Refuse'))
  NK$E4a_lab <- ordered(NK$E4a_lab, levels = c('City','Town/Trading Centre','Rural','Refuse'))
  #resp water
  Consented$water_source_lab <- ordered(Consented$water_source_lab, levels = c('Piped into dwelling','Piped to yard/plot','Public tap','Tubewell/Borehole','Protected well','Unprotected well','Protected spring','Unprotected spring','Rainwater','Bottled water','Cart with small tank','Tank/Drum','Tanker-truck','Surface water','Other','Refuse'))
  Kin$water_source_lab <- ordered(Kin$water_source_lab, levels = c('Piped into dwelling','Piped to yard/plot','Public tap','Tubewell/Borehole','Protected well','Unprotected well','Protected spring','Unprotected spring','Rainwater','Bottled water','Cart with small tank','Tank/Drum','Tanker-truck','Surface water','Other','Refuse'))
  NK$water_source_lab <- ordered(NK$water_source_lab, levels = c('Piped into dwelling','Piped to yard/plot','Public tap','Tubewell/Borehole','Protected well','Unprotected well','Protected spring','Unprotected spring','Rainwater','Bottled water','Cart with small tank','Tank/Drum','Tanker-truck','Surface water','Other','Refuse'))
  
  Des.table <- rbind(SDT(Consented$Resp.Sex, Kin$Resp.Sex, NK$Resp.Sex),
                     SDT(Consented$Resp.Age.grp_lab, Kin$Resp.Age.grp_lab, NK$Resp.Age.grp_lab),  
                     SDT(Consented$Resp.Region_lab, Kin$Resp.Region_lab, NK$Resp.Region_lab),
                     SDT(Consented$Resp.Language, Kin$Resp.Language, NK$Resp.Language), 
                     SDT(Consented$Resp.Educ_lab, Kin$Resp.Educ_lab, NK$Resp.Educ_lab), 
                     SDT(Consented[!is.na(Consented$Resp.Marriage_lab),]$Resp.Marriage_lab, Kin[!is.na(Kin$Resp.Marriage_lab),]$Resp.Marriage_lab, NK[!is.na(NK$Resp.Marriage_lab),]$Resp.Marriage_lab),
                     SDT(Consented$E4a_lab, Kin$E4a_lab, NK$E4a_lab),
                     SDT(Consented[Consented$electricity_status_lab!="Don't know"&!is.na(Consented$electricity_status_lab),]$electricity_status_lab, 
                         Kin[Kin$electricity_status_lab!="Don't know"&!is.na(Kin$electricity_status_lab),]$electricity_status_lab,
                         NK[NK$electricity_status_lab!="Don't know"&!is.na(NK$electricity_status_lab),]$electricity_status_lab),
                     SDT(Consented[!is.na(Consented$water_source_lab),]$water_source_lab, 
                         Kin[!is.na(Kin$water_source_lab),]$water_source_lab, 
                         NK[!is.na(NK$water_source_lab),]$water_source_lab))
  #Des.table <- Des.table[c(1:8,10,11,12,9,13,14,15,17,16, 18,20, 21,19, 22:nrow(Des.table)), ] 
  
  
  Des.table$Categories <- rownames(Des.table)
  Des.table$Variable <- c("Sex", rep(NA, length(unique(Consented$Resp.Sex))-1),
                          "Age", rep(NA, length(unique(Consented$Resp.Age.grp_lab))-1),
                          "Region", rep(NA, length(unique(Consented$Resp.Region_lab))-1),
                          "Language", rep(NA, length(unique(Consented$Resp.Language))-1),
                          "Education", rep(NA, length(unique(Consented$Resp.Educ_lab))-1),
                          "Marriage", rep(NA, length(unique(Consented$Resp.Marriage_lab))-2),
                          "Residence", rep(NA, 3),
                          "Electricity", rep(NA, length(unique(Kin$electricity_status_lab))-1),
                          "Water Source", rep(NA, length(unique(Consented$water_source_lab))-1))
  Des.table <- Des.table%>%relocate(Variable, Categories) %>%
    mutate(Categories = ifelse(Categories == 'Refuse1' | Categories == 'Refuse2' | Categories == 'Refuse3' | Categories == 'Refuse4', 'Refuse',Categories),
           Freq.1 = ifelse(Categories == 'Nord Kivu', 0, Freq.1),
           `%.1` = ifelse(Categories == 'Nord Kivu', 0 , `%.1`),
           Freq.2 = ifelse(Categories == 'Kinshasa', 0, Freq.2),
           `%.2` = ifelse(Categories == 'Kinshasa', 0 , `%.2`))
  rownames(Des.table) <- NULL
  Des.table$Variable[is.na(Des.table$Variable)] <- ""
  
  
  
  Des.table.1 <- Des.table %>%
    kbl(caption = "") %>%
    kable_classic(full_width = F, html_font = "Arial")%>%
    kable_styling(latex_options = 'hold_position')
  return(add_header_above(Des.table.1, c(" "=2, "Total " = 2, "Kinshasa" = 2, "Nord Kivu" = 2)))
}