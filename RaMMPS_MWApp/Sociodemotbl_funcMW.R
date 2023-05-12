# socio demographic table function 

function(Consentedmw, Northern, Central, Southern){
  # === Table of characteristics
  library(questionr)
  SDT <- function(var1, var2, var3, var4){
    D1 <- questionr::freq(var1, cum = FALSE,  total = F)[,-3] 
    D2<- questionr::freq(var2, cum = FALSE,  total = F)[,-3]
    D3 <- questionr::freq(var3, cum = FALSE,  total = F)[,-3]
    D4 <- questionr::freq(var4, cum = FALSE,  total = F)[,-3]
    # D1$var <- rownames(D1)
    # D2$var <- rownames(D2)
    # D3$var <- rownames(D3)
    # D4$var <- rownames(D4)
    #D12 <- merge(D1, D2, by=0, all=TRUE)  # merge by row names (by=0 or by="row.names")
    D <- cbind(D1, D2, D3, D4)
    #rownames(D12) <- D12$Row.names
    #D[is.na(D)] <- 0                 # replace NA values
    #D <- merge(D12, D3, by=0, all=TRUE)  # merge by row names (by=0 or by="row.names")
    D[is.na(D)] <- 0   #; rownames(D) <- D$Row.names ; D$Row.names <- NULL ; D$Row.names  <- NULL         # replace NA values
    colnames(D) <- c("Freq", "%", "Freq", "%", "Freq", "%", "Freq", "%")
    return(D)
  }
  #re-ordering vars
  #resp education
  Consentedmw$Resp.Educ_lab <- ordered(Consentedmw$Resp.Educ_lab, levels = c("No Education" ,"Primary","Secondary", "Higher"))
  Northern$Resp.Educ_lab <- ordered(Northern$Resp.Educ_lab, levels = c("No Education" ,"Primary","Secondary", "Higher"))
  Central$Resp.Educ_lab <- ordered(Central$Resp.Educ_lab, levels = c("No Education" ,"Primary","Secondary", "Higher"))
  Southern$Resp.Educ_lab <- ordered(Southern$Resp.Educ_lab, levels = c("No Education" ,"Primary","Secondary", "Higher"))
  #resp marriage
  Consentedmw$Resp.Marriage_lab <- ordered(Consentedmw$Resp.Marriage_lab, levels = c('Never married','Married/Cohabiting','Divorced/Separated','Widowed/(cohabiting) partner passed away','Refuse'))
  Northern$Resp.Marriage_lab <- ordered(Northern$Resp.Marriage_lab, levels = c('Never married','Married/Cohabiting','Divorced/Separated','Widowed/(cohabiting) partner passed away','Refuse'))
  Central$Resp.Marriage_lab <- ordered(Central$Resp.Marriage_lab, levels = c('Never married','Married/Cohabiting','Divorced/Separated','Widowed/(cohabiting) partner passed away','Refuse'))
  Southern$Resp.Marriage_lab <- ordered(Southern$Resp.Marriage_lab, levels = c('Never married','Married/Cohabiting','Divorced/Separated','Widowed/(cohabiting) partner passed away','Refuse'))
  #resp residence
  Consentedmw$RuralUrban <- ordered(Consentedmw$RuralUrban, levels = c('Rural','Urban'))
  Northern$RuralUrban <- ordered(Northern$RuralUrban, levels = c('Rural','Urban'))
  Central$RuralUrban <- ordered(Central$RuralUrban, levels = c('Rural','Urban'))
  Southern$RuralUrban <- ordered(Southern$RuralUrban, levels = c('Rural','Urban'))
  #resp water
  Consentedmw$water_source <- ordered(Consentedmw$water_source, levels = c('Piped into dwelling','Piped to yard/plot','Public tap','Tubewell/Borehole','Protected well','Unprotected well','Protected spring','Unprotected spring','Rainwater','Bottled water','Cart with small tank','Tank/Drum','Tanker-truck','Surface water','Other','Refuse'))
  Northern$water_source <- ordered(Northern$water_source, levels = c('Piped into dwelling','Piped to yard/plot','Public tap','Tubewell/Borehole','Protected well','Unprotected well','Protected spring','Unprotected spring','Rainwater','Bottled water','Cart with small tank','Tank/Drum','Tanker-truck','Surface water','Other','Refuse'))#'Bottled water','Cart with small tank','Tanker-truck','Refuse'
  Central$water_source <- ordered(Central$water_source, levels = c('Piped into dwelling','Piped to yard/plot','Public tap','Tubewell/Borehole','Protected well','Unprotected well','Protected spring','Unprotected spring','Rainwater','Bottled water','Cart with small tank','Tank/Drum','Tanker-truck','Surface water','Other','Refuse'))#'Rainwater','Cart with small tank','Tanker-truck',
  Southern$water_source <- ordered(Southern$water_source, levels = c('Piped into dwelling','Piped to yard/plot','Public tap','Tubewell/Borehole','Protected well','Unprotected well','Protected spring','Unprotected spring','Rainwater','Bottled water','Cart with small tank','Tank/Drum','Tanker-truck','Surface water','Other','Refuse'))#'Rainwater','Tank/Drum',
  # fixing lang var
  Consentedmw$Resp.Language <- ordered(Consentedmw$Resp.Language, levels = c('Chichewa','Chisena',"Chitumbuka",'Chiyao','English'))
  Northern$Resp.Language <- ordered(Northern$Resp.Language, levels = c('Chichewa','Chisena',"Chitumbuka",'Chiyao','English'))
  Central$Resp.Language <- ordered(Central$Resp.Language, levels = c('Chichewa','Chisena',"Chitumbuka",'Chiyao','English'))
  Southern$Resp.Language <- ordered(Southern$Resp.Language, levels = c('Chichewa','Chisena',"Chitumbuka",'Chiyao','English'))
  #resp electricity
  Consentedmw$electricity_status_lab <- ordered(Consentedmw$electricity_status_lab, levels = c('Access to electricity', 'No access to electricity', 'Refuse'))
  Northern$electricity_status_lab <- ordered(Northern$electricity_status_lab, levels = c('Access to electricity', 'No access to electricity', 'Refuse'))
  Central$electricity_status_lab <- ordered(Central$electricity_status_lab, levels = c('Access to electricity', 'No access to electricity', 'Refuse'))
  Southern$electricity_status_lab <- ordered(Southern$electricity_status_lab, levels = c('Access to electricity', 'No access to electricity', 'Refuse'))
  
  Des.table <- rbind(SDT(Consentedmw$Resp.Sex, Northern$Resp.Sex, Central$Resp.Sex, Southern$Resp.Sex),
                     SDT(Consentedmw$Resp.Age.grp_lab, Northern$Resp.Age.grp_lab, Central$Resp.Age.grp_lab, Southern$Resp.Age.grp_lab),  
                     SDT(Consentedmw$Resp.Region, Northern$Resp.Region, Central$Resp.Region, Southern$Resp.Region),
                     SDT(Consentedmw$Resp.Language, Northern$Resp.Language, Central$Resp.Language, Southern$Resp.Language), 
                     SDT(Consentedmw[!is.na(Consentedmw$Resp.Educ_lab),]$Resp.Educ_lab, 
                         Northern[!is.na(Northern$Resp.Educ_lab),]$Resp.Educ_lab, 
                         Central[!is.na(Central$Resp.Educ_lab),]$Resp.Educ_lab, 
                         Southern[!is.na(Southern$Resp.Educ_lab),]$Resp.Educ_lab), 
                     SDT(Consentedmw[!is.na(Consentedmw$Resp.Marriage_lab),]$Resp.Marriage_lab, 
                         Northern[!is.na(Northern$Resp.Marriage_lab),]$Resp.Marriage_lab, 
                         Central[!is.na(Central$Resp.Marriage_lab),]$Resp.Marriage_lab,
                         Southern[!is.na(Southern$Resp.Marriage_lab),]$Resp.Marriage_lab),
                     SDT(Consentedmw[!is.na(Consentedmw$RuralUrban),]$RuralUrban, Northern[!is.na(Northern$RuralUrban),]$RuralUrban, 
                         Central[!is.na(Central$RuralUrban),]$RuralUrban, Southern[!is.na(Southern$RuralUrban),]$RuralUrban),
                     SDT(Consentedmw[Consentedmw$electricity_status_lab!="Don't know"&!is.na(Consentedmw$electricity_status_lab),]$electricity_status_lab, 
                         Northern[Northern$electricity_status_lab!="Don't know"&!is.na(Northern$electricity_status_lab),]$electricity_status_lab,
                         Central[Central$electricity_status_lab!="Don't know"&!is.na(Central$electricity_status_lab),]$electricity_status_lab,
                         Southern[Southern$electricity_status_lab!="Don't know"&!is.na(Southern$electricity_status_lab),]$electricity_status_lab),
                     SDT(Consentedmw[!is.na(Consentedmw$water_source),]$water_source, 
                         Northern[!is.na(Northern$water_source),]$water_source, 
                         Central[!is.na(Central$water_source),]$water_source,
                         Southern[!is.na(Southern$water_source),]$water_source))
  #Des.table <- Des.table[c(1:8,10,11,12,9,13,14,15,17,16, 18,20, 21,19, 22:nrow(Des.table)), ] 
  
  
  Des.table$Categories <- rownames(Des.table)
  Des.table$Variable <- c("Sex", rep(NA, 1),#length(unique(Consentedmw$Resp.Sex))-1
                          "Age", rep(NA, 1),
                          "Region", rep(NA, 2),
                          "Language", rep(NA, 4),
                          "Education", rep(NA, 3),
                          "Marriage", rep(NA, 4),
                          "Residence", rep(NA, 1),
                          "Electricity", rep(NA, 2),
                          "Water Source", rep(NA, 15))
  Des.table <- Des.table%>%relocate(Variable, Categories) %>%
    mutate(Categories = ifelse(Categories == 'Refuse1' | Categories == 'Refuse2' | Categories == 'Refuse3' | Categories == 'Refuse4', 'Refuse',Categories),
           Freq.1 = ifelse(Categories == 'Northern', 0, Freq.1),
           `%.1` = ifelse(Categories == 'Northern', 0 , `%.1`),
           Freq.2 = ifelse(Categories == 'Central', 0, Freq.2),
           `%.2` = ifelse(Categories == 'Central', 0 , `%.2`),
           Freq.3 = ifelse(Categories == 'Southern', 0, Freq.2),
           `%.3` = ifelse(Categories == 'Southern', 0 , `%.2`))
  rownames(Des.table) <- NULL
  Des.table$Variable[is.na(Des.table$Variable)] <- ""
  
  
  
  Des.table.1 <- Des.table %>%
    kbl(caption = "Sociodemographic Characteristics") %>%
    kable_classic(full_width = F, html_font = "Arial")%>%
    kable_styling(latex_options = 'hold_position')
  return(add_header_above(Des.table.1, c(" "=2, "Total " = 2, "Northern" = 2, "Central" = 2, "Southern" = 2)))
}
