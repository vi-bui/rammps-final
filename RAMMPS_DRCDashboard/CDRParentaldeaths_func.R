# CDR func
# Load/install packages 
# pkgs <- c('epitools','plyr','ggpubr','tidyverse', 'kableExtra', 'tidyselect', 
#           'gridExtra','reshape','magick','webshot')
# lapply(pkgs, require, character.only = TRUE)
#lib.loc = 'C:/Users/KellyMcCain/Documents/R/win-library/4.1/',
library(epitools)
function(Consented, Kin, NK, tableyes){
  # === Caluclate CDR (num = number of deaths; den = Number living and who died)
  num <- (sum(Consented$U5Death, na.rm = T)+ sum(Consented$O5Death, na.rm = T))
  den <- ((sum(Consented$U5, na.rm = T)*3) + (sum(Consented$U5Death, na.rm = T)*1.5) +
            (sum(Consented$O5, na.rm = T)*3) + (sum(Consented$O5Death, na.rm = T)*1.5))/12
  
  #num/den
  
  # --- Caluclate CDR in NK
  num.NK <- (sum(NK$U5Death, na.rm = T)+ sum(NK$O5Death, na.rm = T))
  den.NK <- ((sum(NK$U5, na.rm = T)*3) + (sum(NK$U5Death, na.rm = T)*1.5) +
               (sum(NK$O5, na.rm = T)*3) + (sum(NK$O5Death, na.rm = T)*1.5))/12
  # --- Caluclate CDR in Kinshasa
  num.Kin <- (sum(Kin$U5Death, na.rm = T)+ sum(Kin$O5Death, na.rm = T))
  den.Kin <- ((sum(Kin$U5, na.rm = T)*3) + (sum(Kin$U5Death, na.rm = T)*1.5) +
                (sum(Kin$O5, na.rm = T)*3) + (sum(Kin$O5Death, na.rm = T)*1.5))/12
  # --- CDR matrix
  CDR.matrix <- matrix(NA, 3,3)
  CDR.matrix[1,]<-(pois.exact(num, den,  conf.level = 0.95)[3:5]*1000)[1,]; CDR.matrix <- matrix(unlist(CDR.matrix), nrow=3, ncol=3)
  CDR.matrix[2,] <- (pois.exact(num.NK, den.NK,  conf.level = 0.95)[3:5]*1000)[1,]; CDR.matrix <- matrix(unlist(CDR.matrix), nrow=3, ncol=3)
  CDR.matrix[3,] <- (pois.exact(num.Kin, den.Kin,  conf.level = 0.95)[3:5]*1000)[1,]; CDR.matrix <- matrix(unlist(CDR.matrix), nrow=3, ncol=3)
  rownames(CDR.matrix) <- c("CDR total", "CDR North Kivu", "CDR Kinshasa"); colnames(CDR.matrix) <- c("Point Est", "Lower", "Higher")
  CDR.matrix <- round(CDR.matrix, 1)
  
  # --- Using tidyverse knitr, present the table
  CDR <- CDR.matrix %>%
    as.data.frame() %>%
    mutate(Location = row.names(CDR.matrix)) %>%
    select(Location, everything())
    # kbl(caption = "") %>%
    # kable_classic(full_width = F, html_font = "Arial")%>%
    # kable_styling(latex_options = 'hold_position')
  CDRgrob <- gridExtra::tableGrob(CDR, theme=ttheme_minimal(), rows=NULL)
  
  
  # === Populate a matrix of deaths among siblings  
  matrix <- matrix(NA, 4, 4)
  matrix[,1] <-c(sum(Consented$Sis1.Dead, na.rm = T),
                 sum(Consented$Bro1.Dead, na.rm = T),
                 length(which(Consented$M.Dead==2)),
                 length(which(Consented$F.Dead==2)))
  
  matrix[,2] <-c(c(sum(c(Consented$ssh4_yearDied_copy_1_1==2019,Consented$ssh4_yearDied_copy_1_1==2020,Consented$ssh4_yearDied_copy_1_1==2021,
                         Consented$ssh4_yearDied_copy_1_2==2019,Consented$ssh4_yearDied_copy_1_2==2020,Consented$ssh4_yearDied_copy_1_2==2021,
                         Consented$ssh4_yearDied_copy_1_3==2019,Consented$ssh4_yearDied_copy_1_3==2020,Consented$ssh4_yearDied_copy_1_3==2021,
                         Consented$ssh4_yearDied_copy_1_4==2019,Consented$ssh4_yearDied_copy_1_4==2020,Consented$ssh4_yearDied_copy_1_4==2021), na.rm=T)),
                 c(sum(c(Consented$ssh4_yearDied_bro_1==2019,Consented$ssh4_yearDied_bro_1==2020,Consented$ssh4_yearDied_bro_1==2021,
                         Consented$ssh4_yearDied_bro_2==2019,Consented$ssh4_yearDied_bro_2==2020,Consented$ssh4_yearDied_bro_2==2021,
                         Consented$ssh4_yearDied_bro_3==2019,Consented$ssh4_yearDied_bro_3==2020,Consented$ssh4_yearDied_bro_3==2021,
                         Consented$ssh4_yearDied_bro_4==2019,Consented$ssh4_yearDied_bro_4==2020,Consented$ssh4_yearDied_bro_4==2021,
                         Consented$ssh4_yearDied_bro_5==2019,Consented$ssh4_yearDied_bro_5==2020,Consented$ssh4_yearDied_bro_5==2021,
                         Consented$ssh4_yearDied_bro_6==2019,Consented$ssh4_yearDied_bro_6==2020,Consented$ssh4_yearDied_bro_6==2021), na.rm=T)),
                 length(which(Consented$M.dead.Yr>2018 & Consented$M.dead.Yr<2022)),
                 length(which(Consented$F.dead.Yr>2018 & Consented$F.dead.Yr<2022)))
  
  matrix[,3] <- c(table(is.na((Consented[which(Consented$M.Dead=="2"),])$M.dead.Age))[2],table(is.na((Consented[which(Consented$F.Dead=="2"),])$F.dead.Age))[2])
  
  matrix[,4] <- c(table(is.na((Consented[which(Consented$M.Dead==2 & Consented$M.dead.Yr>2018 & Consented$M.dead.Yr<2022),])$M.dead.Age))[2],table(is.na((Consented[which(Consented$F.Dead==2 & Consented$F.dead.Yr>2018 & Consented$F.dead.Yr<2022),])$F.dead.Age))[2])
  
  rownames(matrix) <- c("Sisters", "Brothers","Mothers", "Fathers")
  colnames(matrix) <- c("Deaths", "Deaths since 2019", "Missing age at death", "Missing age at death (since 2019)")
  
  Sibling.parent <- matrix
  Sibling.parenttbl <- Sibling.parent[c(3,4),] %>%
    as.data.frame()
  Sibling.parenttbl$Group <- row.names(Sibling.parenttbl)
  Sibling.parenttbl <- Sibling.parenttbl %>% select(Group, everything())
    # kbl() %>%
    # kable_classic(full_width = F, html_font = "Arial") %>% as.data.frame()
  sibparent <- gridExtra::tableGrob(Sibling.parenttbl, theme=ttheme_minimal(), rows=NULL)
  
  tables <- ggarrange(CDRgrob, sibparent, ncol =1)
  if (tableyes == F){
  return(annotate_figure(tables, top = text_grob("Crude death rates (top) \n Reported parental deaths (bottom)", face = 'bold')))
  }
  else if (tableyes == T){
    return(Sibling.parent)
  }

  }