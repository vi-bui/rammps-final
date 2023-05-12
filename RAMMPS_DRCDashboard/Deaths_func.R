# function for tallying up deaths
function(Consented){
  # === Table of Deceased sisters who died during pregnancy, childbirth or 2 months post-pregnancy
  # --- Bind cols of Whether sister died in pregnancy, childbirth or within 2 months from the end of pregnancy
  CD1.2.3 <- cbind(c(Consented$S_CD_Preg1, Consented$S_CD_Preg2, Consented$S_CD_Preg3, Consented$S_CD_Preg4),c(Consented$S_CD_Cbirth1, Consented$S_CD_Cbirth2, Consented$S_CD_Cbirth3, Consented$S_CD_Cbirth4), c(Consented$S_CD_2M1, Consented$S_CD_2M2, Consented$S_CD_2M3, Consented$S_CD_2M4))
  #  --- remove when all rows are NA and convert to data frame
  CD1.2.3 <- CD1.2.3[rowSums(is.na(CD1.2.3)) != ncol(CD1.2.3), ]
  CD1.2.3 <- as.data.frame(CD1.2.3)
  
  # --- Collapse each column and combine
  CD1.S <- CD1.2.3 %>% 
    mutate(Name = case_when(V1 == 1 ~ 'Yes',V1 == 2 | is.na(V1) ~ 'No', V1 == 3 ~ 'DK'))%>%
    dplyr::group_by(Name) %>% 
    dplyr::summarise(No1 = n()) %>%
    mutate(Variable = "Pregnancy")
  CD2.S <- CD1.2.3 %>% 
    mutate(Name = case_when(V2 == 1 ~ 'Yes',V2 == 2 | is.na(V2) ~ 'No',V2 == 3 ~ 'DK'))%>%
    dplyr::group_by(Name) %>% 
    dplyr::summarise(No1 = n()) %>%
    mutate(Variable = "Childbirth")
  CD3.S <- CD1.2.3 %>% 
    mutate(Name = case_when(V3 == 1 ~ 'Yes',V3 == 2 | is.na(V3) ~ 'No', V3 == 3 ~ 'DK'))%>%
    dplyr::group_by(Name) %>% 
    dplyr::summarise(No1 = n()) %>%
    mutate(Variable = "2 months of CB")
  
  CD1 <- cbind(CD1.S$No1, CD2.S$No1, c(CD3.S$No1))
  CD1 <- CD1[-4,]
  colnames(CD1) <- c("Pregnancy", "Child Birth", "2 months after")
  rownames(CD1) <- c("DK", "No", "Yes")
  CD1 <- as.data.frame(CD1)
  
  CD1tb <- CD1 %>%
    kbl() %>%
    kable_classic(full_width = F, html_font = "Arial")
  
  
  Consented$External.sis1 <- if_else(Consented$CD5_copy_1_1==1 |Consented$CD4_copy_1_1==1, "Y", "N")
  Consented$External.sis2 <- if_else(Consented$CD5_copy_1_2==1 |Consented$CD4_copy_1_2==1, "Y", "N")
  Consented$External.sis3 <- if_else(Consented$CD5_copy_1_3==1 |Consented$CD4_copy_1_3==1, "Y", "N")
  Consented$External.sis4 <- if_else(Consented$CD5_copy_1_4==1 |Consented$CD4_copy_1_4==1, "Y", "N")
  
  Consented$External.bro1 <- if_else(Consented$CD5_copy_bro_1==1 |Consented$CD4_copy_bro_1==1, "Y", "N")
  Consented$External.bro2 <- if_else(Consented$CD5_copy_bro_2==1 |Consented$CD4_copy_bro_2==1, "Y", "N")
  Consented$External.bro3 <- if_else(Consented$CD5_copy_bro_3==1 |Consented$CD4_copy_bro_3==1, "Y", "N")
  Consented$External.bro4 <- if_else(Consented$CD5_copy_bro_4==1 |Consented$CD4_copy_bro_4==1, "Y", "N")
  Consented$External.bro5 <- if_else(Consented$CD5_copy_bro_5==1 |Consented$CD4_copy_bro_5==1, "Y", "N")
  Consented$External.bro6 <- if_else(Consented$CD5_copy_bro_6==1 |Consented$CD4_copy_bro_6==1, "Y", "N")
  
  #### Maternal sis
  
  Consented$Maternal.sis1 <- if_else(Consented$S_CD_Preg1==1 |Consented$S_CD_Cbirth1==1 | Consented$S_CD_2M1==1, "Y", "N")
  Consented$Maternal.sis2 <- if_else(Consented$S_CD_Preg2==1 |Consented$S_CD_Cbirth2==1 | Consented$S_CD_2M2==1, "Y", "N")
  Consented$Maternal.sis3 <- if_else(Consented$S_CD_Preg3==1 |Consented$S_CD_Cbirth3==1 | Consented$S_CD_2M3==1, "Y", "N")
  Consented$Maternal.sis4 <- if_else(Consented$S_CD_Preg4==1 |Consented$S_CD_Cbirth4==1 | Consented$S_CD_2M4==1, "Y", "N")
  
  ###################
  
  # COVID symp
  Consented$Sis.Dead.C19.Symp1 <-  if_else(Consented$CD7_copy_1_1!="", "Y", "N")
  Consented$Sis.Dead.C19.Symp2 <-  if_else(Consented$CD7_copy_1_2!="", "Y", "N")
  Consented$Sis.Dead.C19.Symp3 <-  if_else(Consented$CD7_copy_1_3!="", "Y", "N")
  Consented$Sis.Dead.C19.Symp4 <-  if_else(Consented$CD7_copy_1_4!="", "Y", "N")
  
  Consented$Bro.Dead.C19.Symp1 <-  if_else(Consented$CD7_copy_bro_1!="", "Y", "N")
  Consented$Bro.Dead.C19.Symp2 <-  if_else(Consented$CD7_copy_bro_2!="", "Y", "N")
  Consented$Bro.Dead.C19.Symp3 <-  if_else(Consented$CD7_copy_bro_3!="", "Y", "N")
  Consented$Bro.Dead.C19.Symp4 <-  if_else(Consented$CD7_copy_bro_4!="", "Y", "N")
  Consented$Bro.Dead.C19.Symp5 <-  if_else(Consented$CD7_copy_bro_5!="", "Y", "N")
  Consented$Bro.Dead.C19.Symp6 <-  if_else(Consented$CD7_copy_bro_6!="", "Y", "N")
  
  ##########3
  matrix2019 <- array(NA, c(5,4,3))
  
  
  for (i in 2019:2021){
    
    matrix2019[1,,(i-2018)] <- c(length(subset(Consented, F.dead.Yr==i)$F.Dead=="2"),
                                 0,
                                 length(which(subset(Consented, F.dead.Yr==i)$F.Dead.External=="Y")),
                                 length(which(subset(Consented, F.dead.Yr==i)$F.Dead.COVID=="Very likely")|
                                          which(subset(Consented, F.dead.Yr==i)$F.Dead.COVID=="Somewhat likely")))
    
    matrix2019[2,,(i-2018)] <- c(length(subset(Consented, M.dead.Yr==i)$M.Dead=="2"),
                                 length(which(subset(Consented, M.dead.Yr==i)$M.Dead.mat=="Y")),
                                 length(which(subset(Consented, M.dead.Yr==i)$M.Dead.External=="Y")),
                                 length(which(subset(Consented, M.dead.Yr==i)$M.Dead.COVID=="Very likely")|
                                          which(subset(Consented, M.dead.Yr==i)$M.Dead.COVID=="Somewhat likely")))
    
    
    table(Consented$M.Dead.C19)
    
    matrix2019[3,,(i-2018)] <- c(sum(c(Consented$ssh4_yearDied_bro_1==i,
                                       Consented$ssh4_yearDied_bro_2==i,
                                       Consented$ssh4_yearDied_bro_3==i,
                                       Consented$ssh4_yearDied_bro_4==i,
                                       Consented$ssh4_yearDied_bro_5==i,
                                       Consented$ssh4_yearDied_bro_6==i), na.rm=T),
                                 0,
                                 length(which(subset(Consented, ssh4_yearDied_bro_1==i)$External.bro1=="Y"))+
                                   length(which(subset(Consented, ssh4_yearDied_bro_2==i)$External.bro2=="Y"))+
                                   length(which(subset(Consented, ssh4_yearDied_bro_3==i)$External.bro3=="Y"))+
                                   length(which(subset(Consented, ssh4_yearDied_bro_4==i)$External.bro4=="Y"))+
                                   length(which(subset(Consented, ssh4_yearDied_bro_5==i)$External.bro5=="Y"))+
                                   length(which(subset(Consented, ssh4_yearDied_bro_6==i)$External.bro6=="Y")), 
                                 length(which(subset(Consented, ssh4_yearDied_bro_1==i)$CD14_copy_bro_1<3))+
                                   length(which(subset(Consented, ssh4_yearDied_bro_2==i)$CD14_copy_bro_2<3))+
                                   length(which(subset(Consented, ssh4_yearDied_bro_3==i)$CD14_copy_bro_3<3))+
                                   length(which(subset(Consented, ssh4_yearDied_bro_4==i)$CD14_copy_bro_4<3))+
                                   length(which(subset(Consented, ssh4_yearDied_bro_5==i)$CD14_copy_bro_5<3))+
                                   length(which(subset(Consented, ssh4_yearDied_bro_6==i)$CD14_copy_bro_6<3)))
    
    matrix2019[4,,(i-2018)] <- c(sum(c(Consented$ssh4_yearDied_copy_1_1==i,
                                       Consented$ssh4_yearDied_copy_1_2==i,
                                       Consented$ssh4_yearDied_copy_1_3==i,
                                       Consented$ssh4_yearDied_copy_1_4==i), na.rm=T),
                                 length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$Maternal.sis1=="Y"))+
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$Maternal.sis2=="Y"))+
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$Maternal.sis3=="Y"))+
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$Maternal.sis4=="Y")),
                                 length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$External.sis1=="Y"))+
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$External.sis2=="Y"))+
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$External.sis3=="Y"))+
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$External.sis4=="Y")), 
                                 length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$CD14_copy_1_1<3))+
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$CD14_copy_1_2<3))+
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$CD14_copy_1_3<3))+
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$CD14_copy_1_4<3)) )
  }
  
  Consented$M.Dead.COVID
  
  for (i in 1:4) {
    for (j in 1:3){
      matrix2019[5,i,j] <- sum(matrix2019[1:4,i,j])
    }
  }
  
  mat.b <- array(NA, c(5,3,3))
  for (i in 1:3) {
    for (j in 1:3){
      mat.b[,i,j] <- round((matrix2019[,(i+1),j]/matrix2019[,1,j])*100,1) 
    }
  }
  
  
  matrix2019[,2:4,] <- mat.b[,1:3,]
  
  
  
  
  ############################
  
  matrix.D.set <- array(NA, c(5,5,3))
  
  
  for  (i in 2019:2021){
    matrix.D.set[1,,(i-2018)] <- c(length(subset(Consented, F.dead.Yr==i)$F.Dead=="2"),
                                   length(which(subset(Consented, F.dead.Yr==i)$F.Dead.setting.death=="Health Facility")),
                                   length(which(subset(Consented, F.dead.Yr==i)$F.Dead.setting.death=="Home")),
                                   length(which(subset(Consented, F.dead.Yr==i)$F.Dead.setting.death=="Other")),
                                   length(which(subset(Consented, F.dead.Yr==i)$F.Dead.setting.death=="DK")))
    
    matrix.D.set[2,,(i-2018)] <- c(length(subset(Consented, M.dead.Yr==i)$M.Dead=="2"),
                                   length(which(subset(Consented, M.dead.Yr==i)$M.Dead.setting.death=="Health Facility")),
                                   length(which(subset(Consented, M.dead.Yr==i)$M.Dead.setting.death=="Home")),
                                   length(which(subset(Consented, M.dead.Yr==i)$M.Dead.setting.death=="Other")),
                                   length(which(subset(Consented, M.dead.Yr==i)$M.Dead.setting.death=="DK")))
    
    matrix.D.set[3,,(i-2018)] <- c(sum(c(Consented$ssh4_yearDied_bro_1==i,
                                         Consented$ssh4_yearDied_bro_2==i,
                                         Consented$ssh4_yearDied_bro_3==i,
                                         Consented$ssh4_yearDied_bro_4==i,
                                         Consented$ssh4_yearDied_bro_5==i,
                                         Consented$ssh4_yearDied_bro_6==i), na.rm=T),
                                   length(which(subset(Consented, ssh4_yearDied_bro_1==i)$CD16_copy_bro_1==1))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_2==i)$CD16_copy_bro_2==1))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_3==i)$CD16_copy_bro_3==1))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_4==i)$CD16_copy_bro_4==1))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_5==i)$CD16_copy_bro_5==1))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_6==i)$CD16_copy_bro_6==1)),
                                   length(which(subset(Consented, ssh4_yearDied_bro_1==i)$CD16_copy_bro_1==2))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_2==i)$CD16_copy_bro_2==2))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_3==i)$CD16_copy_bro_3==2))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_4==i)$CD16_copy_bro_4==2))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_5==i)$CD16_copy_bro_5==2))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_6==i)$CD16_copy_bro_6==2)),
                                   length(which(subset(Consented, ssh4_yearDied_bro_1==i)$CD16_copy_bro_1==3))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_2==i)$CD16_copy_bro_2==3))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_3==i)$CD16_copy_bro_3==3))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_4==i)$CD16_copy_bro_4==3))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_5==i)$CD16_copy_bro_5==3))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_6==i)$CD16_copy_bro_6==3)),
                                   length(which(subset(Consented, ssh4_yearDied_bro_1==i)$CD16_copy_bro_1==5))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_2==i)$CD16_copy_bro_2==5))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_3==i)$CD16_copy_bro_3==5))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_4==i)$CD16_copy_bro_4==5))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_5==i)$CD16_copy_bro_5==5))+
                                     length(which(subset(Consented, ssh4_yearDied_bro_6==i)$CD16_copy_bro_6==5)))
    
    matrix.D.set[4,,(i-2018)] <- c(sum(c(Consented$ssh4_yearDied_copy_1_1==i,
                                         Consented$ssh4_yearDied_copy_1_2==i,
                                         Consented$ssh4_yearDied_copy_1_3==i,
                                         Consented$ssh4_yearDied_copy_1_4==i), na.rm=T),
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$CD16_copy_1_1==1))+
                                     length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$CD16_copy_1_2==1))+
                                     length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$CD16_copy_1_3==1))+
                                     length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$CD16_copy_1_4==1)),
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$CD16_copy_1_1==2))+
                                     length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$CD16_copy_1_2==2))+
                                     length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$CD16_copy_1_3==2))+
                                     length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$CD16_copy_1_4==2)),
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$CD16_copy_1_1==3))+
                                     length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$CD16_copy_1_2==3))+
                                     length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$CD16_copy_1_3==3))+
                                     length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$CD16_copy_1_4==3)),
                                   length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$CD16_copy_1_1==5))+
                                     length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$CD16_copy_1_2==5))+
                                     length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$CD16_copy_1_3==5))+
                                     length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$CD16_copy_1_4==5)))
    
  }
  
  
  
  for (i in 1:5) {
    for (j in 1:3){
      matrix.D.set[5,i,j] <- sum(matrix.D.set[1:4,i,j])
    }
  }
  
  
  mat.c <- array(NA, c(5,4,3))
  for (i in 1:4) {
    for (j in 1:3){
      mat.c[,i,j] <- round((matrix.D.set[,(i+1),j]/matrix.D.set[,1,j])*100,1) 
    }
  }
  
  matrix.D.set[,2:5,] <- mat.c[,1:4,]
  
  
  
  
  ##############################################
  
  
  # Place of burial
  matrix.Burial <- array(NA, c(5,5,3))
  
  
  for  (i in 2019:2021){
    matrix.Burial[1,,(i-2018)] <- c(length(subset(Consented, F.dead.Yr==i)$F.Dead=="2"),
                                    length(which(subset(Consented, F.dead.Yr==i)$F.Dead.Buried.plot=="Cemetery")),
                                    length(which(subset(Consented, F.dead.Yr==i)$F.Dead.Buried.plot=="Family plot")),
                                    length(which(subset(Consented, F.dead.Yr==i)$F.Dead.Buried.plot=="Was not buried")),
                                    length(which(subset(Consented, F.dead.Yr==i)$F.Dead.Buried.plot=="DK")))
    
    
    
    matrix.Burial[2,,(i-2018)] <- c(length(subset(Consented, M.dead.Yr==i)$M.Dead=="2"),
                                    length(which(subset(Consented, M.dead.Yr==i)$M.Dead.Buried.plot=="Cemetery")),
                                    length(which(subset(Consented, M.dead.Yr==i)$M.Dead.Buried.plot=="Family plot")),
                                    length(which(subset(Consented, M.dead.Yr==i)$M.Dead.Buried.plot=="Was not buried")),
                                    length(which(subset(Consented, M.dead.Yr==i)$M.Dead.Buried.plot=="DK")))
    
    matrix.Burial[3,,(i-2018)] <- c(sum(c(Consented$ssh4_yearDied_bro_1==i,
                                          Consented$ssh4_yearDied_bro_2==i,
                                          Consented$ssh4_yearDied_bro_3==i,
                                          Consented$ssh4_yearDied_bro_4==i,
                                          Consented$ssh4_yearDied_bro_5==i,
                                          Consented$ssh4_yearDied_bro_6==i), na.rm=T),
                                    length(which(subset(Consented, ssh4_yearDied_bro_1==i)$CD18_copy_bro_1==1))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_2==i)$CD18_copy_bro_2==1))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_3==i)$CD18_copy_bro_3==1))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_4==i)$CD18_copy_bro_4==1))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_5==i)$CD18_copy_bro_5==1))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_6==i)$CD18_copy_bro_6==1)),
                                    length(which(subset(Consented, ssh4_yearDied_bro_1==i)$CD18_copy_bro_1==2))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_2==i)$CD18_copy_bro_2==2))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_3==i)$CD18_copy_bro_3==2))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_4==i)$CD18_copy_bro_4==2))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_5==i)$CD18_copy_bro_5==2))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_6==i)$CD18_copy_bro_6==2)),
                                    length(which(subset(Consented, ssh4_yearDied_bro_1==i)$CD18_copy_bro_1==3))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_2==i)$CD18_copy_bro_2==3))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_3==i)$CD18_copy_bro_3==3))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_4==i)$CD18_copy_bro_4==3))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_5==i)$CD18_copy_bro_5==3))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_6==i)$CD18_copy_bro_6==3)),
                                    length(which(subset(Consented, ssh4_yearDied_bro_1==i)$CD18_copy_bro_1==5))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_2==i)$CD18_copy_bro_2==5))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_3==i)$CD18_copy_bro_3==5))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_4==i)$CD18_copy_bro_4==5))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_5==i)$CD18_copy_bro_5==5))+
                                      length(which(subset(Consented, ssh4_yearDied_bro_6==i)$CD18_copy_bro_6==5)))
    
    
    matrix.Burial[4,,(i-2018)] <- c(sum(c(Consented$ssh4_yearDied_copy_1_1==i,
                                          Consented$ssh4_yearDied_copy_1_2==i,
                                          Consented$ssh4_yearDied_copy_1_3==i,
                                          Consented$ssh4_yearDied_copy_1_4==i), na.rm=T),
                                    length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$CD18_copy_1_1==1))+
                                      length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$CD18_copy_1_2==1))+
                                      length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$CD18_copy_1_3==1))+
                                      length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$CD18_copy_1_4==1)),
                                    length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$CD18_copy_1_1==2))+
                                      length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$CD18_copy_1_2==2))+
                                      length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$CD18_copy_1_3==2))+
                                      length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$CD18_copy_1_4==2)),
                                    length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$CD18_copy_1_1==3))+
                                      length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$CD18_copy_1_2==3))+
                                      length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$CD18_copy_1_3==3))+
                                      length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$CD18_copy_1_4==3)),
                                    length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$CD18_copy_1_1==5))+
                                      length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$CD18_copy_1_2==5))+
                                      length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$CD18_copy_1_3==5))+
                                      length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$CD18_copy_1_4==5)))
    
  }
  
  
  
  
  for (i in 1:5) {
    for (j in 1:3){
      matrix.Burial[5,i,j] <- sum(matrix.Burial[1:4,i,j])
    }
  }
  
  
  mat.c <- array(NA, c(5,4,3))
  for (i in 1:4) {
    for (j in 1:3){
      mat.c[,i,j] <- round((matrix.Burial[,(i+1),j]/matrix.Burial[,1,j])*100,1) 
    }
  }
  
  matrix.Burial[,2:5,] <- mat.c[,1:4,]
  
  
  ################3
  
  # Local auth
  matrix.LA <- array(NA, c(5,4,3))
  
  
  for (i in 2019:2021){
    
    matrix.LA[1,,(i-2018)] <- c(length(subset(Consented, F.dead.Yr==i)$F.Dead=="2"),
                                length(which(subset(Consented, F.dead.Yr==i)$F.Dead.Local.auth=="Y")),
                                length(which(subset(Consented, F.dead.Yr==i)$F.Dead.Local.auth=="N")),
                                length(which(subset(Consented, F.dead.Yr==i)$F.Dead.Local.auth=="Dk")))
    
    
    matrix.LA[2,,(i-2018)] <- c(length(subset(Consented, M.dead.Yr==i)$M.Dead=="2"),
                                length(which(subset(Consented, M.dead.Yr==i)$M.Dead.Local.auth=="Y")),
                                length(which(subset(Consented, M.dead.Yr==i)$M.Dead.Local.auth=="N")),
                                length(which(subset(Consented, M.dead.Yr==i)$M.Dead.Local.auth=="Dk")))
    
    matrix.LA[3,,(i-2018)] <- c(sum(c(Consented$ssh4_yearDied_bro_1==i,
                                      Consented$ssh4_yearDied_bro_2==i,
                                      Consented$ssh4_yearDied_bro_3==i,
                                      Consented$ssh4_yearDied_bro_4==i,
                                      Consented$ssh4_yearDied_bro_5==i,
                                      Consented$ssh4_yearDied_bro_6==i), na.rm=T),
                                length(which(subset(Consented, ssh4_yearDied_bro_1==i)$CD20_copy_bro_1==1))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_2==i)$CD20_copy_bro_2==1))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_3==i)$CD20_copy_bro_3==1))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_4==i)$CD20_copy_bro_4==1))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_5==i)$CD20_copy_bro_5==1))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_6==i)$CD20_copy_bro_6==1)),
                                length(which(subset(Consented, ssh4_yearDied_bro_1==i)$CD20_copy_bro_1==2))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_2==i)$CD20_copy_bro_2==2))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_3==i)$CD20_copy_bro_3==2))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_4==i)$CD20_copy_bro_4==2))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_5==i)$CD20_copy_bro_5==2))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_6==i)$CD20_copy_bro_6==2)),
                                length(which(subset(Consented, ssh4_yearDied_bro_1==i)$CD20_copy_bro_1==3))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_2==i)$CD20_copy_bro_2==3))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_3==i)$CD20_copy_bro_3==3))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_4==i)$CD20_copy_bro_4==3))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_5==i)$CD20_copy_bro_5==3))+
                                  length(which(subset(Consented, ssh4_yearDied_bro_6==i)$CD20_copy_bro_6==3)))
    
    
    matrix.LA[4,,(i-2018)] <- c(sum(c(Consented$ssh4_yearDied_copy_1_1==i,
                                      Consented$ssh4_yearDied_copy_1_2==i,
                                      Consented$ssh4_yearDied_copy_1_3==i,
                                      Consented$ssh4_yearDied_copy_1_4==i), na.rm=T),
                                length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$CD20_copy_1_1==1))+
                                  length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$CD20_copy_1_2==1))+
                                  length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$CD20_copy_1_3==1))+
                                  length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$CD20_copy_1_4==1)),
                                length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$CD20_copy_1_1==2))+
                                  length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$CD20_copy_1_2==2))+
                                  length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$CD20_copy_1_3==2))+
                                  length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$CD20_copy_1_4==2)),
                                length(which(subset(Consented, ssh4_yearDied_copy_1_1==i)$CD20_copy_1_1==3))+
                                  length(which(subset(Consented, ssh4_yearDied_copy_1_2==i)$CD20_copy_1_2==3))+
                                  length(which(subset(Consented, ssh4_yearDied_copy_1_3==i)$CD20_copy_1_3==3))+
                                  length(which(subset(Consented, ssh4_yearDied_copy_1_4==i)$CD20_copy_1_4==3)))
    
    
  }
  
  for (i in 1:4) {
    for (j in 1:3){
      matrix.LA[5,i,j] <- sum(matrix.LA[1:4,i,j])
    }
  }
  
  mat.b <- array(NA, c(5,3,3))
  for (i in 1:3) {
    for (j in 1:3){
      mat.b[,i,j] <- round((matrix.LA[,(i+1),j]/matrix.LA[,1,j])*100,1) 
    }
  }
  
  
  matrix.LA[,2:4,] <- mat.b[,1:3,]
  
  rownames <- c("Fathers", "Mothers", "Brothers", "Sisters", "Total")
  
  rownames(matrix2019) <- rownames
  rownames(matrix.LA)<- rownames
  rownames(matrix.Burial)<- rownames
  rownames(matrix.D.set)<- rownames
  
  colnames (matrix2019) <- c("N", "%Maternal", "%External", "%COVID")
  colnames (matrix.LA) <- c("N", "%Yes", "%No", "%DK")
  colnames (matrix.Burial) <- c("N", "%Cemetery", "%Family Plot", "%Not buried", "%DK")
  colnames (matrix.D.set) <- c("N", "%Health Facility", "Home", "%Other", "%DK")
  
  
  matrixtots <-  as.data.frame(cbind(matrix2019[,1,1], matrix2019[,1,2], matrix2019[,1,3]))
  colnames (matrixtots) <- c("2019", "2020", "2021")
  
  matrix2019.df <-  as.data.frame(cbind(matrix2019[,-1,1], matrix2019[,-1,2], matrix2019[,-1,3]))
  colnames (matrix2019.df) <- rep(c("%Maternal", "%External", "%COVID"),3)
  
  matrix.LA.df <-  as.data.frame(cbind(matrix.LA[,-1,1], matrix.LA[,-1,2], matrix.LA[,-1,3]))
  colnames (matrix.LA.df) <- rep(c("%Yes", "%No", "%DK"),3)
  
  matrix.Burial.df <-  as.data.frame(cbind(matrix.Burial[,-1,1], matrix.Burial[,-1,2], matrix.Burial[,-1,3]))
  colnames (matrix.Burial.df) <- rep(c("%Cemetery", "%Family Plot", "%Not buried", "%DK"),3)
  
  
  matrix.D.set.df <-  as.data.frame(cbind(matrix.D.set[,-1,1], matrix.D.set[,-1,2], matrix.D.set[,-1,3]))
  colnames (matrix.D.set.df) <- rep(c("%Health Facility", "%Home", "%Other", "%DK"),3)
  
  return(list(matrix.D.set.df, matrixtots, matrix2019.df, matrix.LA.df, matrix.Burial.df))
}