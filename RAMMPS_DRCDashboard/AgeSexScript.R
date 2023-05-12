# age sex pyramids by month
function(Consented, NK, Kin, bymonth){
  # === Age and sex pyramid 
  
  if(bymonth == 'Overall'){
    pyramid.fo <- function(DATAFRAME) {
    matrixpyr.Tot <- DATAFRAME %>%
      group_by(Resp.Age.pyr, Resp.Sex) %>%
      dplyr::summarize(population = n()) %>%
      ungroup() %>%
      group_by(Resp.Sex) %>%
      dplyr::mutate(npersex = sum(population)) %>%
      ungroup() %>%
      mutate(population = ifelse(Resp.Sex == "Male", population*-1, population),
             relfreq = population/npersex*100, 
             Group = "Total")
    
    Plot <- ggplot(matrixpyr.Tot, aes(x = Resp.Age.pyr, y = relfreq, fill = Resp.Sex)) + 
      geom_bar(data =subset(matrixpyr.Tot,Resp.Sex == 'Female'), stat = "identity", color= 'black') +
      geom_bar(data =subset(matrixpyr.Tot,Resp.Sex == 'Male'), stat = "identity", color= 'black') +
      geom_text(aes(x = Resp.Age.pyr, y = relfreq, label = paste0(abs(round(relfreq, 1)),"%"))) +
      scale_x_discrete(labels = c('15-19','20-29','30-39','40-49','50-59','60-64')) +
      coord_flip() + scale_fill_brewer(palette = "Dark2") + 
      theme_minimal() + ylab('Relative Frequency of Respondents (%)') + xlab('Respondent Age') +
      theme(axis.text.x = element_text(angle = 90), legend.title = element_blank()) +
      ylim(-60,60)+
      scale_y_continuous(breaks = seq(-100,100,5),
                         labels = abs(seq(-100,100,5)))+
      labs(title = 'Age and Sex Distribution')
    return(Plot)
  }
  Fig <- ggarrange(pyramid.fo(Consented), 
                   pyramid.fo(Kin),
                   pyramid.fo(NK),ncol=1)
  return(Fig)
  }
  
  else if (bymonth %in% c('Aug-21','Sep-21','Oct-21','Nov-21','Dec-21','Jan-22','Feb-22','Mar-22','Apr-22','May-22','Jun-22','Jul-22','Aug-22')) {
    # by month
  pyrmonth <- function(DATAFRAME){
    matrixpyr.Tot <- DATAFRAME %>%
      filter(month.interview == bymonth) %>%
    group_by(Resp.Age.pyr, Resp.Sex) %>%
    dplyr::summarize(population = n()) %>%
    dplyr::mutate(npersex = sum(population)/2) %>%
    ungroup() %>%
    mutate(population = ifelse(Resp.Sex == "Male", population*-1, population),
           relfreq = population/sum(npersex)*100,
           Group = "Total")
  
  Overall.M <- ggplot(matrixpyr.Tot, aes(x = Resp.Age.pyr, y = relfreq, fill = Resp.Sex)) + 
    geom_bar(data =subset(matrixpyr.Tot,Resp.Sex == 'Female'), stat = "identity", position='dodge', color= 'black') +
    geom_bar(data =subset(matrixpyr.Tot,Resp.Sex == 'Male'), stat = "identity", position='dodge', color= 'black') +
    geom_text(aes(x = Resp.Age.pyr, y = relfreq, label = paste0(abs(round(relfreq, 1)),"%"))) +
    scale_x_discrete(labels = c('15-19','20-29','30-39','40-49','50-59','60-64')) +
    coord_flip() + scale_fill_brewer(palette = "Dark2") + 
    theme_minimal() + ylab('Relative Frequency of Respondents (%)') + xlab('Respondent Age') +
    theme(axis.text.x = element_text(angle = 90)) +
    ylim(-60,60)+
    scale_y_continuous(breaks = seq(-100,100,5),
                       labels = abs(seq(-100,100,5)))
  }
  # matrixpyr.Tot <- Kin %>%
  #   group_by(Resp.Age.pyr, Resp.Sex, month.interview) %>%
  #   dplyr::summarize(population = n()) %>%
  #   dplyr::mutate(npersex = sum(population)/2) %>%
  #   ungroup() %>%
  #   mutate(population = ifelse(Resp.Sex == "Male", population*-1, population),
  #          relfreq = population/sum(npersex)*100, Group = "Total")
  # 
  # Kin.M <- ggplot(matrixpyr.Tot, aes(x = Resp.Age.pyr, y = relfreq, fill = month.interview)) + 
  #   geom_bar(data =subset(matrixpyr.Tot,Resp.Sex == 'Female'), stat = "identity", position='dodge', color= 'black') +
  #   geom_bar(data =subset(matrixpyr.Tot,Resp.Sex == 'Male'), stat = "identity", position='dodge', color= 'black') +
  #   geom_text(aes(x = Resp.Age.pyr, y = relfreq, label = paste0(abs(round(relfreq, 1)),"%"))) +
  #   scale_x_discrete(labels = c('15-19','20-29','30-39','40-49','50-59','60-64')) +
  #   coord_flip() + scale_fill_brewer(palette = "Dark2") + 
  #   theme_minimal() + ylab('Relative Frequency of Respondents (%)') + xlab('Respondent Age') +
  #   theme(axis.text.x = element_text(angle = 90)) +
  #   labs(fill = 'Month of Interview')+
  #   ylim(-60,60)+
  #   scale_y_continuous(breaks = seq(-100,100,5),
  #                      labels = abs(seq(-100,100,5)))
  # Kin.M
  # 
  # matrixpyr.Tot <- NK %>%
  #   group_by(Resp.Age.pyr, Resp.Sex, month.interview) %>%
  #   dplyr::summarize(population = n()) %>%
  #   dplyr::mutate(npersex = sum(population)/2) %>%
  #   ungroup() %>%
  #   mutate(population = ifelse(Resp.Sex == "Male", population*-1, population),
  #          relfreq = population/sum(npersex)*100, Group = "Total")
  # 
  # NK.M <- ggplot(matrixpyr.Tot, aes(x = Resp.Age.pyr, y = relfreq, fill = month.interview)) + 
  #   geom_bar(data =subset(matrixpyr.Tot,Resp.Sex == 'Female'), stat = "identity", position='dodge', color= 'black') +
  #   geom_bar(data =subset(matrixpyr.Tot,Resp.Sex == 'Male'), stat = "identity", position='dodge', color= 'black') +
  #   geom_text(aes(x = Resp.Age.pyr, y = relfreq, label = paste0(abs(round(relfreq, 1)),"%"))) +
  #   scale_x_discrete(labels = c('15-19','20-29','30-39','40-49','50-59','60-64')) +
  #   coord_flip() + scale_fill_brewer(palette = "Dark2") + 
  #   theme_minimal() + ylab('Relative Frequency of Respondents (%)') + xlab('Respondent Age') +
  #   theme(axis.text.x = element_text(angle = 90)) +
  #   labs(fill = "Month of Interview")+
  #   ylim(-60,60)+
  #   scale_y_continuous(breaks = seq(-100,100,5),
  #                      labels = abs(seq(-100,100,5)))
  
  Fig2 <- ggarrange(pyrmonth(Consented), 
                    pyrmonth(Kin),
                    pyrmonth(NK),ncol=1)
  return(annotate_figure(Fig2, top = text_grob(paste0("Age and Sex Distribution, ",bymonth), 
                                        face = "bold", size = 12)))
  }
  # if (bymonth == FALSE) {
  #   return(annotate_figure(Fig,
  #                   top = text_grob('Age and sex distribution'),
  #                   bottom = text_grob("Top panel: Overall; Middle panel: Kinshasa; Bottom panel: Nord Kivu", color = "black")))
  #   }
  # else if (bymonth == TRUE) {
  #   return(annotate_figure(Fig2,
  #                 bottom = text_grob("Top panel=Overall; Middle panel=Kinshasa; Bottom panel=Nord Kivu", color = "black")))
  # }
  
}