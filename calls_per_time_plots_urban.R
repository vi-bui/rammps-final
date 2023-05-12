# MW

# create df grouped by Time_of_day
time_mw_urban <- time_mw %>%
  dplyr::group_by(Time_of_day, Outcome2, RuralUrban, .drop = FALSE) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%   
  dplyr::summarize(n = n()) 


time_mw_urban <- time_mw_urban %>%
  filter(Outcome2 == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  group_by(RuralUrban) %>%
  dplyr::mutate(perccum = cumsum(perc))

# get the number of calls from datamw dataset
datamw_urban <- datamw %>% 
  select(RuralUrban, starttime)  %>%
  drop_na(RuralUrban, starttime) %>%
  dplyr::group_by(RuralUrban) %>%
  mutate(starttime = mdy_hms(starttime), 
         Date = as.Date(starttime), 
         Time = format(starttime, "%T"))
# important df
datamw_urban$Time_of_day <- cut(x=hour(datamw_urban$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
# time_mw_sex$Calls <- c(table(datamw_sex$Time_of_day, datamw_sex$Resp.Sex))

calls_rural_mw <- datamw_urban %>%
  group_by(Time_of_day) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%  
  ungroup() %>%  
  group_by(RuralUrban, Time_of_day)  %>%
  summarise(Calls=n())

mw_time_urban <- left_join(time_mw_urban, calls_rural_mw)

ratio_cati_urban_mw <- max(mw_time_urban$Calls)/max(mw_time_urban$perccum)

calls.time.urban.mw <- ggplot(data = mw_time_urban) + 
  geom_line(aes(x = Time_of_day, y = perccum*ratio_cati_urban_mw, group=RuralUrban , colour=RuralUrban ), size = 0.8) + 
  geom_point(aes(x = Time_of_day, y =  perccum*ratio_cati_urban_mw, group=RuralUrban , colour=RuralUrban ), size = 2) +
  geom_bar(aes(y = Calls, x = Time_of_day, fill=RuralUrban), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  theme_bw() +
  scale_fill_manual(values = c("#66A61E","#D95F02")) +
  scale_color_manual(values = c("#66A61E","#D95F02")) +
  theme(axis.title.y = element_text( size=12),
        axis.title.y.right = element_text( size=12))+
  scale_y_continuous(
    name = "Phone calls made ",
    limits = c(0, 12500),
    sec.axis = sec_axis((~ . / ratio_cati_urban_mw), name ='Cumulative percentage of phone \n numbers with completed CATI'))
calls.time.urban.mw

# DRC
# create df grouped by Time_of_day
time_drc_urban <- time_drc %>%
  dplyr::group_by(Time_of_day, Outcome2, RuralUrban) %>%
  drop_na(RuralUrban)%>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>% 
  dplyr::summarize(n = n()) 

# get stats
time_drc_urban <- time_drc_urban %>%
  filter(Outcome2 == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  group_by(RuralUrban) %>%
  dplyr::mutate(perccum = cumsum(perc))


# get the number of calls from datamw dataset
calls_rural_drc <- datadrc %>%
  group_by(Time_of_day) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%  
  ungroup() %>%  
  group_by(RuralUrban, Time_of_day) %>%
  drop_na(RuralUrban) %>%
  summarise(Calls=n())


drc_time_urban <- left_join(time_drc_urban, calls_rural_drc)

ratio_cati_urban_drc <- max(drc_time_urban$Calls)/max(drc_time_urban$perccum)
legend_title <- "Place of residence"

calls.time.urban.drc <- ggplot(data = drc_time_urban) + 
  geom_line(aes(x = Time_of_day, y = perccum*ratio_cati_urban_drc, group=RuralUrban , colour=RuralUrban ), size = 0.8) + 
  geom_point(aes(x = Time_of_day, y =  perccum*ratio_cati_urban_drc, group=RuralUrban , colour=RuralUrban ), size = 2) +
  geom_bar(aes(y = Calls, x = Time_of_day, fill=RuralUrban), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  theme_bw() +
  scale_fill_manual(legend_title,values = c("#66A61E","#D95F02")) +
  scale_color_manual(legend_title,values = c("#66A61E","#D95F02")) +
  theme(axis.title.y = element_text( size=12),
        axis.title.y.right = element_text( size=12))+
  scale_y_continuous(
    name = "Phone calls made ",
    limits = c(0, 12500),
    sec.axis = sec_axis((~ . / ratio_cati_urban_drc), name ='Cumulative percentage of phone \n numbers with completed CATI',))
calls.time.urban.drc

# BF

# create df grouped by Time_of_day
time_bf_urban <- time_bf %>%
  dplyr::group_by(Time_of_day, Outcome,  urban_rural, .drop = FALSE) %>%
  drop_na(urban_rural)%>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%   
  dplyr::summarize(n = n()) %>%
  ungroup() %>%
  filter(Outcome == 'COMP')  %>%
  dplyr::mutate(totalcomp = sum(n),
                totalnums = nrow(bf.dat.wide),
                perc = round(n/totalnums*100,2),
                perccompletedpercallnum=round(n/totalcomp*100,1)) %>%
  group_by(urban_rural)%>%
  dplyr::mutate(perccum = cumsum(perc)) 

# get call data
calls_rural_bf <- bf.data %>% 
  select(urban_rural, instance_time)  %>%
  drop_na(urban_rural, instance_time) %>%
  dplyr::group_by(urban_rural) %>%
  mutate(starttime = instance_time)
# get number of calls based on the time of day
calls_rural_bf$Time_of_day <- cut(x=hour(calls_rural_bf$starttime), breaks = breaks, labels = labels, include.lowest=TRUE)
# time_bf_sex$Calls <- c(table(why$Time_of_day, why$gender))

# collapse the data based on urban_rural and Time_of_day
calls_rural_bf <- calls_rural_bf %>%
  group_by(Time_of_day) %>%
  mutate(Time_of_day =  factor(Time_of_day, levels = days_ordered)) %>% # rearrange plot to match 
  arrange(Time_of_day) %>%  
  ungroup() %>%  
  group_by(urban_rural, Time_of_day) %>%
  summarise(Calls=n())


bf_time_urban <- left_join(time_bf_urban, calls_rural_bf)

ratio_cati_urban_bf <- max(bf_time_urban$Calls)/max(bf_time_urban$perccum)

calls.time.urban.bf <- ggplot(data = bf_time_urban) + 
  geom_line(aes(x = Time_of_day, y = perccum*ratio_cati_urban_bf, group=urban_rural , colour=urban_rural ), size = 0.8) + 
  geom_point(aes(x = Time_of_day, y =  perccum*ratio_cati_urban_bf, group=urban_rural , colour=urban_rural ), size = 2) +
  geom_bar(aes(y = Calls, x = Time_of_day, fill=urban_rural), position= 'dodge',alpha = 0.75,stat = 'identity')+
  xlab('Call attempt order') +
  theme_bw() +
  scale_fill_manual(values = c("#66A61E","#D95F02")) +
  scale_color_manual(values = c("#66A61E","#D95F02")) +
  theme(axis.title.y = element_text( size=12),
        axis.title.y.right = element_text( size=12))+
  scale_y_continuous(
    name = "Phone calls made ",
    limits = c(0, 12500),
    sec.axis = sec_axis((~ . / ratio_cati_urban_bf), name ='Cumulative percentage of phone \n numbers with completed CATI'))
calls.time.urban.bf

# arrange the three plots into 4 quadrants
plots_time_urban <- plot_grid(calls.time.urban.mw + theme(legend.position="none", axis.text=element_text(size=8)), #remove legends first before combining legends
                              calls.time.urban.drc+ theme(legend.position="none", axis.text=element_text(size=8),
                                                      
                                                      axis.ticks.y = element_blank(), # removes the x-axis scales
                                                      axis.title.y = element_blank(), # removes the x-axis title
                                                      axis.ticks.y.right = element_blank(), # removes the right x-axis scales
                                                      axis.title.y.right = element_blank()),
                              calls.time.urban.bf + theme(legend.position="none", axis.text=element_text(size=8),
                                                    
                                                      axis.ticks.y = element_blank(), # removes the x-axis scales
                                                      axis.title.y = element_blank(), # removes the x-axis title
                                                      axis.ticks.y.right = element_blank(), # removes the right x-axis scales
                                                      axis.title.y.right = element_blank()), 
                            labels = c("MW", "DRC", "BF"), #Title for each graph
                            label_size = 10
)

# extract the legend from Malawi plots
legend_time_urban <- get_legend(
  calls.time.urban.drc + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.65, 5),
          legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size=15))
)

# Adding subtitle with the eligibility unknown outcomes
sublegend_time_urban<- add_sub(plots_time_urban, "Early Morning = 5:00 - 8:59 \n Late Morning = 9:00 - 11:59 \n Afternoon = 12:00 - 18:59 \n Evening = 19:00 - 23:59 \n Night = 00:00 - 4:59", x = 0.85, y=3, size=11)

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(sublegend_time_urban, legend_time_urban, ncol = 1, rel_heights = c(1, .1))

