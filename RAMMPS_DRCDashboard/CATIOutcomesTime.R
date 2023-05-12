# CATI Outcomes function 
function(dat.wide, monthtoplot) {
  dat.wide <- dat.wide %>%
  dplyr::mutate(Counter = 1,
                month.interview = lubridate::floor_date(as.Date(time), 'month'),#month(Date.Interview),
                  month.interview = case_when(month.interview == '2022-01-01'~ 'Jan-22',
                                              month.interview == '2022-02-01'~ 'Feb-22',
                                              month.interview == '2022-03-01'~ 'Mar-22',
                                              month.interview == '2022-04-01'~'Apr-22',
                                              month.interview == '2022-05-01'~'May-22',
                                              month.interview == '2022-06-01'~'Jun-22',
                                              month.interview == '2022-07-01'~'Jul-22',
                                              month.interview == '2022-08-01'~'Aug-22',
                                              month.interview == '2021-08-01'~'Aug-21',
                                              month.interview == '2021-09-01'~'Sep-21',
                                              month.interview == '2021-10-01'~'Oct-21',
                                              month.interview == '2021-11-01'~'Nov-21',
                                              month.interview == '2021-12-01'~'Dec-21'))
         
    # === Plot of the the Unique phone number outcomes
    wide.plot.data.S <- dat.wide %>%
      dplyr::group_by(Outcome.FINAL, Source.1, month.interview) %>%
      dplyr::summarise(total = sum(Counter)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Source.1, month.interview) %>%
      dplyr::mutate(rel.freq = (round(100 * total/sum(total), 2))) %>% as.data.frame()
    
    wide.plot.data.T <- dat.wide %>%
      dplyr::group_by(Outcome.FINAL, month.interview) %>%
      dplyr::summarise(total = sum(Counter)) %>%
      dplyr::group_by(month.interview) %>%
      dplyr::mutate(rel.freq = (round(100 * total/sum(total), 2))) %>% as.data.frame()
    
    wide.plot.data.T$Source.1 <- "Overall"
    wide.plot.data.T <- rbind(wide.plot.data.T, wide.plot.data.S)
    wide.plot.data.T$Source.1 <- factor(wide.plot.data.T$Source.1, levels = c('Overall','IVR group 1','IVR group 2','Feroxus'))
    
    
    wide.plot.data.T <- wide.plot.data.T %>%
      mutate(Eligibility = case_when(Outcome.FINAL %in% c('COMP','PART') ~ 'Eligible',
                                     Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                     Outcome.FINAL %in% c('REFU','NNA','NR','LANG','PEND') ~ 'Eligibility unknown'),
             Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
             Outcome.FINAL = factor(Outcome.FINAL, levels = c('COMP','PART','REFU','NNU','NNA','NR','INEL','LANG','DEFER','PEND','OTHER')))
    
    # --- Plot as a bar chart
    catioutcomestime <- function(monthtoplot){
      catiplotdf <- wide.plot.data.T %>%
        filter(month.interview == monthtoplot) %>%
        mutate(colorhex = case_when(Outcome.FINAL == 'COMP' ~ "#1B9E77",
                                    Outcome.FINAL == 'REFU' ~ "#D95F02",
                                    Outcome.FINAL == 'NR' ~ "#7570B3",
                                    Outcome.FINAL == 'NNU' ~ "#E7298A",
                                    Outcome.FINAL == 'INEL' ~ "#66A61E",
                                    Outcome.FINAL == 'LANG' ~ "#E6AB02",
                                    Outcome.FINAL == 'NNA' ~ "#A6761D",
                                    Outcome.FINAL == 'PEND' ~ "#666666",
                                    Outcome.FINAL == 'PART' ~ "#fb9a99",
                                    Outcome.FINAL == 'DEFER' ~ "#a6cee3"))
      
      ggplot(data = catiplotdf) +
        geom_bar(aes(fill=Eligibility, y=rel.freq, x=Outcome.FINAL),position="dodge", stat="identity",alpha = 0.8) +
        geom_bar(aes(color=Eligibility, y=rel.freq, x=Outcome.FINAL),alpha = 0,size = 1,position="dodge", stat="identity" ) +
        geom_text(aes(x=Outcome.FINAL, y=rel.freq, label=rel.freq), vjust=1, color="black", size=2.5)+
        scale_fill_brewer(palette = 'Dark2') +
        scale_color_brewer(palette = 'Dark2') +
        # geom_bar(position="dodge", stat="identity",color="black") +
        # geom_text(aes(x=Outcome.FINAL, y=rel.freq, label=rel.freq), vjust=1, color="black", size=2.5)+
        #scale_fill_identity() +
        #scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#66C2A5")) +
        facet_wrap(~Source.1) +
        theme_minimal() +
        theme(legend.position="none",
              axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1, size = 7)) +
        xlab("")+ ylab("Percent of unique phone numbers") +
        labs(caption=paste0("Feroxus = ",sum(wide.plot.data.T$total[wide.plot.data.T$Source.1=='Feroxus']), "; IVR group 1 = ",sum(wide.plot.data.T$total[wide.plot.data.T$Source.1=='IVR group 1']), "; IVR group 2 = ",sum(wide.plot.data.T$total[wide.plot.data.T$Source.1=='IVR group 2']),"; Overall = ",sum(wide.plot.data.T$total[wide.plot.data.T$Source.1=='Overall'])))
    }
    return(catioutcomestime(monthtoplot))
  
}