# CATI Outcomes function 
function(dat.widemw, monthtoplot) {
  dat.widemw <- dat.widemw %>%
  mutate(Counter = 1, 
         month.interview = lubridate::floor_date(as.Date(time), 'month'),
        month.interview = case_when(month.interview == '2021-08-01'~'Aug-21',
                                    month.interview == '2021-09-01'~'Sep-21',
                                    month.interview == '2021-10-01'~'Oct-21',
                                    month.interview == '2021-11-01'~'Nov-21',
                                    month.interview == '2021-12-01'~'Dec-21',
                                    month.interview == '2022-01-01'~'Jan-22',
                                    month.interview == '2022-02-01'~'Feb-22',
                                    month.interview == '2022-03-01'~'Mar-22',
                                    month.interview == '2022-04-01'~'Apr-22',
                                    month.interview == '2022-05-01'~'May-22',
                                    month.interview == '2022-06-01'~'Jun-22',
                                    month.interview == '2022-07-01'~'Jul-22',
                                    month.interview == '2022-08-01'~'Aug-22',
                                    month.interview == '2022-09-01'~'Sep-22',
                                    month.interview == '2022-10-01'~'Oct-22',
                                    month.interview == '2022-11-01'~'Nov-22',
                                    month.interview == '2022-12-01'~'Dec-22',
                                    month.interview == '2023-01-01'~'Jan-23',
                                    month.interview == '2023-02-01'~'Feb-23',
                                    month.interview == '2023-03-01'~'Mar-23',
                                    month.interview == '2023-04-01'~'Apr-23',
                                    month.interview == '2023-05-01'~'May-23',
                                    month.interview == '2023-06-01'~'Jun-23'),
       month.interview = ordered(month.interview, c('Aug-21', 'Sep-21','Oct-21','Nov-21',
                                               'Dec-21','Jan-22', 'Feb-22','Mar-22',
                                               'Apr-22','May-22','Jun-22','Jul-22',
                                               'Aug-22', 'Sep-22','Oct-22','Nov-22',
                                               'Dec-22','Jan-23','Feb-23','Mar-23',
                                               'Apr-23','May-23','Jun-23')))
         
  wide.plot.data.T <- dat.widemw %>%
    dplyr::group_by(Outcome.FINAL, month.interview) %>%
    dplyr::summarise(total = sum(Counter)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(month.interview) %>%
    dplyr::mutate(rel.freq = (round(100 * total/sum(total), 2)),
                  Eligibility = case_when(Outcome.FINAL %in% c('COMP','PART') ~ 'Eligible',
                                          Outcome.FINAL %in% c('INEL','DEFER','NNU') ~ 'Ineligible',
                                          Outcome.FINAL %in% c('REFU','NNA','NR','LANG','PEND') ~ 'Eligibility unknown'),
                  Eligibility = factor(Eligibility, levels = c('Eligible','Ineligible','Eligibility unknown')),
                  Outcome.FINAL = factor(Outcome.FINAL, levels = c('COMP','PART','REFU','NNU','NNA','NR','INEL','LANG','DEFER','PEND','OTHER')))
 
    
    # --- Plot as a bar chart
    catioutcomestime <- function(monthtoplot){
      catiplotdf <- wide.plot.data.T %>%
        filter(month.interview == monthtoplot)
      
      ggplot(data = catiplotdf) +
        geom_bar(aes(fill=Eligibility, y=rel.freq, x=Outcome.FINAL),position="dodge", stat="identity",alpha = 0.8) +
        geom_bar(aes(color=Eligibility, y=rel.freq, x=Outcome.FINAL),alpha = 0,size = 1,position="dodge", stat="identity" ) +
        geom_text(aes(x=Outcome.FINAL, y=rel.freq, label=rel.freq), vjust=1, color="black", size=2.5)+
        scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#fb9a99","#a6cee3")) +
        scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666","#fb9a99","#a6cee3")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1, size = 7)) +
        xlab("")+ ylab("Percent of unique phone numbers")  +
        labs(title = 'CATI Outcomes')
      }
    return(catioutcomestime(monthtoplot))
    
    
}