# Weighting function 
Weight.Function <- function(data.weight){
  # Specify the population distributions of the variables here
  if(data.weight$Resp.Region==1){
    Education2 <- c(19.2,57.7, 23.1)
    Age2 <- c(63.4, 36.6)
    targets <- list(Education2, Age2)
    names(targets) <- c("Education2", "Age2")
    names(targets$Education2) <- levels(data.weight$Education2)
    names(targets$Age2) <- levels(data.weight$Age2)
    # Create dataframe from input
    dat1 <- data.weight
    # Get it in the correct format - as numric vectors
    dat1$Education2 <- as.numeric(as.factor(dat1$Education2))
    dat1$Age2 <- as.numeric(as.factor(dat1$Age2))
  }
  if(data.weight$Resp.Region==2){
    Education2 <- c(43.7, 39.0, 17.3)
    Age2 <- c(64.3, 35.7)
    Composite2 <- c((100-63.1), 63.1)
    UR <- c(33.3, 66.6)
    targets <- list(Education2, Age2, Composite2, UR)
    names(targets) <- c("Education2", "Age2", "Composite2", "UR")
    names(targets$Education2) <- levels(data.weight$Education2)
    names(targets$Age2) <- levels(data.weight$Age2)
    names(targets$Composite2) <- levels(data.weight$Composite2)
    names(targets$UR) <- levels(data.weight$UR)
    # Create dataframe from input
    dat1 <- data.weight
    # Get it in the correct format - as numric vectors
    dat1$Education2 <- as.numeric(as.factor(dat1$Education2))
    dat1$Age2 <- as.numeric(as.factor(dat1$Age2))
    dat1$Composite2 <- as.numeric(as.factor(dat1$Composite2))
    dat1$UR <- as.numeric(as.factor(dat1$UR))
  }
  # Create caseid as a seq of numbers to the length of the df
  dat1$caseid <- 1:length(dat1$Resp.Sex)
  dat1 <- as.data.frame(dat1)
  # obtain raked weights  with an upper limit of 4 for the post weights
  outsave <- anesrake(targets, dat1, caseid = dat1$caseid,
                      cap = 4, choosemethod = "total",
                      type = "pctlim", pctlim = .05 , nlim = 5,
                      iterate = TRUE , force1 = TRUE)
  # Save it as a variable in the a new outputted dataset
  dat1$weightvec  <- unlist(outsave[1])
  return(dat1)
}