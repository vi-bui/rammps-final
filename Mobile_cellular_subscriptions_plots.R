#_______________________________________________________________________________
#  Mobile cellular subscription plot
#  (From World Bank Data)
#  05/2023
#_______________________________________________________________________________

# Load libraries
pkgs <- c('purrr','plyr','tidyverse', 'ggplot2','reshape2')
lapply(pkgs, require, character.only = TRUE)

# reading in data
mobile_sub_data <- read.csv("data/mobile_phone_cellcular_subscriptions.csv", check.names=FALSE)
names(mobile_sub_data) <- str_replace_all(names(mobile_sub_data), " ", "_")
region_data <- read.csv("data/region.csv", check.names=FALSE)
names(region_data) <- str_replace_all(names(region_data), " ", "_")

# add Sub-Saharan Africa column from the country_code df to the mobile_sub_data df
select_region <- region_data %>%
  select(Region, Country_Code)

mobile_sub_data <- mobile_sub_data %>%
  left_join(select_region, by='Country_Code')

# select Sub-Saharan Africa region
mobile_sub_data_africa <- mobile_sub_data[mobile_sub_data$Region == "Sub-Saharan Africa",]
mobile_sub_data_africa <- mobile_sub_data_africa %>%
  select(-("1960":"1980"))
# mobile_sub_data_africa <- mobile_sub_data_africa %>%
#   drop_na()

# reshape data
mobile_long <- melt(mobile_sub_data_africa, id.vars = c("Country_Name", "Country_Code", "Indicator_Name", "Indicator_Code", "Region"))

# A named vector that represents the colors for each line (to be used in scale_color_manual)
cols <- c("MWI" = "purple", "BFA" = "blue", "COD" = "orange")

# change factor to numeric to plot the years on x-axis
mobile_long$variable <- as.numeric(levels(mobile_long$variable))[mobile_long$variable]

# plot figure
ggplot(mobile_long, aes(x=variable, y=value, group=Country_Code)) +
  geom_line(colour = "grey") +
  geom_line(data = mobile_long[mobile_long$Country_Code == "MWI",], aes(colour = "MWI")) +
  geom_line(data = mobile_long[mobile_long$Country_Code == "BFA",], aes(colour = "BFA"))+
  geom_line(data = mobile_long[mobile_long$Country_Code == "COD",], aes(colour = "COD")) +
  scale_color_manual(values = cols) +
  theme_bw() +
  labs(x='Years', y='Mobile cellular subscriptions (per 100 people)', colour='Country Code') +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020))

# don't need below
# remove row with all NAs
mobile_sub_data_africa <- mobile_sub_data_africa[rowSums(is.na(mobile_sub_data_africa)) != ncol(mobile_sub_data_africa), ]
mobile_sub_data_africa[is.na(mobile_sub_data_africa)] <- 0 # replace NAs with 0

str(mobile_sub_data_africa)
# work out means of all sub-sahran countries
mobile_means <- suppressWarnings(rbind(mobile_sub_data_africa, '1' = colMeans(mobile_sub_data_africa[, sapply(mobile_sub_data_africa, is.numeric)], na.rm = TRUE)))
# data_long <- gather(mobile_sub_data,Country_Name, Country_Code, Indicator_Name, Indicator_Code,  factor_key=TRUE)


mobile_means["Country_Name"][mobile_means["Country_Name"] == 0] <- "Sub-Saharan Africa Countries"
mobile_means["Country_Code"][mobile_means["Country_Code"] == 0] <- "Sub-Saharan Africa Countries"
# mobile_means["Indicator_Name"][mobile_means["Indicator_Name"] == 0] <- "Sub-Saharan Africa Countries"
# mobile_means["Indicator_Code"][mobile_means["Indicator_Code"] == 0] <- "Sub-Saharan Africa Countries"
# mobile_means["Region"][mobile_means["Region"] == 0] <- "Sub-Saharan Africa Countries"

write_csv(mobile_means, "mobile_means.csv")

# convert wide df to long format for dplyr
data_long <- melt(mobile_means, id.vars=c("Country_Name", "Country_Code", "Indicator_Name", "Indicator_Code", "Region"))
data_long[is.na(data_long)] <- 0 # replace NAs with 0


data_long_sub <- subset(data_long, subset=variable %in% c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 
                                         2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))


# get only Sub-Saharan Africa countries
# data_long_subset <- data_long[data_long$Region == "Sub-Saharan Africa",]

# ggplot(data = subset(data_long, Region == "Sub-Saharan Africa")) +
#   geom_line(aes(x=variable, y=value, group=ordered(Region, levels = "Sub-Saharan Africa"), color=Country_Code))

ggplot(data =subset(data_long_sub, Country_Code %in% c("BFA", "MWI", "COD", "WLD", "Sub-Saharan Africa Countries"))) +
  geom_line(aes(x=variable, y=value, color = Country_Code, group=ordered(Country_Code, levels = c("BFA", "MWI", "COD", "WLD","Sub-Saharan Africa Countries"))))


