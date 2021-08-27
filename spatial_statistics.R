#--------------------------------------------------------------------------------
#Code example for Spatial Statistics 
#--------------------------------------------------------------------------------
lsoa_confirmed_day<- readRDS("lsoa_confirmed_day.rds")
#--------------------------------------------------------------------------------
#filter the data to a certain date to get the previous two week cumulative sum

twoweeksnapshot <- lsoa_confirmed_day %>%
  group_by(lsoa) %>% 
filter(confirmeddate==("2020-04-26"))

twoweeksnapshot<- twoweeksnapshot %>% 
select(lsoa, twoweekroll_per_100000, lsoa_pop)

#--------------------------------------------------------------------------------
#Read in spatial data *edited path to remove*

LSOA <- rgdal::readOGR("xxx",
                       "BNSSG_LSOA", stringsAsFactors = FALSE)


#join on suspectedperlsoa onto spatialdata


LSOA@data <- LSOA@data %>%
  left_join(twoweeksnapshot, by = c("lsoa11cd" = "lsoa"))


#works like joining any two dataframes
#should appear in the data slot
glimpse(LSOA@data)

table(LSOA@data$twoweekroll_per_100000, useNA = 'always')


my.palette <- brewer.pal(n = 7, name = "BuPu")

my.palette<- rev(brewer.pal(n = 7, name = "RdBu"))

brewer.pal(n = 5, name = "Spectral")
display.brewer.pal(n = 5, name = 'Spectral')

my.palette<- rev(brewer.pal(n = 7, name = "Spectral"))


# Plot a map of the number of covid cases
spplot(LSOA, zcol = "twoweekroll_per_100000", main = "14 day cumulative sum of confirmed COVID-19 cases on 26th April 2020 per 100,000", sub = "14 day cumulative sum of confirmed COVID-19 cases per 100,000", 
       col = "transparent", col.regions = my.palette, cuts=6)
#--------------------------------------------------------------------------------













#------------------------------------------------------------------------
#Potential spatial correlation in centre
#Determining if there is spatial correlation
#first have to determine which regions are neighbours/build a neighbour structure
#poly2nb() - polygons to neighbours
#Gives nb object
#compute test stat and run significance test H0 - there is no spatial correlation 

install.packages("spdep")
library(spdep)


# Make neighbor list
lsoa_nb <- poly2nb(LSOA)

# Get center points of each borough
lsoa_centers <- coordinates(LSOA)

# Show the connections
plot(LSOA); plot(lsoa_nb, lsoa_centers, add = TRUE)

# Map suspect_per_100000
spplot(LSOA, zcol = "suspect_per_100000")

# Run a Moran I MC test on % Remain
moran.mc(
  LSOA$suspect_per_100000, 
  nb2listw(lsoa_nb), 
  nsim = 999
)

#Reject null hypothesis of no spatial correlation. There is spatial correlation for suspected covid.
#--------------------------------------------------------------------------------
#Task - to map the "Standardized Morbidity Ratio", or SMR.

twoweeksnapshot<- twoweeksnapshot %>% 
  select(lsoa, twoweekroll, lsoa_pop)


LSOA <- rgdal::readOGR("S:/Finance/Shared Area/BNSSG - BI/10 Infrastructure/10 - R/Data/gis/BASEMAP",
                       "BNSSG_LSOA", stringsAsFactors = FALSE)


#join on suspectedperlsoa onto spatialdata



LSOA@data <- LSOA@data %>%
  left_join(twoweeksnapshot, by = c("lsoa11cd" = "lsoa"))


# Map the number of suspected covid cases (total suspected on that date)
spplot(LSOA, "twoweekroll", main = "14 day cumulative sum of confirmed COVID-19 cases on 26th April 2020", col="transparent")

# Determine overall incidence of covid (within 2 week period)
oi <- sum(LSOA$twoweekroll) / sum(LSOA$lsoa_pop)
oi

# Calculate the expected number for each borough
LSOA$covid_EXP <- LSOA$lsoa_pop * oi

# Calculate the ratio of OBServed to EXPected
LSOA$covid_SMR <- LSOA$twoweekroll / LSOA$covid_EXP

my.palette<- rev(brewer.pal(n = 7, name = "Spectral"))



# Map the SMR
spplot(LSOA, "covid_SMR", col='transparent', main = "SMR 14 day cumulative confirmed COVID-19 cases (26th April 2020)", col.regions = my.palette, cuts=6)


#allows you to compare relatively what areas higher than average versus lower than average in a two week period.
#--------------------------------------------------------------------------------
install.packages("epitools")
library(epitools)
# Get CI from binomial distribution
covidconfirmed_ci <- binom.exact(LSOA$twoweekroll, LSOA$lsoa_pop)

# Add borough names
covidconfirmed_ci$lsoa11nm <- LSOA$lsoa11nm

# Calculate London rate, then compute SMR
r <- sum(LSOA$twoweekroll) / sum(LSOA$lsoa_pop)
covidconfirmed_ci$SMR <- covidconfirmed_ci$proportion / r

# Subset the high SMR data
covidsus_high <- covidconfirmed_ci[covidconfirmed_ci$SMR > 5, ]

# Plot estimates with CIs
ggplot(covidsus_high, aes(x = lsoa11nm, y = proportion / r,
                          ymin = lower / r, ymax = upper / r)) +
  geom_pointrange() +
  ylab("SMR") +
  xlab("LSOA Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------------------------------------------------------------------




#--------------------------------------------------------------------------------

#Exceedence probabilities
#Probability that a value is over a certain threshold
#E.g. Alert level when probability of SMR doubling is over 0.95.
#Map exceedence probabilities for a threshold and use colour to highlight probabilities close to 1.

# Probability of a binomial exceeding a multiple
binom.exceed <- function(observed, population, expected, e){
  1 - pbinom(e * expected, population, prob = observed / population)
}

# Compute P(rate > 10)
LSOA$covid_2 <- binom.exceed(
  observed = LSOA$twoweekroll,
  population = LSOA$lsoa_pop,
  expected = LSOA$covid_EXP,
  e = 5)

#optional - take out the names/codes for those with SMR exceeding 2
covidsus_high <- covidconfirmed_ci[covidconfirmed_ci$SMR > 5, ]
#write.csv(covidsus_high, "SMRover2")


# Use a 50-color palette that only starts changing at around 0.9
pal <- c(
  rep("#b4f0c8", 40),
  colorRampPalette(c("#b4f0c8", "orange"))(5), 
  colorRampPalette(c("orange", "red"))(5)
)

# Plot the P(rate > 2) map
spplot(LSOA, "covid_2", col.regions = pal, at = seq(0, 1, len = 50), col='transparent', main = "LSOAs at more than 5x the expected rate with associated probability")

#This identifies LSOAS at more than twice the expected rate with associated probability


