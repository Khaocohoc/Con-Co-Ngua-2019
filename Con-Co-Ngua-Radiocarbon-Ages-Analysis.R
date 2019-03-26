library(tidyverse)
library(here)
library(readxl)
library(Bchron)

# Reading data from dates
## Da But site

dates_Dabut <- read_excel(here("data", "dates.xlsx"), sheet = 'Dabut')
dates_Dabut <- dates_Dabut[order(dates_Dabut$ages),]

### Ages 1

ages_Dabut = BchronCalibrate(
  ages = dates_Dabut$ages,
  ageSds = dates_Dabut$ageSds,
  positions = dates_Dabut$position,
  calCurves = rep('intcal13', nrow(dates_Dabut))
)

summary(ages_Dabut)

plot(ages_Dabut, withPositions = TRUE)
ages_Dabut_samples = sampleAges(ages_Dabut)
apply(ages_Dabut_samples, 2, quantile, prob=c(0.025,0.975))
apply(ages_Dabut_samples, 2, quantile, prob=c(0.5))

# Reading data from dates
## Lang Cong site

dates_Langcong <- read_excel(here("data", "dates.xlsx"), sheet = 'Langcong')
dates_Langcong <- dates_Langcong[order(dates_Langcong$ages),]

### Ages 1
ages_Langcong = BchronCalibrate(
  ages = dates_Langcong$ages,
  ageSds = dates_Langcong$ageSds,
  positions = dates_Langcong$position,
  calCurves = rep('intcal13', nrow(dates_Langcong))
  )
  
summary(ages_Langcong)
plot(ages_Langcong, withPositions = TRUE)
Langcong_age_samples = sampleAges(ages_Langcong)
apply(Langcong_age_samples, 2, quantile, prob=c(0.025,0.975))
apply(Langcong_age_samples, 2, quantile, prob=c(0.5))

# Reading data from dates
## Ban Thuy site
dates_Banthuy <- read_excel(here("data", "dates.xlsx"), sheet = 'Banthuy')
dates_Banthuy <- dates_Banthuy[order(dates_Banthuy$ages),]

### Ages 1

ages_Banthuy = BchronCalibrate(
  ages = dates_Banthuy$ages,
  ageSds = dates_Banthuy$ageSds,
  positions = parse_number(dates_Banthuy$position),
  calCurves = rep('intcal13', nrow(dates_Banthuy))
)

summary(ages_Banthuy)
plot(ages_Banthuy, withPositions = TRUE)
Banthuy_age_samples = sampleAges(ages_Banthuy)
apply(Banthuy_age_samples, 2, quantile, prob=c(0.025,0.975))
apply(Banthuy_age_samples, 2, quantile, prob=c(0.5))

## Go Trung site
dates_Gotrung <- read_excel(here("data", "dates.xlsx"), sheet = 'Gotrung')
dates_Gotrung <- dates_Gotrung[order(dates_Gotrung$ages),]

### Ages 1
ages_Gotrung = BchronCalibrate(
  ages = dates_Gotrung$ages,
  ageSds = dates_Gotrung$ageSds,
  positions = dates_Gotrung$position,
  calCurves = rep('intcal13', nrow(dates_Gotrung))
)

summary(ages_Gotrung)
plot(ages_Gotrung, withPositions = TRUE)
Gotrung_age_samples = sampleAges(ages_Gotrung)
apply(Gotrung_age_samples, 2, quantile, prob=c(0.025,0.975))
apply(Gotrung_age_samples, 2, quantile, prob=c(0.5))

# Reading data from dates
## Con Co Ngau 2001 site

dates_ccn2001 <- read_excel(here("data", "dates.xlsx"), sheet = 'ccn2001')
dates_ccn2001 <- dates_ccn2001[order(dates_ccn2001$ages),]

### Ages 1

ages_ccn2001 = BchronCalibrate(
  ages = dates_ccn2001$ages,
  ageSds = dates_ccn2001$ageSds,
  positions = parse_number(dates_ccn2001$position),
  calCurves = rep('intcal13', nrow(dates_ccn2001))
)

summary(ages_ccn2001)
plot(ages_ccn2001, withPositions = TRUE)
ccn2001_age_samples = sampleAges(ages_ccn2001)
apply(ccn2001_age_samples, 2, quantile, prob=c(0.025,0.975))
apply(ccn2001_age_samples, 2, quantile, prob=c(0.5))

# Reading data from dates
## Hang Sao site
dates_Hangsao <- read_excel(here("data", "dates.xlsx"), sheet = 'Hangsao')
dates_Hangsao <- dates_Hangsao[order(dates_Hangsao$ages),]

### Ages 1
ages_Hangsao = BchronCalibrate(
  ages = dates_Hangsao$ages,
  ageSds = dates_Hangsao$ageSds,
  positions = rep(0, nrow(dates_Hangsao)),
  calCurves = rep('intcal13', nrow(dates_Hangsao))
)

summary(ages_Hangsao)
plot(ages_Hangsao, withPositions = TRUE)
Hangsao_age_samples = sampleAges(ages_Hangsao)
apply(Hangsao_age_samples, 2, quantile, prob=c(0.025,0.975))
apply(Hangsao_age_samples, 2, quantile, prob=c(0.5))

# Reading data from dates
## Hang Co site
dates_Hangco <- read_excel(here("data", "dates.xlsx"), sheet = 'Hangco')
dates_Hangco <- dates_Hangco[order(dates_Hangco$ages),]

### Ages 1
ages_Hangco = BchronCalibrate(
  ages = dates_Hangco$ages,
  ageSds = dates_Hangco$ageSds,
  positions = rep(0, nrow(dates_Hangco)),
  calCurves = rep('intcal13', nrow(dates_Hangco))
)

summary(ages_Hangco)
plot(ages_Hangco, withPositions = TRUE)
Hangco_age_samples = sampleAges(ages_Hangco)
apply(Hangco_age_samples, 2, quantile, prob=c(0.025,0.975))
apply(Hangco_age_samples, 2, quantile, prob=c(0.5))
age_diff = Hangco_age_samples[,2] - Hangco_age_samples[,1]
hist(age_diff)


