
## ----eval = F------------------------------------------------------------
## install.packages("SSOAP", repos="http://www.omegahat.org/R", dependencies = T,
##   type =  "source")


## ----eval = F, message = F-----------------------------------------------
## install.packages('devtools')
## require(devtools)
## install_github('fawda123/SWMPr')
## require(SWMPr)

## ----eval = T, echo = F, message = F, cache = F--------------------------
devtools::load_all('M:/docs/SWMPr')


## ----eval = F------------------------------------------------------------
## # retrieve metadata for all sites
## site_codes()
## 
## # retrieve metadata for a single site
## site_codes_ind('apa')


## ----eval = F------------------------------------------------------------
## # all parameters for a station, most recent
## all_params('hudscwq')
## 
## # get all parameters within a date range
## all_params_dtrng('hudscwq', c('09/10/2012', '02/8/2013'))
## 
## # get single parameter within a date range
## all_params_dtrng('hudscwq', c('09/10/2012', '02/8/2013'), param = 'do_mgl')
## 
## # single parameter for a station, most recent
## single_param('hudscwq', 'do_mgl')
## 


## ----eval = F------------------------------------------------------------
## # import data for apaebmet that you downloaded
## 
## # this is an example path with the csv files, change as needed
## path <- 'C:/my_path/'
## 
## # import, do not include file extension
## import_local(path, 'apaebmet')


## ----eval = T, results = 'hide'------------------------------------------
# import data for apaebmet that comes with SWMPr

# this is the path for csv example files
path <- system.file('zip_ex', package = 'SWMPr')

# import, do not include file extension
import_local(path, 'apaebmet') 


## ----eval = T------------------------------------------------------------
# import data and assign to dat
dat <- import_local(path, 'apaebmet', trace = F) 

# view first six rows
head(dat)


## ----eval = T------------------------------------------------------------
# verify that dat is swmpr class
class(dat)

# all attributes of dat
names(attributes(dat))

# a single attribute of dat
attr(dat, 'station')


## ----eval = T------------------------------------------------------------
# available methods for swmpr class
methods(class = 'swmpr')


## ----eval = F------------------------------------------------------------
## # qaqc screen for a swmpr object, retain only '0'
## qaqc(dat)
## 
## # retain all data regardless of flag
## qaqc(dat, qaqc_keep = NULL)
## 
## # retain only '0' and '-1' flags
## qaqc(dat, qaqc_keep = c(0, -1))
## 


## ----eval = F------------------------------------------------------------
## # select two parameters from dat
## subset(dat, select = c('rh', 'bp'))
## 
## # subset records greater than or equal to a date
## subset(dat, subset = '2013-01-01 0:00', operator = '>=')
## 
## # subset records within a date range
## subset(dat, subset = c('2012-07-01 6:00', '2012-08-01 18:15'))
## 
## # subset records within a date range, select two parameters
## subset(dat, subset = c('2012-07-01 6:00', '2012-08-01 18:15'),
##   select = c('atemp', 'totsorad'))
## 
## # remove rows/columns that do not contain data
## subset(dat, rem_rows = T, rem_cols = T)
## 


## ----eval = F------------------------------------------------------------
## # convert time series to two hour invervals
## # tolerance of +/- 30 minutes for matching existing data
## setstep(dat, timestep = 120, differ = 30)
## 
## # convert a nutrient time series to a continuous time series
## # then remove empty rows and columns
## dat_nut <- import_local(path, 'apacpnut')
## dat_nut <- setstep(dat_nut, timestep = 60)
## subset(dat_nut, rem_rows = T, rem_cols = T)


## ----eval = F------------------------------------------------------------
## # get nuts, wq, and met data as separate objects for the same station
## # note that most sites usually have one weather station
## swmp1 <- import_local(path, 'apacpnut')
## swmp2 <- import_local(path, 'apacpwq')
## swmp3 <- import_local(path, 'apaebmet')
## 
## # combine nuts and wq data by union
## comb(swmp1, swmp2, method = 'union')
## 
## # combine nuts and wq data by intersect
## comb(swmp1, swmp3, method = 'intersect')
## 
## # combine nuts, wq, and met data by nuts time series, two hour time step
## comb(swmp1, swmp2, swmp3, timestep = 120, method = 'apacpnut')
## 


## ----eval = F------------------------------------------------------------
## # combine, qaqc, remove empty columns
## dat <- comb(swmp1, swmp2, method = 'union')
## dat <- qaqc(dat)
## swmpr_in <- subset(dat, rem_cols = T)
## 
## # get mean DO by quarters
## aggregate(swmpr_in, 'quarters', params = c('do_mgl'))
## 
## # get mean DO by quarters, remove NA when calculating means
## fun_in <- function(x) mean(x, na.rm = T)
## aggregate(swmpr_in, FUN = fun_in, 'quarters', params = c('do_mgl'))
## 


## ----eval = T, fig.height = 3--------------------------------------------
# import data
swmp1 <- import_local(path, 'apadbwq')

# qaqc and subset imported data
dat <- qaqc(swmp1)
dat <- subset(dat, subset = c('2012-07-09 00:00', '2012-07-24 00:00'))

# filter
test <- smoother(dat, window = 50, params = 'do_mgl')

# plot to see the difference
plot(do_mgl ~ datetimestamp, data = dat, type = 'l')
lines(test, select = 'do_mgl', col = 'red', lwd = 2)



## ----eval = T, fig.height = 6--------------------------------------------
# get data
swmp1 <- import_local(path, 'apadbwq')

# qaqc and subset imported data
dat <- qaqc(swmp1)
dat <- subset(dat, subset = c('2013-01-22 00:00', '2013-01-26 00:00'))

# interpolate, maxgap of 10 records
test <- na.approx(dat, params = 'do_mgl', maxgap = 10)

# interpolate maxgap of 30 records
test2 <- na.approx(dat, params = 'do_mgl', maxgap = 30)

# plot for comparison
par(mfrow = c(3, 1))
plot(do_mgl ~ datetimestamp, dat, main = 'Raw', type = 'l')
plot(do_mgl ~ datetimestamp, test, col = 'red', 
  main = 'Interpolation - maximum gap of 10 records', type = 'l')
lines(dat, select = 'do_mgl')
plot(do_mgl ~ datetimestamp, test2, col = 'red', 
  main = 'Interpolation - maximum gap of 30 records', type = 'l')
lines(dat, select = 'do_mgl')


## ----eval = T, fig.height = 6--------------------------------------------
# get data
swmp1 <- import_local(path, 'apadbwq')

# subset for daily decomposition
dat <- subset(swmp1, subset = c('2013-07-01 00:00', '2013-07-31 00:00'))

# decomposition and plot
test <- decomp(dat, param = 'do_mgl', frequency = 'daily')
plot(test)



## ----eval = T, fig.height = 6, warning = F-------------------------------
# get data
dat <- subset(swmp1, subset = c('2013-06-01 00:00', '2013-07-31 00:00'))

# this returns an error
# test <- decomp(dat, param = 'do_mgl', frequency = 'daily')

# how many missing values?
sum(is.na(dat$do_mgl))

# use na.approx to interpolate missing data
dat <- na.approx(dat, params = 'do_mgl', maxgap = 10)

# decomposition and plot
test <- decomp(dat, param = 'do_mgl', frequency = 'daily')
plot(test)



