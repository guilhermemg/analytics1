# script to numeric variables in adult.csv

# reading data
data = read.csv("Lab1/adult.csv")

# age ------------------------------------------------------------------------------
# scale type : ratio
# ==> better central tendency index (to assymetric distribution): median
# ==> better measure of dispersion: iqr (interquartile range)

age = data$age

summary(age)
boxplot(age)
hist(age)

age_min = min(age) # minimum
age_max = max(age) # maximum
age_var = var(age) # variance
age_sd = sd(age) # standard deviation
age_cov = age_var/mean(age) # covariance
age_q1 = quantile(age, .25) # first quartil
age_q3 = quantile(age, .75) # third quartil
age_iqr = age_q3 - age_q1 # interquartile diff
age_BL = age_q1 - ((1.5) * age_iqr) # bottom limit
age_UL = age_q3 - ((1.5) * age_iqr) # upper limit

abline(v = mean(age), col=2, lty=2, lwd=2) 
abline(v = median(age), col=3, lty=3, lwd=2) 
abline(v = which.max(table(age)), col=4, lty=4, lwd=2)
ex12 = expression(mean, median, mode) 
utils::str(legend("topright", ex12, col = 2:4, lty=2:4, lwd=2))


# fnlwgt (Final sampling weight) ---------------------------------------------------
# scale type : ratio
# ==> better central tendency index (to assymetric distribution): median
# ==> better measure of dispersion: iqr (interquartile range) (?)

# ==> specifically, in this case, the variable is a tax, with unknowlegde formula, so
# some other central tendency index can be better that the meadian value

fnlwgt = data$fnlwgt
summary(fnlwgt)
boxplot(fnlwgt)
hist(fnlwgt)

fnlwgt_min = min(fnlwgt) # minimum
fnlwgt_max = max(fnlwgt) # maximum
fnlwgt_var = var(fnlwgt) # variance
fnlwgt_sd = sd(fnlwgt) # standard deviation
fnlwgt_cov = age_var/mean(fnlwgt) # covariance
fnlwgt_q1 = quantile(fnlwgt, .25) # first quartil
fnlwgt_q3 = quantile(fnlwgt, .75) # third quartil
fnlwgt_iqr = fnlwgt_q3 - fnlwgt_q1 # interquartile diff
fnlwgt_BL = fnlwgt_q1 - ((1.5) * fnlwgt_iqr) # bottom limit
fnlwgt_UL = fnlwgt_q3 - ((1.5) * fnlwgt_iqr) # upper limit

abline(v = mean(fnlwgt), col=2, lty=2, lwd=2) 
abline(v = median(fnlwgt), col=3, lty=3, lwd=2) 
abline(v = which.max(table(fnlwgt)), col=4, lty=4, lwd=2)
ex12 = expression(mean, median, mode) 
utils::str(legend("topright", ex12, col = 2:4, lty=2:4, lwd=2))

# education.num --------------------------------------------------------------------
# scale type : ratio
# ==> better central tendency index (to symetric and unimodal distribution): median
# ==> better measure of dispersion: var, sd or cov

# this variable is the number of years of education, so is a sortable variable

education_num = data$education.num

summary(education_num)
boxplot(education_num)
h = hist(education_num, nclass=4)

education_num_min = min(education_num) # minimum
education_num_max = max(education_num) # maximum
education_num_var = var(education_num) # variance
education_num_sd = sd(education_num) # standard deviation
education_num_cov = education_num_var/mean(education_num) # covariance
education_num_q1 = quantile(education_num, .25) # first quartil
education_num_q3 = quantile(education_num, .75) # third quartil
education_num_iqr = education_num_q3 - education_num_q1 # interquartile diff
education_num_BL = education_num_q1 - ((1.5) * education_num_iqr) # bottom limit
education_num_UL = education_num_q3 - ((1.5) * education_num_iqr) # upper limit

abline(v = mean(education_num), col=2, lty=2, lwd=2) 
abline(v = median(education_num), col=3, lty=3, lwd=2) 
abline(v = which.max(table(education_num)), col=4, lty=4, lwd=2)
ex12 = expression(mean, median, mode) 
utils::str(legend("topright", ex12, col = 2:4, lty=2:4, lwd=2))

# capital.gain --------------------------------------------------------------------
# scale type : ratio
# ==> better central tendency index (to assymetric distribution): median
# ==> better measure of dispersion: quantiles or iqr

capital_gain = data$capital.gain

summary(capital_gain)
boxplot(capital_gain)
hist(capital_gain)

capital_gain_min = min(capital_gain) # minimum
capital_gain_max = max(capital_gain) # maximum
capital_gain_var = var(capital_gain) # variance
capital_gain_sd = sd(capital_gain) # standard deviation
capital_gain_cov = capital_gain_var/mean(capital_gain) # covariance
capital_gain_q1 = quantile(capital_gain, .25) # first quartil
capital_gain_q3 = quantile(capital_gain, .75) # third quartil
capital_gain_iqr = capital_gain_q3 - capital_gain_q1 # interquartile diff
capital_gain_BL = capital_gain_q1 - ((1.5) * capital_gain_iqr) # bottom limit
capital_gain_UL = capital_gain_q3 - ((1.5) * capital_gain_iqr) # upper limit

abline(v = mean(capital_gain), col=2, lty=2, lwd=2) 
abline(v = median(capital_gain), col=3, lty=3, lwd=2) 
abline(v = which.max(table(capital_gain)), col=4, lty=4, lwd=2)
ex12 = expression(mean, median, mode) 
utils::str(legend("topright", ex12, col = 2:4, lty=2:4, lwd=2))

# capital.loss ----------------------------------------------------------------------
# scale type : ratio
# ==> better central tendency index (to assymetric distribution): median
# ==> better measure of dispersion: quantiles or iqr

capital_loss = data$capital.loss

summary(capital_loss)
boxplot(capital_loss)
hist(capital_loss)

capital_loss_min = min(capital_loss) # minimum
capital_loss_max = max(capital_loss) # maximum
capital_loss_var = var(capital_loss) # variance
capital_loss_sd = sd(capital_loss) # standard deviation
capital_loss_cov = capital_loss_var/mean(capital_loss) # covariance
capital_loss_q1 = quantile(capital_loss, .25) # first quartil
capital_loss_q3 = quantile(capital_loss, .75) # third quartil
capital_loss_iqr = capital_loss_q3 - capital_loss_q1 # interquartile diff
capital_loss_BL = capital_loss_q1 - ((1.5) * capital_loss_iqr) # bottom limit
capital_loss_UL = capital_loss_q3 - ((1.5) * capital_loss_iqr) # upper limit

abline(v = mean(capital_loss), col=2, lty=2, lwd=2) 
abline(v = median(capital_loss), col=3, lty=3, lwd=2) 
abline(v = which.max(table(capital_loss)), col=4, lty=4, lwd=2)
ex12 = expression(mean, median, mode)
utils::str(legend("topright", ex12, col = 2:4, lty=2:4, lwd=2))

# hours.per.week --------------------------------------------------------------------
# scale type : ratio
# ==> better central tendency index (to symetric and unimodal distribution): median
# ==> better measure of dispersion: var, sd or cov

hours_per_week = data$hours.per.week

summary(hours_per_week)
boxplot(hours_per_week)
hist(hours_per_week, nclass=5)

hours_per_week_min = min(hours_per_week) # minimum
hours_per_week_max = max(hours_per_week) # maximum
hours_per_week_var = var(hours_per_week) # variance
hours_per_week_sd = sd(hours_per_week) # standard deviation
hours_per_week_cov = hours_per_week_var/mean(hours_per_week) # covariance
hours_per_week_q1 = quantile(hours_per_week, .25) # first quartil
hours_per_week_q3 = quantile(hours_per_week, .75) # third quartil
hours_per_week_iqr = hours_per_week_q3 - hours_per_week_q1 # interquartile diff
hours_per_week_BL = hours_per_week_q1 - ((1.5) * hours_per_week_iqr) # bottom limit
hours_per_week_UL = hours_per_week_q3 - ((1.5) * hours_per_week_iqr) # upper limit

abline(v = mean(hours_per_week), col=2, lty=2, lwd=2) 
abline(v = median(hours_per_week), col=3, lty=3, lwd=2) 
abline(v = which.max(table(hours_per_week)), col=4, lty=4, lwd=2)
ex12 = expression(mean, median, mode) 
utils::str(legend("topright", ex12, col = 2:4, lty=2:4, lwd=2))

# Wages (annual) --------------------------------------------------------------------
# scale type : ratio
# ==> better central tendency index (to symetric and unimodal distribution): median
# ==> better measure of dispersion: var, sd or cov

wages = data$Wages

summary(wages)
boxplot(wages)
hist(wages, nclass=5)

wages_min = min(wages) # minimum
wages_max = max(wages) # maximum
wages_var = var(wages) # variance
wages_sd = sd(wages) # standard deviation
wages_cov = wages_var/mean(wages) # covariance
wages_q1 = quantile(wages, .25) # first quartil
wages_q3 = quantile(wages, .75) # third quartil
wages_iqr = wages_q3 - wages_q1 # interquartile diff
wages_BL = wages_q1 - ((1.5) * wages_iqr) # bottom limit
wages_UL = wages_q3 - ((1.5) * wages_iqr) # upper limit

abline(v = mean(wages), col=2, lty=2, lwd=2) 
abline(v = median(wages), col=3, lty=3, lwd=2) 
abline(v = which.max(table(wages)), col=4, lty=4, lwd=2)
ex12 = expression(mean, median, mode) 
utils::str(legend("topright", ex12, col = 2:4, lty=2:4, lwd=2))

