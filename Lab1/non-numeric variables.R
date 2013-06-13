# script to non-numeric variables in adult.csv

# reading data
data = read.csv("Lab1/adult.csv")

# workclass ------------------------------------------------------------------------
# scale type : nominal
# ==> better central tendency index (to assymetric distribution): mode
# ==> better measure of dispersion n(to non-numeric variables): top-frequent

workclass = data$workclass

summary(workclass)

workclass_frequencies = round(100*prop.table(table(workclass)),1)
workclass_top_frequent_value = max(workclass_frequencies)

barplot(table(workclass)) # bar chart

pie(table(workclass)) # pie chart

# marital-status -------------------------------------------------------------------
# scale type : nominal
# ==> better central tendency index (to assymetric distribution): mode
# ==> better measure of dispersion n(to non-numeric variables): top-frequent

marital_status = data$marital.status

summary(marital_status)

marital_status_frequencies = round(100*prop.table(table(marital_status)),1)
marital_status_top_frequent_value = max(marital_status_frequencies)

barplot(table(marital_status)) # bar chart

pie(table(marital_status)) # pie chart

# occupation -----------------------------------------------------------------------
# scale type : nominal
# ==> better central tendency index (to assymetric distribution): mode
# ==> better measure of dispersion n(to non-numeric variables): top-frequent

occupation = data$occupation

summary(occupation)

occupation_frequencies = round(100*prop.table(table(occupation)),1)
occupation_top_frequent_value = max(occupation_frequencies)

barplot(table(occupation)) # bar chart

pie(table(occupation)) # pie chart

# relasionship ----------------------------------------------------------------------
# scale type : nominal
# ==> better central tendency index (to assymetric distribution): mode
# ==> better measure of dispersion n(to non-numeric variables): top-frequent

relasionship = data$relasionship

summary(relasionship)

relasionship_frequencies = round(100*prop.table(table(relasionship)),1)
relasionship_top_frequent_value = max(relasionship_frequencies)

barplot(table(relasionship)) # bar chart

pie(table(relasionship)) # pie chart


# race -----------------------------------------------------------------------------
# scale type : nominal
# ==> better central tendency index (to assymetric distribution): mode
# ==> better measure of dispersion n(to non-numeric variables): top-frequent

race = data$race

summary(race)

race_frequencies = round(100*prop.table(table(race)),1)
race_top_frequent_value = max(race_frequencies)

barplot(table(race)) # bar chart

pie(table(race)) # pie chart

# sex ------------------------------------------------------------------------------
# scale type : nominal
# ==> better central tendency index (to assymetric distribution): mode
# ==> better measure of dispersion n(to non-numeric variables): top-frequent

sex = data$sex

summary(sex)

sex_frequencies = round(100*prop.table(table(sex)),1)
sex_top_frequent_value = max(sex_frequencies)

barplot(table(sex)) # bar chart

pie(table(sex)) # pie chart

# native.country --------------------------------------------------------------------
# scale type : nominal
# ==> better central tendency index (to assymetric distribution): mode
# ==> better measure of dispersion n(to non-numeric variables): top-frequent

native_country = data$native.country

summary(native_country)

native_country_frequencies = round(100*prop.table(table(native_country)),1)
native_country_top_frequent_value = max(native_country_frequencies)

barplot(table(native_country)) # bar chart

pie(table(native_country)) # pie chart


# class ----------------------------------------------------------------------------
# scale type : nominal
# ==> better central tendency index (to assymetric distribution): mode
# ==> better measure of dispersion n(to non-numeric variables): top-frequent

class = data$class

summary(class)

class_frequencies = round(100*prop.table(table(class)),1)
class_top_frequent_value = max(class_frequencies)

barplot(table(class)) # bar chart

pie(table(class)) # pie chart


# education -------------------------------------------------------------------------
# scale type : nominal
# ==> better central tendency index (to assymetric distribution): mode
# ==> better measure of dispersion n(to non-numeric variables): top-frequent

education = data$education

summary(education)

education_frequencies = round(100*prop.table(table(education)),1)
education_top_frequent_value = max(education_frequencies)

barplot(table(education)) # bar chart

pie(table(education)) # pie chart