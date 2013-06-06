# script to non-numeric variables in adult.csv

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

marital_status = data$marital.status

summary(marital_status)

marital_status_frequencies = round(100*prop.table(table(marital_status)),1)
marital_status_top_frequent_value = max(marital_status_frequencies)

barplot(table(marital_status)) # bar chart

pie(table(marital_status)) # pie chart

# occupation -----------------------------------------------------------------------

occupation = data$occupation

summary(occupation)

occupation_frequencies = round(100*prop.table(table(occupation)),1)
occupation_top_frequent_value = max(occupation_frequencies)

barplot(table(occupation)) # bar chart

pie(table(occupation)) # pie chart

# relasionship ----------------------------------------------------------------------

relasionship = data$relasionship

summary(relasionship)

relasionship_frequencies = round(100*prop.table(table(relasionship)),1)
relasionship_top_frequent_value = max(relasionship_frequencies)

barplot(table(relasionship)) # bar chart

pie(table(relasionship)) # pie chart


# race -----------------------------------------------------------------------------

summary(data$race)

# sex ------------------------------------------------------------------------------

summary(data$sex)

# native.country --------------------------------------------------------------------

summary(data$native.country)

# class ----------------------------------------------------------------------------

summary(data$class)

# education -------------------------------------------------------------------------

summary(data$education)