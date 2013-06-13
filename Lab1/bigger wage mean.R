# bigger wage mean

# reading data
data = read.csv("Lab1/adult.csv")

age = data$age
sex = data$sex
race = data$race
native_country = data$native.country
education_num = data$education.num
education = data$education
wages = data$Wages
hours_worked_per_year = data$hours.per.week * 4 * 12

# age --------------------------------------------------------------------------------

age_x_wages = data.frame(age,wages)

plot(age, wages, xlab="Age", ylab="Wages", main="Age x Wages Diagram", type="h")
summary(age_x_wages)
boxplot(wages ~ age, data=age_x_wages,main="Wages x Age Boxplot",xlab="Age",ylab="Wages")
# the boxplot shows that the age with bigger wage mean is 79 years-old

# sex -------------------------------------------------------------------------------
plot(sex, wages, xlab="Sex", ylab="Wages", main="Sex x Wages Diagram", type="h")
# with the plot we can note that the man have a bigger wage mean

# race ------------------------------------------------------------------------------
plot(race, wages, xlab="Race", ylab="Wages", main="Race x Wages Diagram", type="h")
summary(race)

race_x_wages = data.frame(race,wages)

plot(race, wages, xlab="Race", ylab="Wages", main="Race x Wages Diagram", type="h")
summary(race_x_wages)
boxplot(wages ~ race, data=race_x_wages,main="Race x Wages Boxplot",xlab="Race",ylab="Wages")
# the boxplot shows that the Asian-Pac-Islander have greather wages, and is relatively near
# of White's wages

# native country --------------------------------------------------------------------
native_country_x_wages = data.frame(native_country,wages)

plot(native_country, wages, xlab="Native Country", ylab="Wages", main="Native Country x Wages Diagram", type="h")
boxplot(wages ~ native_country, data=native_country_x_wages,main="Wages x Native Country Boxplot",xlab="Native Country",ylab="Wages")

# education years -------------------------------------------------------------------
education_years_x_wages = data.frame(education_num,wages)
boxplot(wages ~ education_num, data=education_years_x_wages,main="Wages x Education Years Boxplot",xlab="Education Years",ylab="Wages")
plot(education_num, wages, xlab="Years of Education", ylab="Wages", main="Years of Education x Wages Diagram", type="h")
# the boxplot shows that the wage increase with the years of study, being 15 and 16 the years of study
# with bigger income

# education level -------------------------------------------------------------------
education_level_x_wages = data.frame(education,wages)
plot(education, wages, xlab="Education Level", ylab="Wages", main="Education Level x Wages Diagram", type="h")
boxplot(wages ~ education, data=education_level_x_wages, xlab="Education Level", ylab="Wages",main="Education Level x Wages Boxplot")

# -----------------------------------------------------------------------------------------
# proportion between mean wage and mean hours-worked
mean_wages = mean(wages)
mean_wages
mean_hours_worked = mean(hours_worked_per_year)
mean_hours_worked

proportion = mean_wages/mean_hours_worked
proportion
