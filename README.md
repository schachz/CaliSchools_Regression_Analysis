# CA_Schools_RegressionAnalysis
## Zach Schachter

## Project Overview:
This project analyzes data from California K-6 and K-8 school districts to study the relationship between various school characteristics and standardized test scores. It involves data preprocessing, statistical tests, linear regression modeling, and lasso regression. The analysis explores factors such as enrollment, teacher-student ratio, socioeconomic indicators, and more to predict average test scores. The results provide insights into the most influential factors impacting students' performance in California schools.

## Data description:

The data used here are from all 420 K-6 and K-8 districts in California with data available for 1998 and 1999. Test scores are on the Stanford 9 standardized test administered to 5th grade students. School characteristics (averaged across the district) include enrollment, number of teachers (measured as “full-time equivalents”, number of computers per classroom, and expenditures per student. Demographic variables for the students are averaged across the district. The demographic variables include the percentage of students in the public assistance program CalWorks (formerly AFDC), the percentage of students that qualify for a reduced price lunch, and the percentage of students that are English learners (that is, students for whom English is a second language).

Reference: Stock, J. H. and Watson, M. W. (2007). *Introduction to Econometrics*, 2nd ed. Boston: Addison Wesley.


#### Dataset:

Data file is available at
https://github.com/schachz/CA_Schools_RegressionAnalysis/blob/main/CASchools.csv


#### Dataset characteristics:

- district: character. District code.
- school: character. School name.
- county: factor indicating county.
- grades: factor indicating grade span of district.
- students: Total enrollment.
- teachers: Number of teachers.
- calworks: Percent qualifying for CalWorks (income assistance).
- lunch: Percent qualifying for reduced-price lunch.
- computer: Number of computers.
- expenditure: Expenditure per student.
- income: District average income (in USD 1,000).
- english: Percent of English learners.
- read: Average reading score.
- math: Average math score.
