do_this(to_this)
do_that(to_this, to_that, with_those)

install.packages("package_name")
library(package_name)

library(openintro)
loans <- loans_full_schema %>%
  select(loan_amount, interest_rate, term, grade, 
         state, annual_income, homeownership, debt_to_income)
glimpse(loans)