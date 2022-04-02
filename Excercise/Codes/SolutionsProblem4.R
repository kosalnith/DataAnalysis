### Problem 4: Data transformations
#-------------------------------------------------------------------------------

### Filter rows with filter()
## 1. Use "flights" data frame in the "nycflights13" library and use 
## "tidyverse" package. Find all flights that:
library(tidyverse)
library(nycflights13)

# (a) Had an arrival delay of two or more hours. 
# To do so, you should find the variable that denote as an arrival delay.
str(flights)
?flights
# As show in the Help, an arrival delay denoted as arr_delay within minutes
head(flights$arr_delay)
filter(flights, arr_delay >= 120)

# (b) Show the flights "dest" variable that flew to Houston where 
# the destination is either "IAH" or "HOU".
head(flights$dest)
filter(flights, dest == "IAH" | dest == "HOU")

# (c) Were operated by United, American, or Delta.
# Find the operation or airlines to the United, American and Delta. 
?flights
# "carrier" variable is a two letter carrier abbreviation or the airline. 
flights$carrier
airlines

filter(flights, carrier %in% c("AA", "DL", "UA"))

# (d) Departed in summer (July, August, and September).
head(flights$month)
filter(flights, month >= 7, month <= 9) # or
dta <- filter(flights, month >= 7, month <= 9) # or
dta
filter(flights, month %in% 7:9) # is the same. 


# (e) Arrived more than two hours late, but didn't leave late.
filter(flights, arr_delay > 120, dep_delay <= 0)

# (f) Were delayed by at least an hour, but made up over 30 minutes in flight. 
filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)

# (g) Departed between midnight and 6am (inclusive).
flights$dep_time # format HMM: it means that 2400 = 24:00, 600 = 6:00 
summary(flights$dep_time)
filter(flights, dep_time <= 600 | dep_time == 2400) # or
filter(flights, dep_time %% 2400 <= 600)
count(filter(flights, dep_time <= 600 | dep_time == 2400))


## 2. In the month variable of the flights data frame, 
## please show departed in summer (month >= 7 & month <= 10) 
## using the between() function.	
filter(flights, between(month, 7, 9))

## 3. How many flights have a missing dep_time of the flights data frame? 
## What other variables are missing? What might these rows represent?
filter(flights, is.na(dep_time))
summary(filter(flights, is.na(dep_time)))

## 4. Why is NA\textasciicircum 0 not missing? Why is NA | TRUE not missing? 
## Why is FALSE & NA} not missing? Can you figure out the general rule? 
NA ^ 0 # since for all numeric values x^0 = 1
NA | TRUE # because anything or TRUE is TRUE
NA & FALSE # is FALSE because anything and FALSE is always FALSE
NA | FALSE # the value is unknown since TRUE | FALSE
NA & TRUE # the value is unknown since FALSE & TRUE== FALSE
NA * 0 # Since x ∗ 0 = 0 for all finite numbers

#-------------------------------------------------------------------------------

### Arrange rows with arrange()

## 1. Continuing the flights data frame, how could you use arrange() 
## to sort all missing values to the start?
## The start here is refer to departure time.
?arrange
?desc # Descending order
arrange(flights, desc(is.na(dep_time)), dep_time)

## 2. Sort flights to find the most delayed flights. Find the flights that left earliest.
arrange(flights, desc(dep_delay))
arrange(flights, dep_delay)

## Sort flights to find the fastest (the highest speed) flights.

## "fastest" flight we can be assume the flight with the shortest flight time. 
## We can use arrange to sort our data by the air_time
## the “fastest flight” is the flight with the highest average ground speed.
arrange(flights, air_time)
head(flights$distance)
arrange(flights, desc(distance / air_time))

## 3. Which flights traveled the farthest? Which traveled the shortest?
arrange(flights, desc(distance))
arrange(flights, distance)
arrange(flights, desc(air_time))
arrange(flights, air_time)

#-------------------------------------------------------------------------------

### Select columns with select()

## 1. Please select dep_time, dep_delay, arr_time, and arr_delay from flights data frame.
select(flights, dep_time, dep_delay, arr_time, arr_delay)

## 2. What happens if you include the name of a variable multiple times in a 
## select() call?
select(flights, year, month, day, year, year)
select(flights, arr_delay, everything())

## 3. What does the one_of() function do? 
## Why might it be helpful in conjunction with this vector?
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))
select(flights, any_of(vars))
select(flights, all_of(vars))

## 4. Does the result of running the following code surprise you? 
## How do the select helpers deal with case by default? 
## How can you change that default?
select(flights, contains("TIME"))
select(flights, contains("TIME", ignore.case = FALSE))

#-------------------------------------------------------------------------------

### Add new variables with mutate()

## 1. Currently dep_time and sched_dep_time} are convenient to look at, 
## but hard to compute with because they're not really continuous numbers. 
## Convert them to a more convenient representation of number of minutes since midnight.
head(flights$dep_time)
head(flights$sched_dep_time)
# For example, 1600 represents 16:00 (or 4:00 PM), which is 960 minutes after midnight.
# We need a way to split out the hour-digits from the minute-digits.
1600 %/% 100
1600 %% 100
# We can combine the hours (multiplied by 60 to convert them to minutes) 
# and minutes to get the number of minutes after midnight.
1600 %/% 100 * 60 + 1600 %% 100
# Even so
60*24 # Which represented Midnight 2400, we convert it to zero while keeping 
# all the other times the same. 

flights_times <- mutate(flights,
                        dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                        sched_dep_time_mins = (sched_dep_time %/% 100 * 60 +
                                                 sched_dep_time %% 100) %% 1440
)

# Now, read the new variable
select(
  flights_times, dep_time, dep_time_mins, sched_dep_time,
  sched_dep_time_mins
)

# Other method is create your own function. 
time2mins <- function(x) {
  (x %/% 100 * 60 + x %% 100) %% 1440
}

flights_times <- mutate(flights,
                        dep_time_mins = time2mins(dep_time),
                        sched_dep_time_mins = time2mins(sched_dep_time)
)

select(
  flights_times, dep_time, dep_time_mins, sched_dep_time,
  sched_dep_time_mins
)

## 2. Compare air_time with arr_time - dep_time. What do you expect to see? 
## What do you see? What do you need to do to fix it?
head(flights$air_time) # in minutes. 
flights_airtime <-
  mutate(flights,
         dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
         air_time_diff = air_time - arr_time + dep_time
  )
nrow(filter(flights_airtime, air_time_diff != 0))

ggplot(flights_airtime, aes(x = air_time_diff)) +
  geom_histogram(binwidth = 1)
# it shows that there are many flights in which the difference between 
# air time and local arrival and departure times is not divisible by 60.

## 3. Compare dep_time, sched_dep_time, and dep_delay. 
## How would you expect those three numbers to be related?

# We expect the dep_delay to be equal to the difference between 
# sched_dep_time, and dep_time: dep_time - sched_dep_time = dep_delay.

flights_deptime <-
  mutate(flights,
         dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         sched_dep_time_min = (sched_dep_time %/% 100 * 60 +
                                 sched_dep_time %% 100) %% 1440,
         dep_delay_diff = dep_delay - dep_time_min + sched_dep_time_min
  )
	
filter(flights_deptime, dep_delay_diff != 0)

ggplot(
  filter(flights_deptime, dep_delay_diff > 0),
  aes(y = sched_dep_time_min, x = dep_delay_diff)
) +
  geom_point()

## 4. Find the 10 most delayed flights using a ranking function. 
## How do you want to handle ties? 

flights_delayed <- mutate(flights, 
                          dep_delay_min_rank = min_rank(desc(dep_delay)),
                          dep_delay_row_number = row_number(desc(dep_delay)),
                          dep_delay_dense_rank = dense_rank(desc(dep_delay))
)

flights_delayed <- filter(flights_delayed, 
                          !(dep_delay_min_rank > 10 | dep_delay_row_number > 10 |
                              dep_delay_dense_rank > 10))


flights_delayed <- arrange(flights_delayed, dep_delay_min_rank)

print(select(flights_delayed, month, day, carrier, flight, dep_delay, 
             dep_delay_min_rank, dep_delay_row_number, dep_delay_dense_rank), 
      n = Inf)


## 5. What does 1:3 + 1:10 return? Why?
1:3 + 1:10
# When adding two vectors, R recycles the shorter vector’s values to create 
# a vector of the same length as the longer vector.
	
## 6. What trigonometric functions does R provide?
?Trig	

#-------------------------------------------------------------------------------

### Grouped summaries with summarise()

## 1. Come up with another approach that will give you the same output as 
## not_cancelled %>% count(dest) and not_cancelled %>% count(tailnum, wt = distance)

# (a)
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  count(dest)

## Without using count().
not_cancelled %>%
  group_by(dest) %>%
  summarise(n = length(dest))

# (b)
not_cancelled %>% 
  count(tailnum, wt = distance)

## Without using count().
not_cancelled %>%
  group_by(tailnum) %>%
  tally(distance)

## 2. Our definition of canceled flights (is.na(dep_delay) | is.na(arr_delay))
## is slightly sub-optimal. Why? Which is the most important column?

# If a flight never departs, then it won’t arrive. A flight could also depart 
# and not arrive if it crashes, or if it is redirected and lands in an airport 
# other than its intended destination.
filter(flights, !is.na(dep_delay), is.na(arr_delay)) %>%
  select(dep_time, arr_time, sched_arr_time, dep_delay, arr_delay)

## 3. Look at the number of canceled flights per day. Is there a pattern? 
## Is the proportion of canceled flights related to the average delay?
cancelled_per_day <- 
  flights %>%
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    cancelled_num = sum(cancelled),
    flights_num = n(),
  )

ggplot(cancelled_per_day) +
  geom_point(aes(x = flights_num, y = cancelled_num)) 

cancelled_and_delays <- 
  flights %>%
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    cancelled_prop = mean(cancelled),
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(cancelled_and_delays) +
  geom_point(aes(x = avg_dep_delay, y = cancelled_prop))

ggplot(cancelled_and_delays) +
  geom_point(aes(x = avg_arr_delay, y = cancelled_prop))


## 4. Which carrier has the worst delays? Challenge: 
## can you disentangle the effects of bad airports vs. bad carriers? Why/why not? 

flights %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))


airlines  

flights %>%
  filter(!is.na(arr_delay)) %>%
  # Total delay by carrier within each origin, dest
  group_by(origin, dest, carrier) %>%
  summarise(
    arr_delay = sum(arr_delay),
    flights = n()
  ) %>%
  # Total delay within each origin dest
  group_by(origin, dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    flights_total = sum(flights)
  ) %>%
  # average delay of each carrier - average delay of other carriers
  ungroup() %>%
  mutate(
    arr_delay_others = (arr_delay_total - arr_delay) /
      (flights_total - flights),
    arr_delay_mean = arr_delay / flights,
    arr_delay_diff = arr_delay_mean - arr_delay_others
  ) %>%
  # remove NaN values (when there is only one carrier)
  filter(is.finite(arr_delay_diff)) %>%
  # average over all airports it flies to
  group_by(carrier) %>%
  summarise(arr_delay_diff = mean(arr_delay_diff)) %>%
  arrange(desc(arr_delay_diff))

## 5. What does the sort argument to count() do? When might you use it?
flights %>%
  count(dest, sort = TRUE)

#-------------------------------------------------------------------------------

### Grouped mutates (and filters)

## 1. Refer back to the lists of useful mutate and filtering functions. 
## Describe how each operation changes when you combine it with grouping.

# Should read the book by yourself. 

## 2. What time of day should you fly if you want to avoid delays as much as possible?
flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)

## 3. For each destination, compute the total minutes of delay. For each flight, 
## compute the proportion of the total delay for its destination.
flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    arr_delay_prop = arr_delay / arr_delay_total
  ) %>%
  select(dest, month, day, dep_time, carrier, flight,
         arr_delay, arr_delay_prop) %>%
  arrange(dest, desc(arr_delay_prop))

# flight could also refer to the flight number, which is the code a carrier 
# uses for an airline service of a route

flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest, origin, carrier, flight) %>%
  summarise(arr_delay = sum(arr_delay)) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_prop = arr_delay / sum(arr_delay)
  ) %>%
  arrange(dest, desc(arr_delay_prop)) %>%
  select(carrier, flight, origin, dest, arr_delay_prop)


## 4. Delays are typically temporally correlated: even once the problem 
## that caused the initial delay has been resolved, later flights are 
## delayed to allow earlier flights to leave. Using lag() explore how 
## the delay of a flight is related to the delay of the immediately preceding flight.
lagged_delays <- flights %>%
  arrange(origin, month, day, dep_time) %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))

lagged_delays %>%
  group_by(dep_delay_lag) %>%
  summarise(dep_delay_mean = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay_mean, x = dep_delay_lag)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 1500, by = 120)) +
  labs(y = "Departure Delay", x = "Previous Departure Delay")

lagged_delays %>%
  group_by(origin, dep_delay_lag) %>%
  summarise(dep_delay_mean = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay_mean, x = dep_delay_lag)) +
  geom_point() +
  facet_wrap(~ origin, ncol=1) +
  labs(y = "Departure Delay", x = "Previous Departure Delay")

## 5. Look at each destination. Can you find flights that are suspiciously fast? 
## (i.e. flights that represent a potential data entry error). Compute 
## the air time of a flight relative to the shortest flight to that destination. 
## Which flights were most delayed in the air?
standardized_flights <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest, origin) %>%
  mutate(
    air_time_mean = mean(air_time),
    air_time_sd = sd(air_time),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(air_time_standard = (air_time - air_time_mean) / (air_time_sd + 1))

ggplot(standardized_flights, aes(x = air_time_standard)) +
  geom_density()

standardized_flights %>%
  arrange(air_time_standard) %>%
  select(
    carrier, flight, origin, dest, month, day,
    air_time, air_time_mean, air_time_standard
  ) %>%
  head(10) %>%
  print(width = Inf)

standardized_flights2 <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest, origin) %>%
  mutate(
    air_time_median = median(air_time),
    air_time_iqr = IQR(air_time),
    n = n(),
    air_time_standard = (air_time - air_time_median) / air_time_iqr)

ggplot(standardized_flights2, aes(x = air_time_standard)) +
  geom_density()

# Unusually fast flights are those flights with the smallest standardized values.

standardized_flights2 %>%
  arrange(air_time_standard) %>%
  select(
    carrier, flight, origin, dest, month, day, air_time,
    air_time_median, air_time_standard
  ) %>%
  head(10) %>%
  print(width = Inf)

flights %>%
  mutate(mph = distance / (air_time / 60)) %>%
  ggplot(aes(x = mph)) +
  geom_histogram(binwidth = 10)

# The fastest flight is the same one identified as the largest outlier earlier. 
flights %>%
  mutate(mph = distance / (air_time / 60)) %>%
  arrange(desc(mph)) %>%
  select(mph, flight, carrier, flight, month, day, dep_time) %>%
  head(5)

# One explanation for unusually fast flights is that they are “making up time” 
# in the air by flying faster.
flights %>%
  mutate(mph = distance / (air_time / 60)) %>%
  arrange(desc(mph)) %>%
  select(
    origin, dest, mph, year, month, day, dep_time, flight, carrier,
    dep_delay, arr_delay
  )

# Compare flights to the fastest flight on a route to find the flights 
# most delayed in the air
air_time_delayed <-
  flights %>%
  group_by(origin, dest) %>%
  mutate(
    air_time_min = min(air_time, na.rm = TRUE),
    air_time_delay = air_time - air_time_min,
    air_time_delay_pct = air_time_delay / air_time_min * 100
  )

# The most delayed flight in air in minutes was DL841 from JFK to SFO which 
# departed on 2013-07-28 at 17:27. It took 189 minutes longer than the flight 
# with the shortest air time on its route.

air_time_delayed %>%
  arrange(desc(air_time_delay)) %>%
  select(
    air_time_delay, carrier, flight,
    origin, dest, year, month, day, dep_time,
    air_time, air_time_min
  ) %>%
  head() %>%
  print(width = Inf)

# The most delayed flight in air as a percentage of the fastest flight along 
# that route was US2136 from LGA to BOS departing on 2013-06-17 at 16:52.
air_time_delayed %>%
  arrange(desc(air_time_delay)) %>%
  select(
    air_time_delay_pct, carrier, flight,
    origin, dest, year, month, day, dep_time,
    air_time, air_time_min
  ) %>%
  head() %>%
  print(width = Inf)

## 6. Find all destinations that are flown by at least two carriers. 
## Use that information to rank the carriers.
 
flights %>%
  # find all airports with > 1 carrier
  group_by(dest) %>%
  mutate(n_carriers = n_distinct(carrier)) %>%
  filter(n_carriers > 1) %>%
  # rank carriers by numer of destinations
  group_by(carrier) %>%
  summarize(n_dest = n_distinct(dest)) %>%
  arrange(desc(n_dest))

filter(airlines, carrier == "EV") # flies to the most destinations
filter(airlines, carrier %in% c("AS", "F9", "HA"))

## 7. For each plane, count the number of flights before the first delay 
## of greater than 1 hour.
flights %>%
  # sort in increasing order
  select(tailnum, year, month,day, dep_delay) %>%
  filter(!is.na(dep_delay)) %>%
  arrange(tailnum, year, month, day) %>%
  group_by(tailnum) %>%
  # cumulative number of flights delayed over one hour
  mutate(cumulative_hr_delays = cumsum(dep_delay > 60)) %>%
  # count the number of flights == 0
  summarise(total_flights = sum(cumulative_hr_delays < 1)) %>%
  arrange(total_flights)
