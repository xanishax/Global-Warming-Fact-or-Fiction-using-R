# Clean up all variables
rm(list = ls())

# Define the basic parameters
N_population <- 10000 # Population size
N_days <- 100 # Number of simulation days
initial_sick <- 2  # Number of initially infected citizens
meet_people <- 20  # Maximum number of people a sick person can meet
infection_rate <- 0.3  # Base infection rate
infection_period <- 10  # Number of days a sick person remains infectious
mortality_rate <- 0.005 / infection_period  # Mortality rate
immunity_coeff <- 0.1  # Immunity coefficient

# Create initial health state and day count
health_state <- rep(0, times = N_population)  # 0 represents healthy
day_count <- rep(0, times = N_population)

# Randomly mark initial sick citizens
initial_sick_indices <- sample(1:N_population, size = initial_sick)
health_state[initial_sick_indices] <- 1  # 1 represents sick

# Create an empty data frame to store daily results
results_df <- data.frame(
  Day = integer(0),
  TotalHealthy = integer(0),
  TotalSick = integer(0),
  DailyDeaths = integer(0),
  TotalDeaths = integer(0),
  NewInfections = integer(0)
)

# Initialize vectors to store historical data
history_total_sick <- integer(0)
history_total_healthy <- integer(0)
history_total_deaths <- integer(0)
previous_day_deaths <- 0  # Initialize the previous day's sick count to 0

# Initialize a vector to store daily death counts
daily_deaths <- integer(0)

# Simulate the pandemic
for (day in 1:N_days) {
  
  # Extract relevant indices for sick individuals and increment day of sickness count
  sick_indices <- which(health_state == 1)
  day_count[sick_indices] <- day_count[sick_indices] + 1
  
  # Determine mortality using a function to generate random numbers
  mortality <- runif(length(sick_indices)) < mortality_rate
  health_state[sick_indices[mortality]] <- 2  # 2 represents dead
  recovered_indices <- sick_indices[!mortality & day_count[sick_indices] > infection_period]
  health_state[recovered_indices] <- 3  # 3 represents immune
  day_count[recovered_indices] <- 0
  
  # Determine who meets whom
  num_to_meet_matrix <- sample(0:meet_people, size = length(sick_indices), replace = TRUE)
  num_random_meetings <- sum(num_to_meet_matrix) # Total number of meetings
  meet_indices <- sample(N_population, size = num_random_meetings, replace = TRUE) # Indices to be met
  meet_health <- health_state[meet_indices]
  
  # Filter to select healthy and immune individuals meeting sick people
  idx_to_infect <- meet_indices[meet_health == 0 | meet_health == 3]
  
  # Determine infection using a function to generate random numbers and consider immunity
  infection_probs <- runif(length(idx_to_infect))
  immune_individuals <- health_state[idx_to_infect] == 3
  adjusted_infection_rate <- ifelse(immune_individuals, infection_rate * immunity_coeff, infection_rate)
  infection_success <- infection_probs < adjusted_infection_rate
  
  # Update health state for newly infected individuals
  new_infections <- idx_to_infect[infection_success]
  health_state[new_infections] <- 1
  
  # Calculate daily statistics
  total_healthy <- sum(health_state %in% c(0, 3))
  total_sick <- sum(health_state == 1)
  total_deaths <- sum(health_state == 2)
  new_infections <- length(new_infections)
  
  # Calculate daily deaths by subtracting total deaths from previous day's deaths
  daily_death_count <- total_deaths - previous_day_deaths
  # Update the previous day's deaths count for the next iteration
  previous_day_deaths <- total_deaths
  
  # Remove dead people from population
  N_population <- N_population - daily_death_count
  
  # Append daily results to the data frame
  results_df <- rbind(
    results_df,
    data.frame(Day = day, TotalHealthy = total_healthy, TotalSick = total_sick, 
               TotalDeaths = total_deaths, NewInfections = new_infections,
               DailyDeaths = daily_death_count)
  )
  # Update historical data
  history_total_sick <- c(history_total_sick, total_sick)
  history_total_healthy <- c(history_total_healthy, total_healthy)
  history_total_deaths <- c(history_total_deaths, total_deaths)
}
print(results_df)

# Plot the history of infection
plot(results_df$Day, history_total_sick, type = "l", col = "red", xlab = "Day", ylab = "Number of People", main = "History of Infection")
lines(results_df$Day, history_total_healthy, col = "blue")
lines(results_df$Day, history_total_deaths, col = "green")

# Add a legend at the custom "middle right" position

legend('topright', legend = c("Cumulative Sick", "Cumulative Healthy", "Cumulative Dead"), col = c("red", "blue", "green"), lty = 1)

# A higher mortality rate can deter people from engaging in risky behaviors that 
# contribute to disease transmission (e.g., practicing better hygiene or avoiding 
# close contact with the sick). If more infected individuals die, they spend less 
# time in the infectious state, which can reduce the opportunities for transmission. 
# This, in turn, can slow infection growth. A lower infection rate means that each 
# infected person is less likely to transmit the disease to others. This can 
# significantly slow the spread of the disease within the population. If individuals 
# remain infectious for a longer time, they have more opportunities to transmit the 
# disease to others, potentially increasing infection growth. On the other hand, if 
# individuals recover more slowly, they might be isolated or receive medical care, 
# which can reduce transmission. The net effect depends on other factors. Fewer 
# interactions between individuals mean that the disease has fewer opportunities 
# to spread, which can substantially slow down infection growth.


