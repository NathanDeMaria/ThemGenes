require(data.table)

# Implement genetic algorithm

# create initial generation ####
create_initial_gen <- function(count, init_means = 0, init_sd = 1) {
	
	initial_gen <- data.table(w = rnorm(count, init_means, init_sd), 
							  x = rnorm(count, init_means, init_sd), 
							  y = rnorm(count, init_means, init_sd), 
							  z = rnorm(count, init_means, init_sd))
	
	initial_gen
}

# check fitness
check_fitness <- Vectorize(function(w, x, y, z) {
	
	objective_function(w, x, y, z)
})


# have babies
make_babies <- function(generation, expected_babies = 1) {
		
	parents <- get_parents(generation, expected_babies = 1)
}

get_parents <- function(generation, expected_babies = 1) {

	generation$baby_odds <- calc_baby_odds(generation$errors, expected_babies)
	
	generation[baby_odds > runif(length(baby_odds)),]
}

calc_baby_odds <- function(errors, expected_babies = 1) {
	
	raw <- expected_babies * (1 - errors / sum(errors, na.rm = T))
	adjustment <- (length(errors[!is.nan(errors)]) - 1)
	raw / adjustment
}

# die
