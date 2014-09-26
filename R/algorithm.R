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


# die
