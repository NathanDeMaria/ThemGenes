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

# check fitness ####
check_fitness <- Vectorize(function(w, x, y, z) {
	
	objective_function(w, x, y, z)
})


# have babies ####
make_babies <- function(generation, expected_babies = 5, baby_sd = 1) {
		
	parents <- get_parents(generation, expected_babies = expected_babies)
	
	parents[,w:=ifelse(runif(length(w)) < .25, rnorm(1, mean=w, sd=baby_sd), w)]
	parents[,x:=ifelse(runif(length(x)) < .25, rnorm(1, mean=x, sd=baby_sd), x)]
	parents[,y:=ifelse(runif(length(y)) < .25, rnorm(1, mean=y, sd=baby_sd), y)]
	parents[,z:=ifelse(runif(length(z)) < .25, rnorm(1, mean=z, sd=baby_sd), z)]
	parents
}

get_parents <- function(generation, expected_babies = 1) {

	generation$baby_odds <- calc_baby_odds(generation$error, expected_babies)
	
	generation[baby_odds > runif(length(baby_odds)),]
}

calc_baby_odds <- function(errors, expected_babies = 1) {
	
	raw <- expected_babies * (1 - errors / sum(errors, na.rm = T))
	adjustment <- (length(errors[!is.nan(errors)]) - 1)
	raw / adjustment
}

# die ####
get_survivors <- function(generation, expected_deaths = 1) {
	
	generation[,death_odds:=expected_deaths * error/sum(error, na.rm=T)]
	
	generation[death_odds < runif(length(death_odds)),]
}

# next gen ####
next_generation <- function(generation, 
							expected_babies = 1,
							expected_deaths = 1) {
	
	generation[, error:=check_fitness(w,x,y,z)]
	babies <- make_babies(generation, expected_babies)
	survivors <- get_survivors(generation, expected_deaths)
	
	rbindlist(list(babies[,list(w,x,y,z)], survivors[,list(w,x,y,z)]))
}

