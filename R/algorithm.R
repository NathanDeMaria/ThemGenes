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
make_babies <- function(generation, expected_babies = 1) {
		
	parents <- get_parents(generation, expected_babies = 1)
	
	# you are literally the worst. data.table this
	data.table(t(sapply(1:nrow(parents), function(row_num) {
		make_baby(data.frame(parents)[row_num,])
	})))
}

make_baby <- function(row, baby_sd = 1) {

	w <- row[,'w']
	x <- row[,'x']
	y <- row[,'y']
	z <- row[,'z']
		
	data.table(w = ifelse(runif(1) < .25, rnorm(1, mean=w, sd=baby_sd), w),
			   x = ifelse(runif(1) < .25, rnorm(1, mean=x, sd=baby_sd), x),
			   y = ifelse(runif(1) < .25, rnorm(1, mean=y, sd=baby_sd), y),
			   z = ifelse(runif(1) < .25, rnorm(1, mean=z, sd=baby_sd), z))
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

