
source('R/algorithm.R')
source('R/objective.R')
source('Helpers/progressBar.R')

set.seed(18)
generation <- create_initial_gen(50)

errors <- data.table(generation=0, error=generation[, check_fitness(w,x,y,z)])
g <- 1

moving <- reactive({
	invalidateLater(2000, NULL)
	generation <<- next_generation(generation, 5, 5, baby_sd = 3)
	errors <<- rbindlist(list(errors, data.table(generation = g, error = generation[,check_fitness(w,x,y,z)])))
	g <<- g + 1
	data.frame(errors[error < 60 & generation > (g - 20)])
})

moving %>% ggvis() %>% layer_points(~generation, ~error)

