
source('R/algorithm.R')
source('R/objective.R')
source('Helpers/progressBar.R')

set.seed(18)
generation <- create_initial_gen(50)

errors <- data.table(generation=0, error=generation[, check_fitness(w,x,y,z)])

iterations <- 2e3

bar <- load_progress_bar()
bar$init(length = iterations, title = 'Generations')

for(i in 1:iterations) {
	
	generation <- next_generation(generation, 5, 5, baby_sd = 3)
	errors <- rbindlist(list(errors, data.table(generation = i, error = generation[,check_fitness(w,x,y,z)])))
	bar$inc()
}
bar$close()

g <- ggplot(errors[error < 145]) + geom_point(aes(generation, error))
plot(g)