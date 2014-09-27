
source('R/algorithm.R')
source('R/objective.R')
source('Helpers/progressBar.R')

set.seed(18)
generation <- create_initial_gen(100)

init_errors <- generation[, check_fitness(w,x,y,z)]

iterations <- 2e3

bar <- load_progress_bar()
bar$init(length = iterations, title = 'Generations')

for(i in 1:iterations) {
	
	generation <- next_generation(generation, 15, 15, baby_sd = 2)
	bar$inc()
}
bar$close()
end_errors <- generation[, check_fitness(w,x,y,z)]
plot(init_errors, ylim=c(0, 70))
points(end_errors, pch=19)
