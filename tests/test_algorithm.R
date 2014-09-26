
source('../R/algorithm.R')
context('Algorithm')

test_that('Initial gen works', {
	
	result <- create_initial_gen(10)
	
	expect_is(result, 'data.table')
	expect_equal(nrow(result), 10)	
})

test_that('Checking fitness', {
	
	generation <- data.table(w = c(1, 2, 3),
							 x = c(1, NaN, 3),
							 y = c(1, 2, 3),
							 z = c(1, 2, 3))
	
	generation[,fitness:=check_fitness(w,x,y,z)]
	
	expect_is(generation$fitness, 'numeric')
})
