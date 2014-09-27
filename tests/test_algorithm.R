
source('../R/objective.R')
source('../R/algorithm.R')
context('Algorithm')

test_that('Initial gen works', {
	
	result <- create_initial_gen(10)
	
	expect_is(result, 'data.table')
	expect_equal(nrow(result), 10)	
})

test_that('Checking error', {
	
	generation <- data.table(w = c(1, 2, 3),
							 x = c(1, NaN, 3),
							 y = c(1, 2, 3),
							 z = c(1, 2, 3))
	
	generation[, error:=check_fitness(w,x,y,z)]
	
	expect_is(generation$error, 'numeric')
})

test_that('Check baby odds', {
	
	odds <- calc_baby_odds(c(1:10, NaN), 1)
	
	expect_equal(sum(odds, na.rm = T), 1)
})

test_that('Get parents', {
	
	generation <- data.table(w = c(1, 2, 3),
							 x = c(1, NaN, 3),
							 y = c(1, 2, 3),
							 z = c(1, 2, 3),
							 error = c(1, NaN, 1000))
	
	result <- get_parents(generation, 1)
	
	expect_is(result, 'data.table')
})

test_that('Make babies', {
	
	generation <- data.table(w = c(1, 2, 7),
							 x = c(9, 3, 3),
							 y = c(3, NaN, 2),
							 z = c(2, 8, 5),
							 error = c(1, NaN, 1000))
	
	babies <- make_babies(generation)
	
	expect_equal(nrow(babies), nrow(generation))
	expect_is(babies, 'data.table')
})

