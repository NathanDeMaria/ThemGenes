
source('../R/algorithm.R')
context('Initial test')

test_that('Initial gen works', {
	
	result <- create_initial_gen(10)
	
	expect_is(result, 'data.table')
	expect_equal(nrow(result), 10)	
})
