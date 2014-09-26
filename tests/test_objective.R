
source('../R/objective.R')
context('Objective function')

test_that('Perfection returns 0', {
	
	result <- objective_function(30, 0, 0, 1)
	
	expect_equal(result, 0)
})

test_that('Errors return positive numbers', {
	
	result <- objective_function(1,2,3,4)
	
	expect_true(result > 0)
})

test_that('Negative y produces NaN', {
	
	result <- objective_function(30, 0, -1, 1)
	
	expect_true(is.nan(result))
})

