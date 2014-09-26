
# The function to be optimized 
objective_function <- function(w, x, y, z) {
	
	# different components make it hard:
		# w == 30, linear
		# x == 0, scaled
		# y == 0, but breaks if negative
		# z == 1, exponential
	
	abs(w - 30) + abs(2 * x) + abs(suppressWarnings(sqrt(y))) + abs(10^z - 10)
}