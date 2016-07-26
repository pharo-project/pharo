test
demoMethodWithDuplicates
	| a b result1 result2 answer |
	a := 3.
	b := a + 5.
	result1 := a + b.
	result2 := a + 5.
	answer := result1 + result2 + (a + 5).
	^ answer