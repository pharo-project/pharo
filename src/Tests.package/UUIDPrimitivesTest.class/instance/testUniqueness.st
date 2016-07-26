tests
testUniqueness 
	"Test uniqueness for a given number of generated values"
	
	|maxTestValue|
	maxTestValue := 1000.
	self assert: ((1 to: maxTestValue) collect: [ :i | UUID new asString ]) asSet size = maxTestValue