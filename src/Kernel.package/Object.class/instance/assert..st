asserting
assert: aBlock
	"Throw an assertion error if aBlock does not evaluates to true.
	We check for true explicitly to make the assertion fail for non booleans"
	
	aBlock value == true ifFalse: [AssertionFailure signal: 'Assertion failed']