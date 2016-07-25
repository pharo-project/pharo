asserting
assert: aBlock description: aStringOrBlock
	"Throw an assertion error if aBlock does not evaluates to true."
	
	aBlock value 
		ifFalse: [ AssertionFailure signal: aStringOrBlock value ]