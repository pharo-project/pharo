asserting
assert: aBlock descriptionBlock: descriptionBlock
	"Throw an assertion error if aBlock does not evaluate to true."
	
	aBlock value ifFalse: [AssertionFailure signal: descriptionBlock value asString ]