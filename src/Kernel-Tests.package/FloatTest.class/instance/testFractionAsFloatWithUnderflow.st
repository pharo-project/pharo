tests
testFractionAsFloatWithUnderflow
	"test rounding to nearest even"

	| underflowPower |
	underflowPower := Float emin - Float precision.
	self assert: (2 raisedTo: underflowPower) asFloat = 0.0.	
	self assert: (2 raisedTo: underflowPower) negated asFloat = 0.0.
	self assert: (2 raisedTo: underflowPower) negated asFloat sign = -1 description: 'a negative underflow should return a negative zero'.