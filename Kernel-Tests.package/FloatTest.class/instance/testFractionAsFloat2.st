tests - conversion
testFractionAsFloat2
	"test rounding to nearest even"
		
	self assert: ((1<<52)+0+(1/4)) asFloat asTrueFraction = ((1<<52)+0).
	self assert: ((1<<52)+0+(1/2)) asFloat asTrueFraction = ((1<<52)+0).
	self assert: ((1<<52)+0+(3/4)) asFloat asTrueFraction = ((1<<52)+1).
	self assert: ((1<<52)+1+(1/4)) asFloat asTrueFraction = ((1<<52)+1).
	self assert: ((1<<52)+1+(1/2)) asFloat asTrueFraction = ((1<<52)+2).
	self assert: ((1<<52)+1+(3/4)) asFloat asTrueFraction = ((1<<52)+2).