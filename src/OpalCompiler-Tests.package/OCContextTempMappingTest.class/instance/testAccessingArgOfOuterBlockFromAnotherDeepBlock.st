tests
testAccessingArgOfOuterBlockFromAnotherDeepBlock
	
	| actual |
	actual := [:outerArg | 
		outerArg asString.
		[ :innerArg | innerArg asString. thisContext tempNamed: #outerArg ] value: #innerValue.
		] value: #outerValue.
	
	self assert: actual equals: #outerValue