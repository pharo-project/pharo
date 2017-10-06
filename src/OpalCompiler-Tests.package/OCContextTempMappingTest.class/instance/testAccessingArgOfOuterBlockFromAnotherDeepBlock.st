tests
testAccessingArgOfOuterBlockFromAnotherDeepBlock
	
	| actual |
	
	"Check the source code availability to do not fail on images without sources"
	thisContext method hasSourceCode ifTrue: [ ^ self ].
	
	actual := [:outerArg | 
		outerArg asString.
		[ :innerArg | innerArg asString. thisContext tempNamed: #outerArg ] value: #innerValue.
		] value: #outerValue.
	
	self assert: actual equals: #outerValue