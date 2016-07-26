tests - on-fork
testOnFork
	"Test that if code runs without errors, there is no fork! "
	
	| result1 result2 |
	
	result2 := nil.
	result1 := [ 1 ] on: Exception fork: [ result2 := 2 ].
	
	Processor yield.
	
	self assert: (result1 = 1).
	self assert: (result2 isNil ).