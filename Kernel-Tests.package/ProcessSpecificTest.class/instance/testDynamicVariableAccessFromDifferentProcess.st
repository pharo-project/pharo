testing
testDynamicVariableAccessFromDifferentProcess
	| process sem result |
	
	sem := Semaphore new.
	process := [  
		TestDynamicVariable 
			value: 123 
			during: [ sem wait ] ] fork.
	Processor yield.
	Processor activeProcess 
		evaluate: [  result := TestDynamicVariable value ] 
		onBehalfOf: process.
	sem signal.
	self assert: result = 123