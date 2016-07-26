tests - ast mapping
testSourceNodeExecuted
	| sourceNode |
	
	sourceNode := thisContext sender sender sourceNodeExecuted.
	self assert: sourceNode selector = #performTest.