tests - ast mapping
testSourceNodeExecutedWhenContextIsJustAtStartpc
	| sourceNode context |
	
	context := Context sender: thisContext receiver: self method: self class >> testSelector arguments: #().
	sourceNode := context sourceNodeExecuted.
	self assert: sourceNode equals: (self class >> testSelector) ast sendNodes first receiver