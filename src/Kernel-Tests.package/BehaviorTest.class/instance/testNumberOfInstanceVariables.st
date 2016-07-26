metrics
testNumberOfInstanceVariables
	self assert: Object numberOfInstanceVariables = 0.
	self assert: Point numberOfInstanceVariables = 2.
	self assert: Metaclass numberOfInstanceVariables = 3