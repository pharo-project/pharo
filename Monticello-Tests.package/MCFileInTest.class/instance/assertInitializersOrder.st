testing
assertInitializersOrder

	| initializationOrder |
	initializationOrder := self mockClassA initializationOrder.
	self assert: initializationOrder = 2.	
