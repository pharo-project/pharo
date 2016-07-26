testing
testClassEquality
	| newClass |
	
	self assert: OrderedCollection asRingDefinition = OrderedCollection asRingDefinition.
	
	newClass := (OrderedCollection asRingDefinition)
		category: #Kernel.
	self assert: (OrderedCollection asRingDefinition = newClass) 