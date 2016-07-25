testing
testTraitEquality

	| newClass |
	self assert: TSortable asRingDefinition = TSortable asRingDefinition.
	
	newClass := (TSortable asRingDefinition)
		category: #Kernel.
	self assert: (TSortable asRingDefinition = newClass) 