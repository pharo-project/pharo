tests
testExampleClassSide
	" can we install a slot on the class side?"
	aClass := self make: [ :builder |
		builder 
			classSlots: {#slot1 =>ExampleSlotWithState}
		].

	self assert: (aClass class hasSlotNamed: #slot1).
	self assert: aClass class slotDefinitionString =  '{ #slot1 => ExampleSlotWithState }'.