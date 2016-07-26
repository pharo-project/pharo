tests-class slots
testWithoutClassSlots

	aClass := self make: [ :builder | 
		builder classSlots: #() ].
	
	self assert: aClass classVarNames isEmpty