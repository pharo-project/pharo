tests-class slots
testAddClassSlotAndMigrate

	| classAdded classModified |
	classAdded := 
		self make: [ :builder | 
			builder classSlots: #(a) ].

	classAdded instVarNamed: #a put: $A.
	
	classModified := 
		self make: [ :builder | 
			builder classSlots: #(a b) ].
	
	self assert: (classModified instVarNamed: #a) equals: $A.
	self assert: (classModified instVarNamed: #b) isNil.