tests-class slots
testRemoveClassSlotAndMigrate

	| classAdded classModified |
	classAdded := 
		self make: [ :builder | 
			builder classSlots: #(a b) ].

	classAdded instVarNamed: #a put: $A.
	
	classModified := 
		self make: [ :builder | 
			builder classSlots: #(a) ].
	
	self assert: (classModified instVarNamed: #a) equals: $A.
	self should: [ classModified instVarNamed: #b ] raise: Error
