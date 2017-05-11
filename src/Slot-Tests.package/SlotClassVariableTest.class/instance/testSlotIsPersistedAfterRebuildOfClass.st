tests
testSlotIsPersistedAfterRebuildOfClass
	| class1 |	
	
	class1 := self make: [ :builder | 
		builder
			name: self aClassName;
			superclass: Object ].
		
	class1 class addSlot: (AccessorInstanceVariableSlot named: #Foo).

	self assert: class1 class slots size equals: 1.
	self assert: (class1 class slots at:1) name equals: #Foo.
	self assert: (class1 class slots at:1) class equals: AccessorInstanceVariableSlot.
	
	class1 := self make: [ :builder | 
		builder
			name: self aClassName;
			superclass: Object;
			copyClassSlotsFromExistingClass ].

	self assert: class1 class slots size equals: 1.
	self assert: (class1 class slots at:1) name equals: #Foo.
	self assert: (class1 class slots at:1) class equals: AccessorInstanceVariableSlot.
