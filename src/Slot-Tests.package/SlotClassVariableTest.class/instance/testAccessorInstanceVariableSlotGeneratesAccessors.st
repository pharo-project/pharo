tests
testAccessorInstanceVariableSlotGeneratesAccessors
	| class1 |	
	
	class1 := self make: [ :builder | 
		builder
			name: self aClassName;
			superclass: Object ].
		
	class1 class addSlot: (AccessorInstanceVariableSlot named: #foo).

	self assert: (class1 class canUnderstand: #foo).
	self assert: (class1 class canUnderstand: #foo:).