tests
testCreateClassWithTwoPropertySlots
	"Add instance variables using the builder interface"
	
	| propertySlot1 propertySlot2 |
	
	propertySlot1 := #prop1 => PropertySlot.
	propertySlot2 := #prop2 => PropertySlot.
	
	aClass := self make: [ :builder |
		builder 
			slots: {propertySlot1 . propertySlot2 }
		].

	self assert: (aClass hasSlot: propertySlot1).
	self assert: (aClass classLayout hasSlot: (propertySlot1 instVarNamed: 'baseSlot')).
	
	self assert: (aClass hasSlot: propertySlot2).
	self assert: (aClass classLayout  hasSlot: (propertySlot2 instVarNamed: 'baseSlot'))
	
	
	