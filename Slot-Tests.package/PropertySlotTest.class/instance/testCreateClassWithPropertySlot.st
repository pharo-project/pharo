tests
testCreateClassWithPropertySlot
	"Add instance variables using the builder interface"
	
	| propertySlot |
	
	propertySlot := #prop => PropertySlot.
	
	aClass := self make: [ :builder |
		builder 
			slots: {propertySlot}
		].

	self assert: (aClass hasSlot: propertySlot).
	self assert: (aClass classLayout hasSlot: (propertySlot instVarNamed: 'baseSlot'))
	
