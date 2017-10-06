tests
testExampleTwoIvarSlots

	| object slot1 slot2 |
	
	slot1 := #slot1 => UnlimitedInstanceVariableSlot.
	slot2 := #slot2 => UnlimitedInstanceVariableSlot.

	aClass := self make: [ :builder |
		builder 
			slots: {slot1 . slot2 }
		].

	self assert: (slot1 instVarNamed: 'baseSlot') isNotNil.
	self assert: (slot1 instVarNamed: 'baseSlot') == (slot2 instVarNamed: 'baseSlot').

	object := aClass new. 
	"test reflective write and read"
	slot1 write: true to: object.
	self assert: (slot1 read: object) = true.
	
	slot2 write: true to: object.
	self assert: (slot2 read: object) = true.
	
	slot2 write: true to: object.
	slot1 write: false to: object.
	self assert: (slot2 read: object) = true.
	slot2 write: false to: object.
	self assert: (slot2 read: object) = false.
	
	
	