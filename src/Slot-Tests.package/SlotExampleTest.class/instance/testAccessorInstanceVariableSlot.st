tests
testAccessorInstanceVariableSlot
	| slot object |
	
	aClass := self make: [ :builder |
		builder 
			slots: {#slot1 =>AccessorInstanceVariableSlot  }
		].

	self assert: (aClass hasSlotNamed: #slot1).
	
	"test refletive write and read"
	slot := aClass slotNamed: #slot1.
	object := aClass new.
	slot write: 5 to: object.
	self assert: (slot read: object) = 5.
	
	"test generated accessors"
	object slot1: 10.
	self assert: object slot1 = 10.
	
	
	
	
	