tests
testExampleSlotWithDefaultValue
	| slot object|
	
	aClass := self make: [ :builder |
		builder 
			slots: {#slot1 => ExampleSlotWithDefaultValue default: 5  }
		].

	self assert: (aClass hasSlotNamed: #slot1).
	
	"test refletive write and read"
	slot := aClass slotNamed: #slot1.
	object := aClass new.

	self assert: (slot read: object) = 5.
	slot write: 10 to: object.
	self assert: (slot read: object) = 10.
	
	
	
	
	