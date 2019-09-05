"
This class is the place for tests related to the Slot examples. 
(some examples have their own test class, the rest is here)
"
Class {
	#name : #AccessorInstanceVariableSlotTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
AccessorInstanceVariableSlotTest >> testAccessorInstanceVariableSlot [
	| slot object |
	aClass := self
		make:
			[ :builder | builder slots: {(#slot1 => AccessorInstanceVariableSlot)} ].
	self assert: (aClass hasSlotNamed: #slot1).

	"test reflective write and read"
	slot := aClass slotNamed: #slot1.
	object := aClass new.
	slot write: 5 to: object.
	self assert: (slot read: object) equals: 5.
	object slot1: 10.
	self assert: object slot1 equals: 10.
	
	"did we create accessors?"
	self assert: (aClass includesSelector: #slot1).
	self assert: (aClass includesSelector: #slot1:)
]
