Class {
	#name : #UnlimitedInstanceVariableSlotTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #'test - unlimited ivars' }
UnlimitedInstanceVariableSlotTest >> testExampleIvarSlot [
	| object slot |
	slot := #slot1 => UnlimitedInstanceVariableSlot.

	aClass := self make: [ :builder | builder slots: {slot} ].
	self assert: (slot instVarNamed: 'baseSlot') isNotNil.

	object := aClass new.
	"test reflective write and read"
	slot write: 5 to: object.
	self assert: (slot read: object) identicalTo: 5.

	self compileAccessorsFor: slot.

	self assert: object slot1 identicalTo: 5.
	object slot1: false.
	self deny: object slot1
]

{ #category : #'test - unlimited ivars' }
UnlimitedInstanceVariableSlotTest >> testExampleTwoIvarSlots [
	| object slot1 slot2 |
	slot1 := #slot1 => UnlimitedInstanceVariableSlot.
	slot2 := #slot2 => UnlimitedInstanceVariableSlot.

	aClass := self make: [ :builder | builder slots: {slot1 . slot2} ].

	self assert: (slot1 instVarNamed: 'baseSlot') isNotNil.
	self assert: (slot1 instVarNamed: 'baseSlot') identicalTo: (slot2 instVarNamed: 'baseSlot').

	object := aClass new.
	"test reflective write and read"
	slot1 write: true to: object.
	self assert: (slot1 read: object).

	slot2 write: true to: object.
	self assert: (slot2 read: object).

	slot2 write: true to: object.
	slot1 write: false to: object.
	self assert: (slot2 read: object).
	slot2 write: false to: object.
	self deny: (slot2 read: object)
]
