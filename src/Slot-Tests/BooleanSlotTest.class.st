"
Tests for BooleanSlot
"
Class {
	#name : #BooleanSlotTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
BooleanSlotTest >> testExampleBooleanSlot [
	| object slot |
	slot := #slot1 => BooleanSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	self assert: (slot instVarNamed: 'baseSlot') isNotNil.

	object := aClass new.
	"test reflective write and read"
	slot write: true to: object.
	self assert: (slot read: object).

	"compiled accessors to test code gen"
	self compileAccessorsFor: slot.

	self assert: object slot1 identicalTo: true.
	object slot1: false.
	self assert: object slot1 identicalTo: false
]

{ #category : #tests }
BooleanSlotTest >> testExampleTwoBooleanSlots [
	| object slot1 slot2 |
	slot1 := #slot1 => BooleanSlot.
	slot2 := #slot2 => BooleanSlot.

	aClass := self make: [ :builder | builder slots: {slot1 . slot2} ].

	self assert: (slot1 instVarNamed: 'baseSlot') isNotNil.
	self assert: (slot1 instVarNamed: 'baseSlot') identicalTo: (slot2 instVarNamed: 'baseSlot').

	object := aClass new.
	"test reflective write and read"
	slot1 write: true to: object.
	self assert: (slot1 read: object) equals: true.

	slot2 write: true to: object.
	self assert: (slot2 read: object) equals: true.

	slot2 write: true to: object.
	slot1 write: false to: object.
	self assert: (slot2 read: object) equals: true.
	slot2 write: false to: object.
	self assert: (slot2 read: object) equals: false
]

{ #category : #tests }
BooleanSlotTest >> testExampleTwoBooleanSlotsRemoveOne [
	| object slot1 slot2 |
	slot1 := #slot1 => BooleanSlot.
	slot2 := #slot2 => BooleanSlot.

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
	self deny: (slot2 read: object).

	aClass removeSlot: slot1
]
