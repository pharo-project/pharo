"
Automated tests for WriteOnceSlot
"
Class {
	#name : 'WriteOnceSlotTest',
	#superclass : 'SlotSilentTest',
	#category : 'Slot-Tests-Examples',
	#package : 'Slot-Tests',
	#tag : 'Examples'
}

{ #category : 'tests' }
WriteOnceSlotTest >> testClassSide [
	"test that we can change a normal slot into a WriteOnceSlot. Tests class side slot init"
	| slot |
	slot := #slot1 => InstanceVariableSlot.
	aClass := self make: [ :builder | builder classSlots: {slot} ].
	slot := #slot1 => WriteOnceSlot.
	aClass := self make: [ :builder | builder classSlots: {slot} ].

	self assert: (aClass class hasSlotNamed: #slot1)
]

{ #category : 'tests' }
WriteOnceSlotTest >> testRead [
	| slot |
	<ignoreNotImplementedSelectors: #(slot1)>
	slot := #slot1 => WriteOnceSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	"now compile an accessor and read"
	self compileAccessorsFor: slot.
	self assert: aClass new slot1 equals: nil
]

{ #category : 'tests' }
WriteOnceSlotTest >> testWriteAndRead [
	| slot object |
	<ignoreNotImplementedSelectors: #(slot1: slot1)>
	slot := #slot1 => WriteOnceSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	"now compile an accessor and read"
	self compileAccessorsFor: slot.
	object := aClass new.
	self assert: object slot1 equals: nil.
	object slot1: 3.
	self assert: object slot1 equals: 3
]

{ #category : 'tests' }
WriteOnceSlotTest >> testWriteTwice [
	| slot object |
	<ignoreNotImplementedSelectors: #(slot1: slot1)>
	slot := #slot1 => WriteOnceSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	"now compile an accessor and read"
	self compileAccessorsFor: slot.
	object := aClass new.
	self assert: object slot1 equals: nil.
	"write first time"
	object slot1: 3.
	self assert: object slot1 equals: 3.
	"second write is error"
	self should: [ object slot1: 27 ] raise: Error
]
