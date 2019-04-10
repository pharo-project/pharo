"
Automated tests for WriteOnceSlot
"
Class {
	#name : #WriteOnceSlotTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
WriteOnceSlotTest >> testRead [
	| slot |
	slot := #slot1 => WriteOnceSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	"now compile an accessor and read"
	self compileAccessorsFor: slot.
	self assert: aClass new slot1 equals: nil
]

{ #category : #tests }
WriteOnceSlotTest >> testWriteAndRead [
	| slot object |
	slot := #slot1 => WriteOnceSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	"now compile an accessor and read"
	self compileAccessorsFor: slot.
	object := aClass new.
	self assert: object slot1 equals: nil.
	object slot1: 3.
	self assert: object slot1 equals: 3
]

{ #category : #tests }
WriteOnceSlotTest >> testWriteTwice [
	| slot object |
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
