Class {
	#name : #HistorySlotTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
HistorySlotTest >> testAddOneElement [
	| slot instance |
	slot := #slot1 => HistorySlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	self compileAccessorsFor: slot.

	instance := aClass new.
	instance slot1: 25.
	self assert: instance slot1 equals: 25
]

{ #category : #tests }
HistorySlotTest >> testAddTwoElements [
	| slot instance |
	slot := #slot1 => HistorySlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	self compileAccessorsFor: slot.

	instance := aClass new.
	instance slot1: 25.
	instance slot1: 35.
	self assert: instance slot1 equals: 35
]

{ #category : #tests }
HistorySlotTest >> testAddTwoElementsHistoryRead [
	| slot instance |
	slot := #slot1 => HistorySlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	self compileAccessorsFor: slot.

	instance := aClass new.
	instance slot1: 25.
	instance slot1: 35.
	self assert: instance slot1 equals: 35.
	self assert: (slot readHistory: instance) size equals: 3
]

{ #category : #tests }
HistorySlotTest >> testHistoryGreaterThanThree [
	| slot instance |
	slot := #slot1 => HistorySlot size: 3.
	aClass := self make: [ :builder | builder slots: {slot} ].

	self compileAccessorsFor: slot.

	instance := aClass new.
	instance slot1: 25.
	instance slot1: 35.
	instance slot1: 35.
	instance slot1: 35.
	instance slot1: 35.
	instance slot1: 35.
	self assert: (slot readHistory: instance) size equals: 3
]

{ #category : #tests }
HistorySlotTest >> testIsFirstElementNil [

	| slot instance |
	slot := #slot1 => HistorySlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	self compileAccessorsFor: slot.

	instance := aClass new.

  	self assert: (instance slot1)  equals:  nil.
]
