Class {
	#name : #HistorySlotTest,
	#superclass : #SlotSilentTest,
	#category : #'VariablesLibrary-Tests-Tests'
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
HistorySlotTest >> testAddOneElementAndModifyClass [
	| slot instance |
	slot := #slot1 => HistorySlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	self compileAccessorsFor: slot.

	instance := aClass new.
	instance slot1: 25.
	
	aClass addInstVarNamed: 'x'.
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
	self assert: (slot readHistory: instance) asArray equals: #(nil 25 35)
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

  	self assert: (instance slot1) equals: nil
]

{ #category : #tests }
HistorySlotTest >> testModifyIvarToHistory [
	"test if we can transform a standard ivar to a history slot when instances are present"

	| slot instance |
	slot := #slot1 =>InstanceVariableSlot .
	aClass := self make: [ :builder | builder slots: {slot} ].

	self compileAccessorsFor: slot.

	instance := aClass new.
	instance slot1: 25.
	slot := #slot1 =>HistorySlot.
	aClass := self make: [ :builder | builder slots: {slot} ].
	
	self assert: instance slot1 equals: 25.
	self assert: (slot readHistory: instance) asArray equals: #(nil 25)
]
