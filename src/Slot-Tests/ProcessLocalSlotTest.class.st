Class {
	#name : #ProcessLocalSlotTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
ProcessLocalSlotTest >> testIsPossibleToSetSlotInBlock [
	| slot writerString instance |
	slot := #slot1 => ProcessLocalSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	writerString := String streamContents: [ :str |
		str 
			nextPutAll: slot name;
			nextPutAll: ': anObject';
			cr;tab;
			nextPutAll: 'true ifTrue: [ ';
			nextPutAll: slot name;
			nextPutAll: ':= anObject ]'.
		].
	
	aClass compile: writerString.
	
	instance := aClass new.
	instance slot1: #test.
	self assert: (instance instVarNamed: #slot1) value equals: #test
]

{ #category : #tests }
ProcessLocalSlotTest >> testRead [
	| slot |
	slot := #slot1 => ProcessLocalSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].

	"now compile an accessor and read"
	self compileAccessorsFor: slot.
	self assert: aClass new slot1 equals: nil.
	
	"test refelective read"
	self assert: (slot read: aClass new) equals: nil
]

{ #category : #tests }
ProcessLocalSlotTest >> testWriteAndReadCompiled [
	| slot object |
	slot := #slot1 => ProcessLocalSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].
	object := aClass new.
	
	"now compile an accessor and read/write"
	self compileAccessorsFor: slot.
	self assert: object slot1 equals: nil.
	object slot1: 3.
	self assert: object slot1 equals: 3.
	"inside another process it it nil"
	[ self assert: object slot1 equals: nil. ] fork.
	self assert: object slot1 equals: 3
]

{ #category : #tests }
ProcessLocalSlotTest >> testWriteAndReadReflective [
	| slot object |
	slot := #slot1 => ProcessLocalSlot.
	aClass := self make: [ :builder | builder slots: {slot} ].
	object := aClass new.
	"now do reflective read and write"
	self assert: (slot read: object) equals: nil.
	slot write: 3 to: object.
	self assert: (slot read: object) equals: 3.
	"inside another process it it nil"
	[ self assert: (slot read: object) equals: nil. ] fork.
	self assert: (slot read: object) equals: 3
]
