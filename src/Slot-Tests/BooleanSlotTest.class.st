Class {
	#name : #BooleanSlotTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
BooleanSlotTest >> testExampleBooleanSlot [

	| object slot reader writer |
	
	slot := #slot1 => BooleanSlot.

	aClass := self make: [ :builder |
		builder 
			slots: {slot}
		].

	self assert: (slot instVarNamed: 'baseSlot') isNotNil.

	object := aClass new. 
	"test refletive write and read"
	slot write: true to: object.
	self assert: (slot read: object) = true.
	
		"compiled accessors to test code gen"
	reader := String streamContents: [ :str |
		str 
			nextPutAll: slot name;
			cr;tab;
			nextPutAll: ' ^';
			nextPutAll: slot name.
		 ].
	writer := String streamContents: [ :str |
		str 
			nextPutAll: slot name;
			nextPutAll: ': anObject';
			cr;tab;
			nextPutAll: slot name;
			nextPutAll: ':= anObject.'.
		].
	aClass compile: reader classified: 'accessing'.
	aClass compile: writer classified: 'accessing'.
	
	self assert: object slot1 == true.
	object slot1: false.
	self assert: object slot1 == false.
]

{ #category : #tests }
BooleanSlotTest >> testExampleTwoBooleanSlots [

	| object slot1 slot2 |
	
	slot1 := #slot1 => BooleanSlot.
	slot2 := #slot2 => BooleanSlot.

	aClass := self make: [ :builder |
		builder 
			slots: {slot1 . slot2 }
		].

	self assert: (slot1 instVarNamed: 'baseSlot') isNotNil.
	self assert: (slot1 instVarNamed: 'baseSlot') == (slot2 instVarNamed: 'baseSlot').

	object := aClass new. 
	"test reflective write and read"
	slot1 write: true to: object.
	self assert: (slot1 read: object) = true.
	
	slot2 write: true to: object.
	self assert: (slot2 read: object) = true.
	
	slot2 write: true to: object.
	slot1 write: false to: object.
	self assert: (slot2 read: object) = true.
	slot2 write: false to: object.
	self assert: (slot2 read: object) = false.
	
	
	
]

{ #category : #tests }
BooleanSlotTest >> testExampleTwoBooleanSlotsRemoveOne [

	| object slot1 slot2 |
	
	slot1 := #slot1 => BooleanSlot.
	slot2 := #slot2 => BooleanSlot.

	aClass := self make: [ :builder |
		builder 
			slots: {slot1 . slot2 }
		].

	self assert: (slot1 instVarNamed: 'baseSlot') isNotNil.
	self assert: (slot1 instVarNamed: 'baseSlot') == (slot2 instVarNamed: 'baseSlot').

	object := aClass new. 
	"test reflective write and read"
	slot1 write: true to: object.
	self assert: (slot1 read: object) = true.
	
	slot2 write: true to: object.
	self assert: (slot2 read: object) = true.
	
	slot2 write: true to: object.
	slot1 write: false to: object.
	self assert: (slot2 read: object) = true.
	slot2 write: false to: object.
	self assert: (slot2 read: object) = false.
	
	aClass removeSlot: slot1.
	
	
	
]
