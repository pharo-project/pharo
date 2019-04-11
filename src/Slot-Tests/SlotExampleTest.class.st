"
This class is the place for tests related to the Slot examples. 
(some examples have their own test class, the rest is here)
"
Class {
	#name : #SlotExampleTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
SlotExampleTest >> testAccessorInstanceVariableSlot [
	| slot object |
	aClass := self
		make:
			[ :builder | builder slots: {(#slot1 => AccessorInstanceVariableSlot)} ].
	self assert: (aClass hasSlotNamed: #slot1).

	"test refletive write and read"
	slot := aClass slotNamed: #slot1.
	object := aClass new.
	slot write: 5 to: object.
	self assert: (slot read: object) equals: 5.
	object slot1: 10.
	self assert: object slot1 equals: 10.
	
	"did we create accessors?"
	self assert: (aClass includesSelector: #slot1).
	self assert: (aClass includesSelector: #slot1:).
]

{ #category : #tests }
SlotExampleTest >> testExampleClassSide [
	" can we install a slot on the class side?"
	aClass := self make: [ :builder |
		builder 
			classSlots: {#slot1 =>ExampleSlotWithState}
		].

	self assert: (aClass class hasSlotNamed: #slot1).
	self assert: aClass class slotDefinitionString equals:  '{ #slot1 => ExampleSlotWithState }'.
]

{ #category : #'test - unlimited ivars' }
SlotExampleTest >> testExampleIvarSlot [

	| object slot reader writer |
	
	slot := #slot1 => UnlimitedInstanceVariableSlot.

	aClass := self make: [ :builder |
		builder 
			slots: {slot}
		].
	self assert: (slot instVarNamed: 'baseSlot') isNotNil.

	object := aClass new. 
	"test refletive write and read"
	slot write: 5 to: object.
	self assert: (slot read: object) == 5.
	
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
	
	self assert: object slot1 == 5.
	object slot1: false.
	self deny: object slot1.
]

{ #category : #tests }
SlotExampleTest >> testExampleSlotWithDefaultValue [
	| slot object|
	
	aClass := self make: [ :builder |
		builder 
			slots: {#slot1 => ExampleSlotWithDefaultValue default: 5  }
		].

	self assert: (aClass hasSlotNamed: #slot1).
	
	"test refletive write and read"
	slot := aClass slotNamed: #slot1.
	object := aClass new.

	self assert: (slot read: object) equals: 5.
	slot write: 10 to: object.
	self assert: (slot read: object) equals: 10.
	
	
	
	
	
]

{ #category : #tests }
SlotExampleTest >> testExampleSlotWithState [
	| slot reader writer |
	aClass := self
		make:
			[ :builder | builder slots: {(#slot1 => ExampleSlotWithState)} ].
	self assert: (aClass hasSlotNamed: #slot1).

	"test refletive write and read"
	slot := aClass slotNamed: #slot1.
	slot write: 5 to: aClass new.
	self assert: (slot read: aClass new) equals: 5.
	reader := String
		streamContents: [ :str | 
			str
				nextPutAll: slot name;
				cr;
				tab;
				nextPutAll: ' ^';
				nextPutAll: slot name ].
	writer := String
		streamContents: [ :str | 
			str
				nextPutAll: slot name;
				nextPutAll: ': anObject';
				cr;
				tab;
				nextPutAll: slot name;
				nextPutAll: ':= anObject.' ].
	aClass compile: reader classified: 'accessing'.
	aClass compile: writer classified: 'accessing'.
	aClass new slot1: 10.
	self assert: aClass new slot1 equals: 10
]

{ #category : #'test - unlimited ivars' }
SlotExampleTest >> testExampleTwoIvarSlots [

	| object slot1 slot2 |
	
	slot1 := #slot1 => UnlimitedInstanceVariableSlot.
	slot2 := #slot2 => UnlimitedInstanceVariableSlot.

	aClass := self make: [ :builder |
		builder 
			slots: {slot1 . slot2 }
		].

	self assert: (slot1 instVarNamed: 'baseSlot') isNotNil.
	self assert: (slot1 instVarNamed: 'baseSlot') == (slot2 instVarNamed: 'baseSlot').

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

{ #category : #tests }
SlotExampleTest >> testExampleTwoSlotWithState [
	" add two, remove one"
	aClass := self make: [ :builder |
		builder 
			slots: {#slot1 =>ExampleSlotWithState . #slot2 =>ExampleSlotWithState}
		].

	self assert: (aClass hasSlotNamed: #slot1).
	self assert: (aClass hasSlotNamed: #slot2).
	
	aClass := self make: [ :builder |
		builder 
			slots: {#slot2 =>ExampleSlotWithState}
		].
	self deny: (aClass hasSlotNamed: #slot1).
	self assert: (aClass hasSlotNamed: #slot2).
]
