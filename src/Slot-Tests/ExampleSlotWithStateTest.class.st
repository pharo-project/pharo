Class {
	#name : #ExampleSlotWithStateTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
ExampleSlotWithStateTest >> testExampleClassSide [
	" can we install a slot on the class side?"
	aClass := self make: [ :builder |
		builder 
			classSlots: {#slot1 =>ExampleSlotWithState}
		].

	self assert: (aClass class hasSlotNamed: #slot1).
	self assert: aClass class slotDefinitionString equals:  '{ #slot1 => ExampleSlotWithState }'.
]

{ #category : #tests }
ExampleSlotWithStateTest >> testExampleSlotWithState [
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

{ #category : #tests }
ExampleSlotWithStateTest >> testExampleTwoSlotWithState [
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
