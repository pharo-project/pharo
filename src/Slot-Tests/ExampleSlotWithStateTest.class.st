Class {
	#name : #ExampleSlotWithStateTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
ExampleSlotWithStateTest >> testExampleClassSide [
	" can we install a slot on the class side?"
	aClass := self make: [ :builder | builder classSlots: {#slot1 =>ExampleSlotWithState}].

	self assert: (aClass class hasSlotNamed: #slot1).
	self assert: aClass class slotDefinitionString equals:  '{ #slot1 => ExampleSlotWithState }'
]

{ #category : #tests }
ExampleSlotWithStateTest >> testExampleSlotWithState [
	| slot |
	slot := #slot1 => ExampleSlotWithState.
	aClass := self make: [ :builder | builder slots: {slot} ].
	self assert: (aClass hasSlotNamed: #slot1).

	"test reflective write and read"
	slot write: 5 to: aClass new.
	self assert: (slot read: aClass new) equals: 5.
	"test compiled read and write"
	self compileAccessorsFor: slot.
	aClass new slot1: 10.
	self assert: aClass new slot1 equals: 10
]

{ #category : #tests }
ExampleSlotWithStateTest >> testExampleTwoSlotWithState [
	" add two, remove one"
	aClass := self make: [ :builder | builder 
		slots: {#slot1 =>ExampleSlotWithState . #slot2 =>ExampleSlotWithState}].

	self assert: (aClass hasSlotNamed: #slot1).
	self assert: (aClass hasSlotNamed: #slot2).
	
	aClass := self make: [ :builder | builder  slots: {#slot2 =>ExampleSlotWithState}].
	self deny: (aClass hasSlotNamed: #slot1).
	self assert: (aClass hasSlotNamed: #slot2)
]
