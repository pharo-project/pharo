Class {
	#name : #SlotExampleTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
SlotExampleTest >> testAccessorInstanceVariableSlot [
	| slot object |
	
	aClass := self make: [ :builder |
		builder 
			slots: {#slot1 =>AccessorInstanceVariableSlot  }
		].

	self assert: (aClass hasSlotNamed: #slot1).
	
	"test refletive write and read"
	slot := aClass slotNamed: #slot1.
	object := aClass new.
	slot write: 5 to: object.
	self assert: (slot read: object) = 5.
	
	"test generated accessors"
	object slot1: 10.
	self assert: object slot1 = 10.
	
	
	
	
	
]

{ #category : #tests }
SlotExampleTest >> testExampleClassSide [
	" can we install a slot on the class side?"
	aClass := self make: [ :builder |
		builder 
			classSlots: {#slot1 =>ExampleSlotWithState}
		].

	self assert: (aClass class hasSlotNamed: #slot1).
	self assert: aClass class slotDefinitionString =  '{ #slot1 => ExampleSlotWithState }'.
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

	self assert: (slot read: object) = 5.
	slot write: 10 to: object.
	self assert: (slot read: object) = 10.
	
	
	
	
	
]

{ #category : #tests }
SlotExampleTest >> testExampleSlotWithState [
	| slot reader writer |
	
	aClass := self make: [ :builder |
		builder 
			slots: {#slot1 =>ExampleSlotWithState  }
		].

	self assert: (aClass hasSlotNamed: #slot1).
	
	"test refletive write and read"
	slot := aClass slotNamed: #slot1.
	slot write: 5 to: aClass new.
	self assert: (slot read: aClass new) = 5.
	
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
	
	aClass new slot1: 10.
	self assert: aClass new slot1 = 10.
	
	
	
	
	
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

{ #category : #tests }
SlotExampleTest >> testPharoClassInstallerExample [
	| newClass |
	[ 
		newClass := Smalltalk classInstaller example.
		self assert: newClass isBehavior.
	] ensure: [ 
		newClass removeFromSystem ].
]
