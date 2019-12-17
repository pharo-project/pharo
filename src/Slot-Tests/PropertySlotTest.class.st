"
Test for PropertySlot
"
Class {
	#name : #PropertySlotTest,
	#superclass : #SlotSilentTest,
	#category : #'Slot-Tests-Examples'
}

{ #category : #tests }
PropertySlotTest >> testCreateClassWithPropertySlot [
	"Add instance variables using the builder interface"
	
	| propertySlot |
	
	propertySlot := #prop => PropertySlot.
	
	aClass := self make: [ :builder |
		builder 
			slots: {propertySlot}
		].

	self assert: (aClass hasSlot: propertySlot).
	self assert: (aClass classLayout hasSlot: (propertySlot instVarNamed: 'baseSlot'))
	

]

{ #category : #tests }
PropertySlotTest >> testCreateClassWithPropertySlotAddSecond [
	"Add instance variables using the builder interface"

	| propertySlot1 propertySlot2 |
	propertySlot1 := #prop1 => PropertySlot.
	propertySlot2 := #prop2 => PropertySlot.

	aClass := self make: [ :builder | builder slots: {propertySlot1} ].

	self assert: (aClass hasSlot: propertySlot1).
	self assert: (aClass classLayout hasSlot: (propertySlot1 instVarNamed: 'baseSlot')).

	aClass := self make: [ :builder | builder slots: {propertySlot1 . propertySlot2} ].

	self assert: (aClass hasSlot: propertySlot1).
	self assert: (aClass classLayout hasSlot: (propertySlot2 instVarNamed: 'baseSlot')).

	self assert: (propertySlot1 instVarNamed: 'baseSlot') identicalTo: (propertySlot2 instVarNamed: 'baseSlot').

	self assert: (aClass hasSlot: propertySlot2).
	self assert: (aClass classLayout hasSlot: (propertySlot2 instVarNamed: 'baseSlot'))
]

{ #category : #tests }
PropertySlotTest >> testCreateClassWithTwoPropertySlots [
	"Add instance variables using the builder interface"
	
	| propertySlot1 propertySlot2 |
	
	propertySlot1 := #prop1 => PropertySlot.
	propertySlot2 := #prop2 => PropertySlot.
	
	aClass := self make: [ :builder |
		builder 
			slots: {propertySlot1 . propertySlot2 }
		].

	self assert: (aClass hasSlot: propertySlot1).
	self assert: (aClass classLayout hasSlot: (propertySlot1 instVarNamed: 'baseSlot')).
	
	self assert: (aClass hasSlot: propertySlot2).
	self assert: (aClass classLayout  hasSlot: (propertySlot2 instVarNamed: 'baseSlot'))
	
	
	
]

{ #category : #tests }
PropertySlotTest >> testRemovePropertySlot [
	
	| propertySlot |
	
	propertySlot := #prop => PropertySlot.
	
	aClass := self make: [ :builder | builder slots: {propertySlot} ].

	self assert: (aClass hasSlot: propertySlot).
	self assert: (aClass classLayout hasSlot: (propertySlot instVarNamed: 'baseSlot')).
	
	aClass := self make: [ :builder | builder  slots: {} ].
	self deny: (aClass hasSlot: propertySlot).	
	self deny: (aClass classLayout hasSlot: (propertySlot instVarNamed: 'baseSlot')).
	self deny: (aClass classLayout hasSlotNamed: #'_propertyBaseSlot')
]

{ #category : #tests }
PropertySlotTest >> testRemovePropertySlot2 [
	"add two property slots, remove one"
	
	| propertySlot1 propertySlot2 |
	
	propertySlot1 := #prop1 => PropertySlot.
	propertySlot2 := #prop2 => PropertySlot.
	
	aClass := self make: [ :builder |
		builder 
			slots: {propertySlot1 . propertySlot2 }
		].

	self assert: (aClass hasSlot: propertySlot1).
	self assert: (aClass classLayout hasSlot: (propertySlot1 instVarNamed: 'baseSlot')).
	
	self assert: (aClass hasSlot: propertySlot2).
	self assert: (aClass classLayout  hasSlot: (propertySlot2 instVarNamed: 'baseSlot')).
	
	aClass := self make: [ :builder |
		builder 
			slots: {propertySlot1}
		].
	
	self assert:	(aClass hasSlot: propertySlot1).
	self deny: 	(aClass hasSlot: propertySlot2).
	self assert:	(aClass classLayout hasSlotNamed:  #'_propertyBaseSlot'). 
		
	aClass := self make: [ :builder |
		builder 
			slots: {}
		].
	
	self deny:	(aClass hasSlot: propertySlot1).
	self deny: 	(aClass hasSlot: propertySlot2).
	self deny:	(aClass classLayout hasSlotNamed:  #'_propertyBaseSlot')
]

{ #category : #tests }
PropertySlotTest >> testRemovePropertySlotWithInstance [
	"add two property slots, remove one"
	
	| propertySlot1 propertySlot2 instance |
	
	propertySlot1 := #prop1 => PropertySlot.
	propertySlot2 := #prop2 => PropertySlot.
	
	aClass := self make: [ :builder |
		builder 
			slots: {propertySlot1 . propertySlot2 }
		].

	instance := aClass new.

	propertySlot1 write: 41 to: instance.
	propertySlot2 write: 42 to: instance.

	self assert: (propertySlot1 read: instance) equals: 41.
	self assert: (propertySlot2 read: instance) equals: 42.

	aClass removeSlot: propertySlot1.
	
	self assert:	(aClass hasSlot: propertySlot2).
	self deny: 	(aClass hasSlot: propertySlot1)
	
	
]

{ #category : #tests }
PropertySlotTest >> testRemovePropertySlotWithTwoInstances [
	"add two property slots, remove one"
	
	| propertySlot1 propertySlot2 instance instance2 |
	
	propertySlot1 := #prop1 => PropertySlot.
	propertySlot2 := #prop2 => PropertySlot.
	
	aClass := self make: [ :builder |
		builder 
			slots: {propertySlot1 . propertySlot2 }
		].

	instance := aClass new.
	instance2 := aClass new.

	propertySlot1 write: 41 to: instance.
	propertySlot2 write: 42 to: instance.

	self assert: (propertySlot1 read: instance) equals: 41.
	self assert: (propertySlot2 read: instance) equals: 42.
	self assert: (propertySlot1 read: instance2) equals: nil.
	
	propertySlot1 write: 5 to: instance2.
	self assert: (propertySlot1 read: instance2) equals: 5
	
	
]

{ #category : #tests }
PropertySlotTest >> testWhichSelectorsAccessFindSlots [
	| cls |
	cls := Object newAnonymousSubclass.
	cls := cls addSlot: #regularIvar.
	cls := cls addSlot: #slotIvar => PropertySlot.
	cls
		compile:
			'initialize 
		super initialize.
	
		regularIvar := 4.
		slotIvar := 2.
	'.
	self
		assert: (cls whichSelectorsAccess: #regularIvar) size equals: 1;
		assert: (cls whichSelectorsAccess: #slotIvar) size equals: 1;
		assert: (cls whichSelectorsAccess: #notAnIvar) equals: #()
]
