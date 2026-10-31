"
This class is the place for tests related to the Slot examples. 
(some examples have their own test class, the rest is here)
"
Class {
	#name : 'AccessorInstanceVariableSlotTest',
	#superclass : 'SlotSilentTest',
	#category : 'Slot-Examples-Tests-Base',
	#package : 'Slot-Examples-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
AccessorInstanceVariableSlotTest >> testAccessorInstanceVariableSlot [
	| slot object |
	<ignoreNotImplementedSelectors: #(slot1 slot1:)>
	aClass := self
		make:
			[ :builder | builder slots: {(#slot1 => AccessorInstanceVariableSlot)} ].
	self assert: (aClass hasSlotNamed: #slot1).

	"test reflective write and read"
	slot := aClass slotNamed: #slot1.
	object := aClass new.
	slot write: 5 to: object.
	self assert: (slot read: object) equals: 5.
	object slot1: 10.
	self assert: object slot1 equals: 10.

	"did we create accessors?"
	self assert: (aClass includesSelector: #slot1).
	self assert: (aClass includesSelector: #slot1:)
]

{ #category : 'tests' }
AccessorInstanceVariableSlotTest >> testAccessorInstanceVariableSlotGeneratesAccessors [
	| class1 |

	class1 := self make: [ :builder |
		builder
			name: self aClassName;
			superclass: Object ].

	class1 class addSlot: (AccessorInstanceVariableSlot named: #foo).

	self assert: (class1 class canUnderstand: #foo).
	self assert: (class1 class canUnderstand: #foo:)
]
