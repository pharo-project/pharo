Class {
	#name : 'SlotMigrateExamplesTest',
	#superclass : 'SlotMigrationTest',
	#category : 'Slot-Examples-Tests-ClassBuilder',
	#package : 'Slot-Examples-Tests',
	#tag : 'ClassBuilder'
}

{ #category : 'tests' }
SlotMigrateExamplesTest >> testAddTestSlot [
	"We create a class without slots and create an instance; then we add a slot and check the instance can hold a value."
	aClass := self makeWithLayout: FixedLayout andSlots: { #aSlot }.

	instance := aClass new.
	instance instVarAt: 1 put: 125.
	"now we extend it with a test slots.  The class definition should've changed"
	self makeWithLayout: FixedLayout andSlots: {#anotherSlot => ExampleSlotWithState. #aSlot. }.

	"It should not have move the first instance as TestSlot size = 0"
	self assert: (instance instVarAt: 1) equals: 125.
	aClass classLayout checkIntegrity
]

{ #category : 'tests' }
SlotMigrateExamplesTest >> testMigrateSlotClass [
	"We create a class with a Test slot, so its  value is shared between instances"
	| instance2 |
	aClass := self makeWithLayout: FixedLayout andSlots: { #aSlot => ExampleSlotWithState. }.

	instance := aClass new.
	instance2 := aClass new.
	(aClass slotNamed: #aSlot) write: 125 to: instance2.
	"now we change the TestSlot with a InstanceVariableSlot. Both instances should both shared the same value in this slot."
	self makeWithLayout: FixedLayout andSlots: { #aSlot }.

	self assert: ((aClass slotNamed: #aSlot) read: instance) equals: 125.
	self assert: ((aClass slotNamed: #aSlot) read: instance2) equals: 125.
	aClass classLayout checkIntegrity
]

{ #category : 'tests' }
SlotMigrateExamplesTest >> testRemoveTestSlot [
	aClass := self makeWithLayout: FixedLayout andSlots: {#anotherSlot => ExampleSlotWithState. #aSlot }.

	instance := aClass new.
	instance instVarAt: 1 put: 125.
	"now we remove the TestSlot from the class definition. As testSlot has a size of 0, the object should not change."
	self makeWithLayout: FixedLayout andSlots: {#aSlot. }.


	self assert: (instance instVarAt: 1) equals: 125.
	aClass classLayout checkIntegrity
]

{ #category : 'tests' }
SlotMigrateExamplesTest >> testUndefinedSlot [
	| slot |
	
	aClass := self make: [ :builder |
		builder slots: { UndefinedSlot named: #a ast: ((OCParser parseExpression: '#a => MySlot') doSemanticAnalysis;yourself) } ].

	slot := aClass slotNamed: #a.

	self assert: slot class equals: UndefinedSlot.
	self assert: slot name equals: #a.
	self assert: slot slotClassName equals: #MySlot.

	instance := aClass new.

	self assert: (slot read: instance) equals: nil.
	slot write: 1 to: instance.
	self assert: (slot read: instance) equals: nil
]
