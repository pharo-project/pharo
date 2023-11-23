"
Unit test for correct propogation of slot changes for traits
"
Class {
	#name : 'TraitPropagatingSlotChangesTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitPropagatingSlotChangesTest >> testAddingSlotToTrait [
	| t1 c1 |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope.
	self assertCollection: c1 slots equals: #().

	t1 := self newTrait: #T1 with: #(aSlot).

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope.
	self assertCollection: (c1 allSlots collect: [:each | each name]) hasSameElements: #(aSlot)
]

{ #category : 'tests' }
TraitPropagatingSlotChangesTest >> testRemovingSlotToTrait [
	| t1 c1 |
	t1 := self newTrait: #T1 with: #(aSlot).
	c1 := self newClass: #C1 traits: t1.

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope.
	self assertCollection: (c1 allSlots collect: [:each | each name]) hasSameElements: #(aSlot).

	t1 := self newTrait: #T1.

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope.
	self assertCollection: c1 allSlots hasSameElements: #()
]
