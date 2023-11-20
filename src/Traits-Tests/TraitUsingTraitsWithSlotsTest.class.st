Class {
	#name : 'TraitUsingTraitsWithSlotsTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitUsingTraitsWithSlotsTest >> testRedefinitionKeepsSlots [
	| t1 c1 |
	t1 := self newTrait: #T1 with: #(a b c).
	t1 classTrait slots: #(aSlot).
	c1 := self newClass: #C1 traits: {t1}.

	self assertEmpty: c1 localSlots.
	self assertEmpty: c1 class localSlots.

	c1 addSlot: #d.

	self assertCollection: (c1 localSlots collect: [:each | each name]) hasSameElements: {#d}.
	self assertEmpty: c1 class localSlots
]
