Class {
	#name : #T2UsingTraitsWithSlotsTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2UsingTraitsWithSlotsTest >> testRedefinitionKeepsSlots [
	| t1 c1 |
	t1 := self newTrait: #T1 with: #(a b c).
	t1 classTrait slots: #(aSlot).
	c1 := self newClass: #C1 with: {} uses: {t1}.

	self assertEmpty: c1 localSlots.
	self assertEmpty: c1 class localSlots.

	c1 addSlot: #d.

	self assertCollection: (c1 localSlots collect: #name) hasSameElements: {#d}.
	self assertEmpty: c1 class localSlots
]
