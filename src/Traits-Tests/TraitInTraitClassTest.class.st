Class {
	#name : 'TraitInTraitClassTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitInTraitClassTest >> testAddingSlotToClassSide [

	| t1 |
	t1 := self newTrait: #T1.
	t1 classTrait trait: {  } slots: #( aSlot ).

	self assert: t1 classTrait slots size equals: 1.
	self assert: t1 classTrait slots anyOne name equals: #aSlot
]

{ #category : 'tests' }
TraitInTraitClassTest >> testAddingSpecialSlotToClassSide [

	| t1 |
	t1 := self newTrait: #T1.
	t1 classTrait trait: {  } slots: { (#aSlot => WeakSlot) }.

	self assert: t1 classTrait slots size equals: 1.
	self assert: t1 classTrait slots anyOne name equals: #aSlot.
	self assert: t1 classTrait slots anyOne species equals: WeakSlot
]
