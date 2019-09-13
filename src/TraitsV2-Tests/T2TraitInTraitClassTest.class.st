Class {
	#name : #T2TraitInTraitClassTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2TraitInTraitClassTest >> testAddingSlotToClassSide [
	
	| t1 |
	
	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: #(aSlot).
	
	self assert: t1 classTrait slots size equals: 1.
	self assert: t1 classTrait slots anyOne name equals: #aSlot.	
]

{ #category : #tests }
T2TraitInTraitClassTest >> testAddingSpecialSlotToClassSide [
	
	| t1 |
	
	t1 := self newTrait: #T1 with: #() uses: #().
	t1 classTrait uses: {} slots: {#aSlot => WeakSlot}.
	
	self assert: t1 classTrait slots size equals: 1.
	self assert: t1 classTrait slots anyOne name equals: #aSlot.	
	self assert: t1 classTrait slots anyOne species equals: WeakSlot.		
]
