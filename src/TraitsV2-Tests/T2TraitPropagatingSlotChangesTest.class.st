"
Unit test for correct propogation of slot changes for traits
"
Class {
	#name : 'T2TraitPropagatingSlotChangesTest',
	#superclass : 'T2AbstractTest',
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'tests' }
T2TraitPropagatingSlotChangesTest >> testAddingSlotToTrait [

	| t1 c1 |
	t1 := self makeTrait: [ :builder | builder name: #T1 ].
	c1 := self newClass: #C1 with: #(  ) uses: t1.

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope.
	self assertCollection: c1 slots equals: #(  ).

	t1 := self newTrait: #T1 with: #( aSlot ).

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope.
	self assertCollection: (c1 allSlots collect: [ :each | each name ]) hasSameElements: #( aSlot )
]

{ #category : 'tests' }
T2TraitPropagatingSlotChangesTest >> testRemovingSlotToTrait [

	| t1 c1 |
	t1 := self newTrait: #T1 with: #( aSlot ) uses: {  }.
	c1 := self newClass: #C1 with: #(  ) uses: t1.

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope.
	self assertCollection: (c1 allSlots collect: [ :each | each name ]) hasSameElements: #( aSlot ).

	t1 := self makeTrait: [ :builder | builder name: #T1 ].

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope.
	self assertCollection: c1 allSlots hasSameElements: #(  )
]
