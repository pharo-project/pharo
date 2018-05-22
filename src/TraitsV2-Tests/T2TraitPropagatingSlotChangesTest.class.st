Class {
	#name : #T2TraitPropagatingSlotChangesTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2TraitPropagatingSlotChangesTest >> testAddingSlotToTrait [
	| t1 c1 |

	t1 := self newTrait: #T1 with: #() uses: {}.
	c1 := self newClass: #C1 with: #() uses: t1.

	self assert: c1 classLayout slotScope parentScope == c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope == c1 class superclass classLayout slotScope.
	self assertCollection: c1 slots equals: #()	.
			
	t1 := self newTrait: #T1 with: #(aSlot) uses: {}.

	self assert: c1 classLayout slotScope parentScope == c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope == c1 class superclass classLayout slotScope. 		 
	self assertCollection: (c1 slots collect: #name)  hasSameElements: #(aSlot).
]

{ #category : #tests }
T2TraitPropagatingSlotChangesTest >> testRemovingSlotToTrait [
	| t1 c1 |

	t1 := self newTrait: #T1 with: #(aSlot) uses: {}.
	c1 := self newClass: #C1 with: #() uses: t1.

	self assert: c1 classLayout slotScope parentScope == c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope == c1 class superclass classLayout slotScope.
	self assertCollection: (c1 slots collect: #name)  hasSameElements: #(aSlot).
			
	t1 := self newTrait: #T1 with: #() uses: {}.

	self assert: c1 classLayout slotScope parentScope == c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope == c1 class superclass classLayout slotScope. 		 
	self assertCollection: c1 slots equals: #()	.

]
