"
Unit test for trait related slot scopes
"
Class {
	#name : #T2TraitSlotScopeTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2TraitSlotScopeTest >> testClassWithComplexTraits [
	| t1 t2 c1 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	c1 := self newClass: #C1 with: #(aSlot) uses: t1 + t2.

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope
]

{ #category : #tests }
T2TraitSlotScopeTest >> testClassWithTraits [
	| t1 c1 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	c1 := self newClass: #C1 with: #(aSlot) uses: t1.

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope
]

{ #category : #tests }
T2TraitSlotScopeTest >> testSubClassAndAddComplexTraitAfter [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	c1 := self newClass: #C1 with: #(aSlot) uses: {}.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		uses: {}.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.

	c2 := self newClass: #C1 with: #(aSlot otherSlot) uses: t1 + t2.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : #tests }
T2TraitSlotScopeTest >> testSubClassAndAddTraitAfter [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	c1 := self newClass: #C1 with: #(aSlot) uses: {}.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		uses: {}.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.

	c2 := self newClass: #C1 with: #(aSlot otherSlot) uses: t1.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : #tests }
T2TraitSlotScopeTest >> testSubClassWithComplexTraits [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	c1 := self newClass: #C1 with: #(aSlot) uses: t1 + t2.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		uses: {}.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : #tests }
T2TraitSlotScopeTest >> testSubClassWithComplexTraitsAfterModification [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	c1 := self newClass: #C1 with: #(aSlot) uses: t1 + t2.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		uses: {}.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.

	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot another)
		uses: {}.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : #tests }
T2TraitSlotScopeTest >> testSubClassWithComplexTraitsAfterModificationOfParent [
	| t1 c1 c2 t2 t3 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.
	t3 := self newTrait: #T3 with: #() uses: {}.

	c1 := self newClass: #C1 with: #(aSlot) uses: t1 + t3.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		uses: t2.

	c2 := self newClass: #C1 with: #(aSlot otherSlot) uses: t2.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : #tests }
T2TraitSlotScopeTest >> testSubClassWithTraits [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	c1 := self newClass: #C1 with: #(aSlot) uses: t1.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		uses: t2.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : #tests }
T2TraitSlotScopeTest >> testSubClassWithTraitsAfterModification [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	c1 := self newClass: #C1 with: #(aSlot) uses: t1.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		uses: t2.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.

	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot another)
		uses: t2.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : #tests }
T2TraitSlotScopeTest >> testSubClassWithTraitsAfterModificationOfParent [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	c1 := self newClass: #C1 with: #(aSlot) uses: t1.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		uses: t2.

	c2 := self newClass: #C1 with: #(aSlot otherSlot) uses: t2.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : #tests }
T2TraitSlotScopeTest >> testSubClassWithTraitsAfterModificationOfParentSharedPools [
	| t1 c1 c2 t2 x1 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	c1 := self newClass: #C1 with: #(aSlot) uses: t1.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		uses: t2.

	x1 := SharedPool subclass: #X1.

	c1 := Object
		subclass: #C1
		uses: t1
		slots: #(aSlot)
		classVariables: #()
		poolDictionaries: 'X1'
		category: 'TraitsV2-Tests-TestClasses'.


	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : #tests }
T2TraitSlotScopeTest >> testSubClassWithTraitsAfterModificationOfParentSharedvariables [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	c1 := self newClass: #C1 with: #(aSlot) uses: t1.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		uses: t2.

	c1 := Object
		subclass: #C1
		uses: t1
		slots: #(aSlot)
		classVariables: #(ClassVar)
		poolDictionaries: ''
		category: 'TraitsV2-Tests-TestClasses'.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]
