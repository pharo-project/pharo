"
Unit test for trait related slot scopes
"
Class {
	#name : 'TraitSlotScopeTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitSlotScopeTest >> testClassWithComplexTraits [
	| t1 t2 c1 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	c1 := self newClass: #C1 with: #(aSlot) traits: t1 + t2.

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope
]

{ #category : 'tests' }
TraitSlotScopeTest >> testClassWithTraits [
	| t1 c1 |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 with: #(aSlot) traits: t1.

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope
]

{ #category : 'tests' }
TraitSlotScopeTest >> testSubClassAndAddComplexTraitAfter [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	c1 := self newClass: #C1 with: #(aSlot) traits: {}.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		traits: {}.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.

	c2 := self newClass: #C2 with: #(aSlot otherSlot) traits: t1 + t2.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : 'tests' }
TraitSlotScopeTest >> testSubClassAndAddTraitAfter [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	c1 := self newClass: #C1 with: #(aSlot) traits: {}.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		traits: {}.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : 'tests' }
TraitSlotScopeTest >> testSubClassWithComplexTraits [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	c1 := self newClass: #C1 with: #(aSlot) traits: t1 + t2.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		traits: {}.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : 'tests' }
TraitSlotScopeTest >> testSubClassWithComplexTraitsAfterModification [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	c1 := self newClass: #C1 with: #(aSlot) traits: t1 + t2.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		traits: {}.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.

	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot another)
		traits: {}.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : 'tests' }
TraitSlotScopeTest >> testSubClassWithComplexTraitsAfterModificationOfParent [
	| t1 c1 c2 t2 t3 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.
	t3 := self newTrait: #T3.

	c1 := self newClass: #C1 with: #(aSlot) traits: t1 + t3.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		traits: t2.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : 'tests' }
TraitSlotScopeTest >> testSubClassWithTraits [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	c1 := self newClass: #C1 with: #(aSlot) traits: t1.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		traits: t2.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : 'tests' }
TraitSlotScopeTest >> testSubClassWithTraitsAfterModification [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	c1 := self newClass: #C1 with: #(aSlot) traits: t1.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		traits: t2.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.

	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot another)
		traits: t2.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : 'tests' }
TraitSlotScopeTest >> testSubClassWithTraitsAfterModificationOfParent [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	c1 := self newClass: #C1 with: #(aSlot) traits: t1.
	c2 := self
		newClass: #C2
		superclass: c1
		with: #(otherSlot)
		traits: t2.

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : 'tests' }
TraitSlotScopeTest >> testSubClassWithTraitsAfterModificationOfParentSharedPools [

	| t1 c1 c2 t2 x1 |
	[
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	c1 := self newClass: #C1 with: #( aSlot ) traits: t1.
	c2 := self
		      newClass: #C2
		      superclass: c1
		      with: #( otherSlot )
		      traits: t2.

	x1 := self class classInstaller make: [ :aClassBuilder |
		      aClassBuilder
			      name: #X1;
			      superclass: SharedPool;
			      package: self packageNameForTests ].


	c1 := self class classInstaller make: [ :aClassBuilder |
		      aClassBuilder
			      name: #C1;
			      traitComposition: t1;
			      sharedPools: 'X1';
			      package: self packageNameForTests ].

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope ] ensure: [
		c1 ifNotNil: [ "For now if we remove a used shared variables it causes troubles. So as a temporary workaround we remove the class first." c1 removeFromSystem ] ]
]

{ #category : 'tests' }
TraitSlotScopeTest >> testSubClassWithTraitsAfterModificationOfParentSharedvariables [

	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	c1 := self newClass: #C1 with: #( aSlot ) traits: t1.
	c2 := self
		      newClass: #C2
		      superclass: c1
		      with: #( otherSlot )
		      traits: t2.

	c1 := self class classInstaller make: [ :builder |
		      builder
			      name: #C1;
			      superclass: Object;
			      slots: #( aSlot );
			      sharedVariables: #( ClassVar );
			      traitComposition: t1;
			      package: self packageNameForTests ].

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]
