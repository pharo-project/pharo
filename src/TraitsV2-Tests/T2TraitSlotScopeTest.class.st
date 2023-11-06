"
Unit test for trait related slot scopes
"
Class {
	#name : 'T2TraitSlotScopeTest',
	#superclass : 'T2AbstractTest',
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'tests' }
T2TraitSlotScopeTest >> testClassWithComplexTraits [
	| t1 t2 c1 |
	t1 := self newTrait: #T1 with: #() traits: {}.
	t2 := self newTrait: #T2 with: #() traits: {}.

	c1 := self newClass: #C1 with: #(aSlot) traits: t1 + t2.

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope
]

{ #category : 'tests' }
T2TraitSlotScopeTest >> testClassWithTraits [
	| t1 c1 |
	t1 := self newTrait: #T1 with: #() traits: {}.
	c1 := self newClass: #C1 with: #(aSlot) traits: t1.

	self assert: c1 classLayout slotScope parentScope identicalTo: c1 superclass classLayout slotScope.
	self assert: c1 class classLayout slotScope parentScope identicalTo: c1 class superclass classLayout slotScope
]

{ #category : 'tests' }
T2TraitSlotScopeTest >> testSubClassAndAddComplexTraitAfter [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() traits: {}.
	t2 := self newTrait: #T2 with: #() traits: {}.

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
T2TraitSlotScopeTest >> testSubClassAndAddTraitAfter [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() traits: {}.
	t2 := self newTrait: #T2 with: #() traits: {}.

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
T2TraitSlotScopeTest >> testSubClassWithComplexTraits [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() traits: {}.
	t2 := self newTrait: #T2 with: #() traits: {}.

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
T2TraitSlotScopeTest >> testSubClassWithComplexTraitsAfterModification [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() traits: {}.
	t2 := self newTrait: #T2 with: #() traits: {}.

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
T2TraitSlotScopeTest >> testSubClassWithComplexTraitsAfterModificationOfParent [
	| t1 c1 c2 t2 t3 |
	t1 := self newTrait: #T1 with: #() traits: {}.
	t2 := self newTrait: #T2 with: #() traits: {}.
	t3 := self newTrait: #T3 with: #() traits: {}.

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
T2TraitSlotScopeTest >> testSubClassWithTraits [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() traits: {}.
	t2 := self newTrait: #T2 with: #() traits: {}.

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
T2TraitSlotScopeTest >> testSubClassWithTraitsAfterModification [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() traits: {}.
	t2 := self newTrait: #T2 with: #() traits: {}.

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
T2TraitSlotScopeTest >> testSubClassWithTraitsAfterModificationOfParent [
	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #() traits: {}.
	t2 := self newTrait: #T2 with: #() traits: {}.

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
T2TraitSlotScopeTest >> testSubClassWithTraitsAfterModificationOfParentSharedPools [

	| t1 c1 c2 t2 x1 |
	t1 := self newTrait: #T1 with: #(  ) traits: {  }.
	t2 := self newTrait: #T2 with: #(  ) traits: {  }.

	c1 := self newClass: #C1 with: #( aSlot ) traits: t1.
	c2 := self
		      newClass: #C2
		      superclass: c1
		      with: #( otherSlot )
		      traits: t2.

	x1 := self class classInstaller make: [ :aClassBuilder |
		      aClassBuilder
			      name: #X1;
			      superclass: SharedPool ].


	c1 := self class classInstaller make: [ :aClassBuilder |
		      aClassBuilder
			      name: #C1;
			      traitComposition: t1;
			      sharedPools: 'X1';
			      package: self packageNameForTests ].

	self assert: c2 classLayout slotScope parentScope identicalTo: c2 superclass classLayout slotScope.
	self assert: c2 class classLayout slotScope parentScope identicalTo: c2 class superclass classLayout slotScope
]

{ #category : 'tests' }
T2TraitSlotScopeTest >> testSubClassWithTraitsAfterModificationOfParentSharedvariables [

	| t1 c1 c2 t2 |
	t1 := self newTrait: #T1 with: #(  ) traits: {  }.
	t2 := self newTrait: #T2 with: #(  ) traits: {  }.

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
