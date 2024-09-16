"
Unit test for trait handling when classes are removed
"
Class {
	#name : 'TraitObsoleteClassTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitObsoleteClassTest >> testObsoleteClassIsRemovedFromUsers [

	| t1 t2 c1 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	c1 := self newClass: #C1 traits: t1 + t2.

	self assert: (t1 users includes: c1).
	self assert: (t2 users includes: c1).

	c1 removeFromSystem.

	self deny: (t1 users includes: c1).
	self deny: (t2 users includes: c1)
]

{ #category : 'tests' }
TraitObsoleteClassTest >> testObsoleteClassIsRemovedFromUsersClassSide [

	| t1 t2 c1 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	c1 := self newClass: #C1.
	c1 class setTraitComposition: t1 + t2.

	self assert: (t1 users includes: c1 class).
	self assert: (t2 users includes: c1 class).

	c1 removeFromSystem.

	self deny: (t1 users includes: c1 class).
	self deny: (t2 users includes: c1 class)
]

{ #category : 'tests' }
TraitObsoleteClassTest >> testObsoleteTraitIsRemovedFromUsers [

	| t1 t2 t3 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.

	t3 := self newTrait: #T3 traits: t1 + t2.

	self assert: (t1 users includes: t3).
	self assert: (t2 users includes: t3).

	t3 removeFromSystem.

	self deny: (t1 users includes: t3).
	self deny: (t2 users includes: t3)
]
