Class {
	#name : 'TraitOverloadingOfMethodsInTraitedClassTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitOverloadingOfMethodsInTraitedClassTest >> createT1 [
	| t1 |
	t1 := self newTrait: #T1 with: '' asSlotCollection.
	t1 classTrait
		compile:
			'isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest
		"Should be removed at the end of the test"
		^true
	'.
	TraitedClass
		compile:
			'isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest
		"Should be removed at the end of the test"
		^false
	'.
	Behavior
		compile:
			'isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest
		"Should be removed at the end of the test"
		^false
	'.
	^ t1
]

{ #category : 'tests' }
TraitOverloadingOfMethodsInTraitedClassTest >> createT2 [
	^ self newTrait: #T2 with: '' asSlotCollection
]

{ #category : 'running' }
TraitOverloadingOfMethodsInTraitedClassTest >> tearDown [
	TraitedClass
		removeSelector: #isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest.
	Behavior
		removeSelector: #isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest.
	super tearDown
]

{ #category : 'tests' }
TraitOverloadingOfMethodsInTraitedClassTest >> testDirectTraitSubclassing [

	<ignoreNotImplementedSelectors: #( isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest )>
	| t1 c1 |
	t1 := self createT1.
	c1 := self newClass: #C1 traits: t1.
	self assert: c1 isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest
]

{ #category : 'tests' }
TraitOverloadingOfMethodsInTraitedClassTest >> testIndirectTraitSubclassing [

	<ignoreNotImplementedSelectors:
	#( isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest )>
	| t1 t2 superclass subclass |
	t1 := self createT1.
	t2 := self createT2.
	superclass := self newClass: #C2Superclass traits: t2.

	subclass := self newClass: #C1Subclass superclass: superclass traits: t1.
	self deny: superclass isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest.
	self assert: subclass isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest
]
