Class {
	#name : #T2OverloadingOfMethodsInTraitedClassTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2OverloadingOfMethodsInTraitedClassTest >> createT1 [
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

{ #category : #tests }
T2OverloadingOfMethodsInTraitedClassTest >> createT2 [
	^ self newTrait: #T2 with: '' asSlotCollection
]

{ #category : #running }
T2OverloadingOfMethodsInTraitedClassTest >> tearDown [
	TraitedClass
		removeSelector: #isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest.
	Behavior
		removeSelector: #isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest.
	super tearDown
]

{ #category : #tests }
T2OverloadingOfMethodsInTraitedClassTest >> testDirectTraitSubclassing [
	| t1 c1 |
	t1 := self createT1.
	c1 := self newClass: #C1 with: '' asSlotCollection uses: t1.
	self
		assert:
			c1 isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest
]

{ #category : #tests }
T2OverloadingOfMethodsInTraitedClassTest >> testIndirectTraitSubclassing [
	| t1 t2 superclass subclass |
	t1 := self createT1.
	t2 := self createT2.
	superclass := self
		newClass: #C2Superclass
		with: '' asSlotCollection
		uses: t2.
		
	subclass := self
		newClass: #C1Subclass
		superclass: superclass
		with: '' asSlotCollection
		uses: t1.
	self
		deny:
			superclass
				isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest.
	self
		assert:
			subclass
				isTrueForUsersOfTheTraitForT2OverloadingOfMethodsInTraitedClassTest
]
