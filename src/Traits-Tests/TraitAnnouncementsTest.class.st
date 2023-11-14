"
Unit test for announcement handling when dealing with traits
"
Class {
	#name : 'TraitAnnouncementsTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitAnnouncementsTest >> testPackageIsUpdatedInClassSide [

	| c1 t1 |
	t1 := self newTrait: #T1.
	t1 class compile: 'msg ^ 1'.

	c1 := self newClass: #C1 superclass: Object traits: t1.
	c1 class compile: 'msg ^ 12'.

	self assert: self packageNameForTests asPackage methods size equals: 2.
	(c1 class >> #msg) removeFromSystem.
	self assert: self packageNameForTests asPackage methods size equals: 1
]

{ #category : 'tests' }
TraitAnnouncementsTest >> testPackageIsUpdatedInInstanceSide [

	| c1 t1 |
	t1 := self newTrait: #T1.
	t1 compile: 'msg ^ 1'.

	c1 := self newClass: #C1 superclass: Object traits: t1.
	c1 compile: 'msg ^ 12'.

	self assert: (c1 >> #msg) origin equals: c1.

	self assert: self packageNameForTests asPackage methods size equals: 2.
	(c1 >> #msg) removeFromSystem.
	self assert: self packageNameForTests asPackage methods size equals: 1
]
