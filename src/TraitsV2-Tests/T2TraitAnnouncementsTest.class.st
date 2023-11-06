"
Unit test for announcement handling when dealing with traits
"
Class {
	#name : 'T2TraitAnnouncementsTest',
	#superclass : 'T2AbstractTest',
	#category : 'TraitsV2-Tests',
	#package : 'TraitsV2-Tests'
}

{ #category : 'tests' }
T2TraitAnnouncementsTest >> packageName [
	^ 'TestPackage'
]

{ #category : 'tests' }
T2TraitAnnouncementsTest >> testRPackageIsUpdatedInClassSide [

	| c1 t1 |

	t1 := self newTrait: #T1 with: #() traits: TEmpty.
	t1 class compile: 'msg ^ 1'.

	c1 := self newClass: #C1 superclass: Object with: #() traits: t1.
	c1 class compile: 'msg ^ 12'.

	self assert: self packageNameForTests asPackage methods size equals: 2.
	(c1 class >> #msg) removeFromSystem.
	self assert: self packageNameForTests asPackage methods size equals: 1
]

{ #category : 'tests' }
T2TraitAnnouncementsTest >> testRPackageIsUpdatedInInstanceSide [

	| c1 t1 |
	t1 := self newTrait: #T1 with: #(  ) traits: #(  ).
	t1 compile: 'msg ^ 1'.

	c1 := self
		      newClass: #C1
		      superclass: Object
		      with: #(  )
		      traits: t1.
	c1 compile: 'msg ^ 12'.

	self assert: (c1 >> #msg) origin equals: c1.

	self assert: self packageNameForTests asPackage methods size equals: 2.
	(c1 >> #msg) removeFromSystem.
	self assert: self packageNameForTests asPackage methods size equals: 1
]
