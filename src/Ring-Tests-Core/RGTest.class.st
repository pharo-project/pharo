Class {
	#name : #RGTest,
	#superclass : #TestCase,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGTest >> checkClassesConsistency: object1 and: object2 [

	"check if instance variables have the same types (after sending of #value)"

	self assert: (object1 class instVarNames collect: [ :var | 
		var -> (object1 instVarNamed: var) value class ]) 
		equals: (object2 class instVarNames collect: [ :var | 
		var -> (object2 instVarNamed: var) value class ]) 
]

{ #category : #utilities }
RGTest >> checkHierarchyConsistencyOf: anRGClass [
	"prove that all relations between class, superclass, metaclass and superclass metaclass are consistent"

	self assert: anRGClass ~~ anRGClass superclass.
	self assert: anRGClass ~~ anRGClass metaclass.
	self assert: anRGClass ~~ anRGClass superclass metaclass.

	self assert: anRGClass superclass ~~ anRGClass metaclass.
	self assert: anRGClass superclass ~~ anRGClass superclass metaclass.
	self assert: anRGClass metaclass ~~ anRGClass superclass metaclass.

	self assert: anRGClass superclass superclass identicalTo: anRGClass superclass.
	self assert: anRGClass superclass metaclass identicalTo: anRGClass metaclass superclass.

	self assert: anRGClass superclass metaclass superclass identicalTo: anRGClass superclass metaclass.
	self assert: anRGClass superclass metaclass metaclass identicalTo: anRGClass superclass metaclass
]

{ #category : #utilities }
RGTest >> checkImplicitSingleClassEnvironmentOf: anRGClass [
	
	self deny: (anRGClass isRingResolved).

	"we have SomeClass, SomeClass class, SomeClass superclass, SomeClass superclass class"
	self assert: anRGClass environment ask behaviors size equals: 4.

	"we have one package for SomeClass and SomeClass class, other for SomeClass superclass 
	and SomeClass superclass class"	
	self assert: anRGClass environment ask packages size equals: 2.

	self checkHierarchyConsistencyOf: anRGClass.
	
	
	

]

{ #category : #utilities }
RGTest >> checkImplicitSingleTraitEnvironmentOf: anRGTrait [
	self assert: anRGTrait environment ask behaviors size equals: 10.
	self assert: anRGTrait environment ask packages size equals: 5.

	self checkHierarchyConsistencyOf: anRGTrait metaclass.

	self assert: anRGTrait ~~ anRGTrait classTrait.
	self assert: anRGTrait superclass identicalTo: anRGTrait superclass.
	self assert: anRGTrait classTrait superclass identicalTo: anRGTrait classTrait superclass
]

{ #category : #tests }
RGTest >> checkIncompatible: aBlock [

	"prove that the block tries to do an action not suitable for a particular behavior strategy"

	self should: aBlock raise: RGIncompatibleBehaviorTypeError

]
