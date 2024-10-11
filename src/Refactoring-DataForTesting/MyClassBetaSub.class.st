"
I exist to test refactoring conditions
"
Class {
	#name : 'MyClassBetaSub',
	#superclass : 'MyClassBeta',
	#instVars : [
		'instVarBSub'
	],
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'dummy methods' }
MyClassBetaSub >> duplicatedInSubclassMoreComplexBody [
	^ 42 + 666
]

{ #category : 'dummy methods' }
MyClassBetaSub >> methodDuplicatedInSubclass [
	^ self
]

{ #category : 'dummy methods' }
MyClassBetaSub >> methodDuplicatedInSubclass3 [
	^ 42 + 666
]

{ #category : 'dummy methods' }
MyClassBetaSub >> methodForSupersendInLeaf [
	^ super methodForSupersendInLeaf 
]

{ #category : 'dummy methods' }
MyClassBetaSub >> methodReferencingInstVarDefinedInItsDefiningClassAndOneInItsSuperclass [
	^ {instVarB  . instVarBSub}
]

{ #category : 'dummy methods' }
MyClassBetaSub >> methodReferencingInstanceVariableDefinedInSuperclass [
	^ instVarB
]
