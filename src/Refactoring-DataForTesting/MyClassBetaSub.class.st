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

	"Duplicate here on purpose for testing purposes"
	^ 42 + 666
]

{ #category : 'dummy methods' }
MyClassBetaSub >> methodDuplicatedInSubclass [

	"Duplicate here on purpose for testing purposes"
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

{ #category : 'dummy methods' }
MyClassBetaSub >> overridenInSubclassAndNoWhereElse [
	"This is used to test pull up method when the target class is two levels above and I have override in subclass."
	^ 42
]
