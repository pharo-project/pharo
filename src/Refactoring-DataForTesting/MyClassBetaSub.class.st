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
MyClassBetaSub >> methodDuplicatedInSubclass [
	^ self
]

{ #category : 'dummy methods' }
MyClassBetaSub >> methodReferencingInstVarDefinedInItsDefiningClassAndOneInItsSuperclass [
	^ {instVarB  . instVarBSub}
]

{ #category : 'dummy methods' }
MyClassBetaSub >> methodReferencingInstanceVariableDefinedInSuperclass [
	^ instVarB
]
