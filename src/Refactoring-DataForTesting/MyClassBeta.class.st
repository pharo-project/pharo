"
I exist to test refactoring conditions
"
Class {
	#name : 'MyClassBeta',
	#superclass : 'MyClassAlpha',
	#instVars : [
		'instVarB',
		'fooAccessor'
	],
	#classVars : [
		'SharedVarB'
	],
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'dummy methods' }
MyClassBeta >> duplicatedInSubclassMoreComplexBody [
	^ 42 + 666
]

{ #category : 'accessing' }
MyClassBeta >> fooAccessor [

	^ fooAccessor
]

{ #category : 'accessing' }
MyClassBeta >> fooAccessor: anObject [

	fooAccessor := anObject
]

{ #category : 'dummy methods' }
MyClassBeta >> methodDuplicatedInSibling [
	^ 38 * 101
]

{ #category : 'dummy methods' }
MyClassBeta >> methodDuplicatedInSubclass [
	^ self
]

{ #category : 'dummy methods' }
MyClassBeta >> methodDuplicatedInSubclass2 [
	^ 42 + 666
]

{ #category : 'dummy methods' }
MyClassBeta >> methodForPullUp [
	^ 24
]

{ #category : 'dummy methods' }
MyClassBeta >> methodForSupersend [
	^ self
]

{ #category : 'dummy methods' }
MyClassBeta >> methodOverriden [
	^ self
]

{ #category : 'dummy methods' }
MyClassBeta >> methodReferencingInstanceVariable [
	^ instVarB 
]

{ #category : 'dummy methods' }
MyClassBeta >> methodReferencingSharedAndInstVariable [

	instVarB.
	^ SharedVarB
]

{ #category : 'dummy methods' }
MyClassBeta >> methodReferencingSharedVarDefinedInItsDefiningClassAndOneInItsSuperclass [
	^ { SharedVarA . SharedVarB } 
]

{ #category : 'dummy methods' }
MyClassBeta >> methodReferencingSharedVariable [
	^ SharedVarB
]

{ #category : 'dummy methods' }
MyClassBeta >> methodReferencingSharedVariableFromSuperclass [
	^ SharedVarA 
]

{ #category : 'dummy methods' }
MyClassBeta >> methodWithSuperSend [
	"I have the same implementation that my superclass. One of my sibling has a different definition."
	^ super methodWithSuperSend
]

{ #category : 'dummy methods' }
MyClassBeta >> methodWithSuperSendAndReferInstVar [
	"send message to super"
	super methodOverriden.
	
	"reference an inst var defined in this class"
	instVarB
]
