"
I exist to test refactoring conditions
"
Class {
	#name : 'MyClassBeta',
	#superclass : 'MyClassAlpha',
	#instVars : [
		'instVarB'
	],
	#classVars : [
		'SharedVarB'
	],
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'dummy methods' }
MyClassBeta >> methodForPushUp [
	^ 24
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
	" I send a message to super "
	^ super methodWithSuperSend
]
