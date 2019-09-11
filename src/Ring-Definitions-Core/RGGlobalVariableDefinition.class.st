"
A global variable definition
"
Class {
	#name : #RGGlobalVariableDefinition,
	#superclass : #RGGlobalDefinition,
	#instVars : [
		'value'
	],
	#category : #'Ring-Definitions-Core-Base'
}

{ #category : #'managing container' }
RGGlobalVariableDefinition >> addInContainer: aRGContainer [

	aRGContainer addGlobalVariable: self
]

{ #category : #testing }
RGGlobalVariableDefinition >> isGlobalVariable [

	^true
]

{ #category : #'managing container' }
RGGlobalVariableDefinition >> isIncludedInContainer: aRGContainer [

	^aRGContainer includesGlobalVariable: self
]

{ #category : #testing }
RGGlobalVariableDefinition >> isVariable [

	^true
]

{ #category : #'managing container' }
RGGlobalVariableDefinition >> removeFromContainer: aRGContainer [

	aRGContainer removeGlobalVariable: self
]

{ #category : #accessing }
RGGlobalVariableDefinition >> value [
	"Retrieves the current value of the global variable"

	^ value
]

{ #category : #accessing }
RGGlobalVariableDefinition >> value: anObject [

	value := anObject
]
