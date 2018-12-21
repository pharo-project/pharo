"
I model the write to a global variable
"
Class {
	#name : #RFGlobalWrite,
	#superclass : #RFVariableWrite,
	#instVars : [
		'environment'
	],
	#category : #'Reflectivity-Operations'
}

{ #category : #accessing }
RFGlobalWrite >> environment [
	^ environment ifNil: [ Smalltalk globals ]
]

{ #category : #accessing }
RFGlobalWrite >> environment: anObject [
	environment := anObject
]

{ #category : #evaluating }
RFGlobalWrite >> value [
	^variable write: assignedValue
]

{ #category : #accessing }
RFGlobalWrite >> variableName: aSymbol [
	variable := self environment associationAt: aSymbol
]
