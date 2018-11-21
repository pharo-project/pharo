"
I model the read from a global variable
"
Class {
	#name : #RFGlobalRead,
	#superclass : #RFVariableRead,
	#instVars : [
		'environment'
	],
	#category : #'Reflectivity-Operations'
}

{ #category : #accessing }
RFGlobalRead >> environment [
	^ environment ifNil: [ Smalltalk globals ]
]

{ #category : #accessing }
RFGlobalRead >> environment: anObject [
	environment := anObject
]

{ #category : #evaluating }
RFGlobalRead >> value [
	^variable read
]

{ #category : #accessing }
RFGlobalRead >> variableName: aSymbol [
	variable := self environment associationAt: aSymbol
]
