"
A mock definition used for testing purposes
"
Class {
	#name : #MCMockDefinition,
	#superclass : #MCDefinition,
	#instVars : [
		'token'
	],
	#category : #'Monticello-Mocks'
}

{ #category : #accessing }
MCMockDefinition class >> token: aString [

	^ self new token: aString
]

{ #category : #compiling }
MCMockDefinition class >> wantsChangeSetLogging [
	^ false
]

{ #category : #comparing }
MCMockDefinition >> = definition [
	self == definition
		ifTrue: [ ^ true ].
	self species = definition species
		ifFalse: [ ^ false ].
	^definition token = token
]

{ #category : #converting }
MCMockDefinition >> asString [

	^ token
]

{ #category : #comparing }
MCMockDefinition >> description [

	^ token first
]

{ #category : #comparing }
MCMockDefinition >> hash [

	^ token hash
]

{ #category : #printing }
MCMockDefinition >> printString [

	^ token
]

{ #category : #printing }
MCMockDefinition >> summary [

	^ token
]

{ #category : #accessing }
MCMockDefinition >> token [

	^ token
]

{ #category : #accessing }
MCMockDefinition >> token: aString [

	token := aString
]
