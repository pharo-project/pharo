"
A mock definition used for testing purposes
"
Class {
	#name : 'MCMockDefinition',
	#superclass : 'MCDefinition',
	#instVars : [
		'token'
	],
	#category : 'Monticello-Tests-Mocks',
	#package : 'Monticello-Tests',
	#tag : 'Mocks'
}

{ #category : 'accessing' }
MCMockDefinition class >> token: aString [

	^ self new token: aString
]

{ #category : 'comparing' }
MCMockDefinition >> = definition [
	self == definition
		ifTrue: [ ^ true ].
	self species = definition species
		ifFalse: [ ^ false ].
	^definition token = token
]

{ #category : 'converting' }
MCMockDefinition >> asString [

	^ token
]

{ #category : 'comparing' }
MCMockDefinition >> description [

	^ token first
]

{ #category : 'comparing' }
MCMockDefinition >> hash [

	^ token hash
]

{ #category : 'printing' }
MCMockDefinition >> printString [

	^ token
]

{ #category : 'printing' }
MCMockDefinition >> summary [

	^ token
]

{ #category : 'accessing' }
MCMockDefinition >> token [

	^ token
]

{ #category : 'accessing' }
MCMockDefinition >> token: aString [

	token := aString
]
