"
An active link in a text linking to a variable
"
Class {
	#name : #TextVariableLink,
	#superclass : #TextLink,
	#instVars : [
		'variableName'
	],
	#category : #'Text-Core'
}

{ #category : #'instance creation' }
TextVariableLink class >> variableName: aSymbol [
	^ self new
		variableName: aSymbol;
		yourself
]

{ #category : #comparing }
TextVariableLink >> = anotherObject [

	^super = anotherObject and: [ variableName = anotherObject variableName ]
]

{ #category : #comparing }
TextVariableLink >> hash [

	^(super hash + variableName hash) hashMultiply 
]

{ #category : #accessing }
TextVariableLink >> variableName [
	^ variableName
]

{ #category : #accessing }
TextVariableLink >> variableName: aString [
	variableName := aString
]
