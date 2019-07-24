Class {
	#name : #MockCriticUseForAddNumber2,
	#superclass : #Object,
	#instVars : [
		'title'
	],
	#category : #'Tool-CriticBrowser-Tests-Mocks'
}

{ #category : #accessing }
MockCriticUseForAddNumber2 class >> title: aTitle [
	^ self new title: aTitle
]

{ #category : #accessing }
MockCriticUseForAddNumber2 class >> uniqueIdentifierName [
	^ self className
]

{ #category : #comparing }
MockCriticUseForAddNumber2 >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ title = anObject title
]

{ #category : #accessing }
MockCriticUseForAddNumber2 >> entity [
	^ self class
]

{ #category : #comparing }
MockCriticUseForAddNumber2 >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ title hash
]

{ #category : #accessing }
MockCriticUseForAddNumber2 >> sourceAnchor [
	^ self
]

{ #category : #accessing }
MockCriticUseForAddNumber2 >> title [
	^ title
]

{ #category : #accessing }
MockCriticUseForAddNumber2 >> title: anObject [
	title := anObject
]
