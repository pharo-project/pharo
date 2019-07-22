Class {
	#name : #CBMockPackage,
	#superclass : #Object,
	#instVars : [
		'title'
	],
	#category : #'Tool-CriticBrowser-Tests-mockPackage'
}

{ #category : #accessing }
CBMockPackage class >> title: aString [
	^ self new title: aString
]

{ #category : #comparing }
CBMockPackage >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ title = anObject title
]

{ #category : #comparing }
CBMockPackage >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ title hash
]

{ #category : #accessing }
CBMockPackage >> title [
	^ title
]

{ #category : #accessing }
CBMockPackage >> title: anObject [
	title := anObject
]
