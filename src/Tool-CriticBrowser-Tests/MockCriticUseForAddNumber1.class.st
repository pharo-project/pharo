Class {
	#name : #MockCriticUseForAddNumber1,
	#superclass : #Object,
	#instVars : [
		'title'
	],
	#category : #'Tool-CriticBrowser-Tests-MockCritic'
}

{ #category : #accessing }
MockCriticUseForAddNumber1 class >> title: aString [
	^ self new title: aString
]

{ #category : #accessing }
MockCriticUseForAddNumber1 class >> uniqueIdentifierName [
	^ self className
]

{ #category : #comparing }
MockCriticUseForAddNumber1 >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ title = anObject title
]

{ #category : #accessing }
MockCriticUseForAddNumber1 >> criticTheNonMetaclassClass [
	^ false
]

{ #category : #accessing }
MockCriticUseForAddNumber1 >> entity [
	^ self class
]

{ #category : #comparing }
MockCriticUseForAddNumber1 >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ title hash
]

{ #category : #accessing }
MockCriticUseForAddNumber1 >> sourceAnchor [
	^ self
]

{ #category : #accessing }
MockCriticUseForAddNumber1 >> title [
	^ title
]

{ #category : #accessing }
MockCriticUseForAddNumber1 >> title: anObject [
	title := anObject
]
