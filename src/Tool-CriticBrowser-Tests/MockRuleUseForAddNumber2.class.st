Class {
	#name : #MockRuleUseForAddNumber2,
	#superclass : #Object,
	#instVars : [
		'title'
	],
	#category : #'Tool-CriticBrowser-Tests-MockRule'
}

{ #category : #accessing }
MockRuleUseForAddNumber2 class >> title: aString [
	^ self new title: aString
]

{ #category : #'as yet unclassified' }
MockRuleUseForAddNumber2 class >> uniqueIdentifierName [
	'mock2'.
]

{ #category : #comparing }
MockRuleUseForAddNumber2 >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ title = anObject title
]

{ #category : #comparing }
MockRuleUseForAddNumber2 >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ title hash
]

{ #category : #accessing }
MockRuleUseForAddNumber2 >> title [
	^ title
]

{ #category : #accessing }
MockRuleUseForAddNumber2 >> title: anObject [
	title := anObject
]
