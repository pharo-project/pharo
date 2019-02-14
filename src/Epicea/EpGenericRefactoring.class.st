"
I represent a generic refactoring, which can be reconstructed via it's #storeString.
"
Class {
	#name : #EpGenericRefactoring,
	#superclass : #EpRefactoring,
	#instVars : [
		'storeString'
	],
	#category : #'Epicea-Model'
}

{ #category : #testing }
EpGenericRefactoring class >> canAccept: aRBRefactoring [ 

	^ (aRBRefactoring class whichClassIncludesSelector: #storeOn:) ~= Object
]

{ #category : #'instance creation' }
EpGenericRefactoring class >> for: aRBRefactoring [
	^ self basicNew
		initializeWith: aRBRefactoring;
		yourself
]

{ #category : #converting }
EpGenericRefactoring >> asRBRefactoring [
	^ self class compiler evaluate: storeString
]

{ #category : #initialization }
EpGenericRefactoring >> initializeWith: aRBRefactoring [
	self initialize.
	storeString := aRBRefactoring storeString
]
