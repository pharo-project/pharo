"
I represent a refactoring which it's not possible to reconstruct because the original information is lost.
"
Class {
	#name : #EpUnknownRefactoring,
	#superclass : #EpRefactoring,
	#instVars : [
		'printString'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpUnknownRefactoring class >> for: aRBRefactoring [
	^ self basicNew
		initializeWith: aRBRefactoring;
		yourself
]

{ #category : #converting }
EpUnknownRefactoring >> asRBRefactoring [
	"The original refactoring cannot be recreated."
	
	^ self shouldNotImplement
]

{ #category : #testing }
EpUnknownRefactoring >> canBuildRBRefactoring [
	^ false
]

{ #category : #initialization }
EpUnknownRefactoring >> initializeWith: aRBRefactoring [
	self initialize.
	printString := aRBRefactoring printString
]
