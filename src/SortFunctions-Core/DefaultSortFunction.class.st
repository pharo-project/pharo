"
A DefaultSortFunction is a collator using the default threeWayCompare: operator.
It is known to work on String and Magnitude.

"
Class {
	#name : #DefaultSortFunction,
	#superclass : #SortFunction,
	#category : #SortFunctions-Core
}

{ #category : #'class initialization' }
DefaultSortFunction class >> initialize [
	Default := self new
]

{ #category : #evaluating }
DefaultSortFunction >> collate: anObject with: anotherObject [
	^anObject threeWayCompareTo: anotherObject
]
