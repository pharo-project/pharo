"
A DefaultSortFunction is a collator using the default threeWayCompareTo: operator.
It is known to work on String and Magnitude.

It is generally not usefull to create a new instance, and the recommended pattern is to use the single instance available by sending the message  SortFunction default .

For other objects  that don't understand threeWayCompareTo: it is necessary to use a custom SortFunction rather than the default one.
"
Class {
	#name : #DefaultSortFunction,
	#superclass : #SortFunction,
	#category : #'SortFunctions-Core'
}

{ #category : #'class initialization' }
DefaultSortFunction class >> initialize [
	Default := self new
]

{ #category : #evaluating }
DefaultSortFunction >> collate: anObject with: anotherObject [
	^anObject threeWayCompareTo: anotherObject
]
