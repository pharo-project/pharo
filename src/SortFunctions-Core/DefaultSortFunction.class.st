"
A DefaultSortFunction is a collator using the default threeWayCompare: operator.
It is known to work on String and Magnitude.

"
Class {
	#name : #DefaultSortFunction,
	#superclass : #SortFunction,
	#classVars : [
		'UniqueInstance'
	],
	#category : #SortFunctions-Core
}

{ #category : #'instance creation' }
DefaultSortFunction class >> new [
	^UniqueInstance ifNil: [UniqueInstance := self basicNew initialize]
]

{ #category : #evaluating }
DefaultSortFunction >> collate: anObject with: anotherObject [
	^anObject threeWayCompareTo: anotherObject
]
