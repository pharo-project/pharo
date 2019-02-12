"
A ComposedSortFunction is an abstract class wrapping over another SortFunction for the sake of composition.

Subclasses have to define the composition behavior via collate:with: message.

Instances variables:
	baseSortFunction		<SortFunction>	the wrapped sort function
"
Class {
	#name : #ComposedSortFunction,
	#superclass : #SortFunction,
	#instVars : [
		'baseSortFunction'
	],
	#category : #'SortFunctions-Core'
}

{ #category : #'instance creation' }
ComposedSortFunction class >> on: aSortFunction [
	^self new baseSortFunction: aSortFunction
]

{ #category : #comparing }
ComposedSortFunction >> = aSortFunction [
	self == aSortFunction ifTrue: [ ^true ].
	^self class = aSortFunction class and: [ baseSortFunction = aSortFunction baseSortFunction ]
]

{ #category : #accessing }
ComposedSortFunction >> baseSortFunction [
	^baseSortFunction
]

{ #category : #accessing }
ComposedSortFunction >> baseSortFunction: aSortFunction [
	baseSortFunction := aSortFunction
]

{ #category : #comparing }
ComposedSortFunction >> hash [
	^baseSortFunction hash hashMultiply
]

{ #category : #initialization }
ComposedSortFunction >> initialize [
	super initialize.
	baseSortFunction := self class default
]
