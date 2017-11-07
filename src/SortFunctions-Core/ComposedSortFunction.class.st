"
A ComposedSortFunction is an abstract class wrapping over another SortFunction for the sake of composition.

Subclasses have to define the composition behavior.

Instances variables:
	collator		<SortFunction>	the wrapped sort function
"
Class {
	#name : #ComposedSortFunction,
	#superclass : #SortFunction,
	#instVars : [
		'collator'
	],
	#category : #SortFunctions-Core
}

{ #category : #comparing }
ComposedSortFunction >> = aSortFunction [
	self == aSortFunction ifTrue: [ ^true ].
	^self class = aSortFunction class and: [ collator = aSortFunction collator ]
]

{ #category : #accessing }
ComposedSortFunction >> collator [
	^collator
]

{ #category : #accessing }
ComposedSortFunction >> collator: aSortFunction [
	collator := aSortFunction
]

{ #category : #comparing }
ComposedSortFunction >> hash [
	^collator hash hashMultiply
]

{ #category : #initailize-release }
ComposedSortFunction >> initialize [
	super initialize.
	collator := DefaultSortFunction new
]
