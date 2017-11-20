"
I add to my parent the idea of a ""next"" function to use when two objects are equal by my primary sort function.

Usage

SortFunctions can be chained together in primary, secondary, tertiary, etc order using the comma method. Consider a sequence of customer objects, where each customer object responds to the messages firstName, lastName, and age. If we want to sort them lastName first, then firstName, and finally oldest first, we would use an expression like:

customers sort: #lastName ascending, #firstName ascending, #age descending

As noted in my super's comment, unary symbols or single arg blocks can be used. One can omit the the ascending methods on arguments (not the receiver), it will default blocks or symbols to be ascending if none is specified. In other words, the above expression could be simplified slightly as

customers sort: #lastName ascending, #firstName, #age descending

(note the missing ascending on the #firstName argument)

Instance Variables
	baseSortFunction	<SortFunction> the primary SortFunction to collate given objects
	next	Function	<SortFunction>	the next SortFunction to evaluate in the event primary collation results are equal values
"
Class {
	#name : #ChainedSortFunction,
	#superclass : #ComposedSortFunction,
	#instVars : [
		'nextFunction'
	],
	#category : #SortFunctions-Core
}

{ #category : #'instance creation' }
ChainedSortFunction class >> startWith: aSortFunction then: nextSortFunction [
	^self new
		baseSortFunction: aSortFunction;
		nextFunction: nextSortFunction
]

{ #category : #converting }
ChainedSortFunction >> , aSortFunction [

	^self class startWith: baseSortFunction then: nextFunction , aSortFunction
]

{ #category : #comparing }
ChainedSortFunction >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ baseSortFunction = anObject baseSortFunction
			and: [ nextFunction = anObject nextFunction ]
]

{ #category : #evaluating }
ChainedSortFunction >> collate: anObject1 with: anObject2 [
	"If the result of current function is 0, then pass on to the next function to work it out"

	| result |
	result := baseSortFunction collate: anObject1 with: anObject2.
	^result isZero
		ifTrue: [nextFunction collate: anObject1 with: anObject2.]
		ifFalse: [result]
]

{ #category : #comparing }
ChainedSortFunction >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ super hash + nextFunction hash
]

{ #category : #accessing }
ChainedSortFunction >> nextFunction [
	^ nextFunction
]

{ #category : #accessing }
ChainedSortFunction >> nextFunction: anObject [
	nextFunction := anObject
]
