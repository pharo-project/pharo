"
I implement collation of objects using their property. Property is defined by selector or one arg block which can be converted directly to my instances:
	#name ascending.
	#name descending.
	[:a | a name] ascending
	[:a | a name] descending

Look at SortFunction comments for more details.

Internal Representation and Key Implementation Points.

    Instance Variables
	property		<Symbol, BlockClosure>

"
Class {
	#name : #PropertySortFunction,
	#superclass : #ComposedSortFunction,
	#instVars : [
		'property'
	],
	#category : #'SortFunctions-Core'
}

{ #category : #'instance creation' }
PropertySortFunction class >> property: selectorOrOneArgBlock [
	^self new 
		property: selectorOrOneArgBlock
]

{ #category : #'instance creation' }
PropertySortFunction class >> property: selectorOrOneArgBlock collatedWith: aSortFunction [
	^self new 
		property: selectorOrOneArgBlock;
		baseSortFunction: aSortFunction
]

{ #category : #comparing }
PropertySortFunction >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ baseSortFunction = anObject baseSortFunction
		and: [ property = anObject property ]
]

{ #category : #evaluating }
PropertySortFunction >> collate: anObject with: another [
	^ baseSortFunction collate: (property value: anObject) with: (property value: another)
]

{ #category : #comparing }
PropertySortFunction >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ super hash bitXor: property hash
]

{ #category : #accessing }
PropertySortFunction >> property [
	^ property
]

{ #category : #accessing }
PropertySortFunction >> property: aValuable [
	property := aValuable
]

{ #category : #converting }
PropertySortFunction >> undefinedFirst [
	"apply on the property"
	^self class
		property: property
		collatedWith: baseSortFunction undefinedFirst
]

{ #category : #converting }
PropertySortFunction >> undefinedLast [
	"apply on the property"
	^self class
		property: property
		collatedWith: baseSortFunction undefinedLast
]
