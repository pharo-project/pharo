"
I implement an abstract string based filter.
My users can update the filteringText using the message #filteringText:
When a new filtering is done I can handle the update of the filtering strategy and also I keep a list of already returned values so on a reset the filter is reapply to them, without reseting the inner iterator.
If the filter requires a full reset, I reset myself and the inner one.

My subclasses implements #criterium: to change how the strings are compared.
"
Class {
	#name : #SptAbstractStringFilter,
	#superclass : #SptIteratorDecorator,
	#instVars : [
		'returnedIndex',
		'originalUnwrapped',
		'results',
		'filteringText'
	],
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #'instance creation' }
SptAbstractStringFilter class >> on: aSptCombinator with: aString [

	^ self new
		originalUnwrapped: aSptCombinator;
		filteringText: aString
]

{ #category : #testing }
SptAbstractStringFilter >> atEnd [

	^ (returnedIndex = results size) and: [ inner atEnd ]
]

{ #category : #protected }
SptAbstractStringFilter >> criterium: aString [

	self subclassResponsibility 

]

{ #category : #protected }
SptAbstractStringFilter >> doReset [

	originalUnwrapped reset.
	returnedIndex := 0.
	results := OrderedCollection new
]

{ #category : #accessing }
SptAbstractStringFilter >> filteringText: aString [

	inner := originalUnwrapped select: [ :e | self criterium: e].

	(aString asLowercase beginsWith: filteringText asLowercase) 
		ifFalse: [ 
			self reset ].

	filteringText := aString.
	results := results select: [ :e | self criterium: e].
	returnedIndex := 0.


]

{ #category : #initialization }
SptAbstractStringFilter >> initialize [

	filteringText := ''.
	results := OrderedCollection new.
	returnedIndex := ''.
]

{ #category : #accessing }
SptAbstractStringFilter >> next [

	| nextOne |

	(returnedIndex < results size) ifTrue: [  
		returnedIndex := returnedIndex + 1.
		^ results at: returnedIndex.	
	].

	nextOne := super next.
	
	nextOne 
		ifNil: [ ^ nil ].
	
	results add: nextOne.
	returnedIndex := results size.
	
	^ nextOne
]

{ #category : #accessing }
SptAbstractStringFilter >> originalUnwrapped [
	^ originalUnwrapped
]

{ #category : #accessing }
SptAbstractStringFilter >> originalUnwrapped: anObject [
	originalUnwrapped := anObject
]
