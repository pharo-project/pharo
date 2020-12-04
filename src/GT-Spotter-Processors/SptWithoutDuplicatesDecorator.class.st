"
I implement a decorator for the iterators. 
I don't allow duplicates to be returned. For doing so, I have a collection of the already returned values, so I can filter duplicates.
"
Class {
	#name : #SptWithoutDuplicatesDecorator,
	#superclass : #SptIteratorDecorator,
	#instVars : [
		'uniqueResults'
	],
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #'instance creation' }
SptWithoutDuplicatesDecorator class >> on: anInner [

	^ self new 
		inner: anInner;
		yourself
]

{ #category : #resetting }
SptWithoutDuplicatesDecorator >> clearDuplicates [
	
	uniqueResults removeAll
]

{ #category : #protected }
SptWithoutDuplicatesDecorator >> doReset [ 

	inner reset.
	self clearDuplicates
]

{ #category : #initialization }
SptWithoutDuplicatesDecorator >> initialize [ 

	super initialize.
	uniqueResults := Set new.
]

{ #category : #accessing }
SptWithoutDuplicatesDecorator >> next [

	| aValue |
	aValue := inner next.
	
	[aValue isNotNil and: [ uniqueResults includes: aValue ]]
		whileTrue: [ aValue := inner next ].
		
	uniqueResults add: aValue.
	^ aValue
	
]
