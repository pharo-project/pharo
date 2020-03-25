"
I represent an abstract Iterator.
My subclasses should implement #next, #doReset, and #atEnd.

I implement some construction messages to ease the construction of chains of iterators.

#, allows to create sequence of iterators, the results are from the first one, then from the second one and so on. 

#asBeginsWithFilter
Creates an iterator that allows to filter by a string, comparing with beginsWith:, with an starting filter of ''.
#beginsWithFilter: aString
Create an iterator that allows to filter by a string, comparing with beginsWith:, with an starting filter of aString.

#asSubStringFilter
Creates an iterator that allows to filter by a string, comparing with substring:, with an starting filter of ''.

#substringFilter: aString
Create an iterator that allows to filter by a string, comparing with substring:, with an starting filter of aString.

#asWithoutDuplicates 
Creates an itearator that returns no duplicates.
"
Class {
	#name : #SptIterator,
	#superclass : #Object,
	#instVars : [
		'onReset'
	],
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #sequencing }
SptIterator >> , aSptGeneratorIterator [ 
	
	^ SptSequenceIterator with: { self. aSptGeneratorIterator }
]

{ #category : #filtering }
SptIterator >> asBeginsWithFilter [
	
	^ self beginsWithFilter: ''
]

{ #category : #filtering }
SptIterator >> asSubstringFilter [
	
	^ self substringFilter: ''
]

{ #category : #filtering }
SptIterator >> asWithoutDuplicates [
	
	^ SptWithoutDuplicatesDecorator on: self
]

{ #category : #testing }
SptIterator >> atEnd [ 

	^ self subclassResponsibility 
]

{ #category : #filtering }
SptIterator >> beginsWithFilter: aString [ 
	
	^ SptBeginsWithFilter on: self with: aString
]

{ #category : #transforming }
SptIterator >> collect: aBlockClosure [ 
	
	^ SptTransformation on: self with: aBlockClosure
]

{ #category : #protected }
SptIterator >> doReset [
	
	^ self subclassResponsibility
]

{ #category : #accessing }
SptIterator >> next [

	self subclassResponsibility 
]

{ #category : #initialization }
SptIterator >> next: aQuantity [

	^ OrderedCollection streamContents: [ :stream | | total last |
			total := 0.
			[ (last := self next).
				last ifNotNil: [  
					stream nextPut: last.
					total := total + 1 ] ]
				doWhileTrue: [ total < aQuantity and: [ last notNil ] ] ]
]

{ #category : #events }
SptIterator >> onReset: aValuable [

	onReset := aValuable
]

{ #category : #reseting }
SptIterator >> reset [

	self doReset.
	onReset ifNotNil: [ onReset value ]
]

{ #category : #filtering }
SptIterator >> select: aBlockClosure [ 
	
	^ SptFilter on: self with: aBlockClosure 
]

{ #category : #filtering }
SptIterator >> substringFilter: aString [ 
	
	^ SptSubStringFilter on: self with: aString
]

{ #category : #accessing }
SptIterator >> upToEnd [

	^ OrderedCollection
		streamContents:
			[ :stream | [ self atEnd ] whileFalse: [ stream nextPut: self next ] ]
]
