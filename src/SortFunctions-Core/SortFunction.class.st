"
I am intended to be used in place of two arg sort blocks.

Usage

In the following example, an ascending SortFunction is created based on the result of the #first message send to each object.
#(#(1 2) #(2 3) #(0 0)) sorted: #first ascending.

To sort by the #last element, but descending, the following would be used:
#(#(1 2) #(2 3) #(0 0)) sorted: #last descending.

One can use blocks as well. The following sorts in descending order, the sub elements based on the sum of their values.
| sumBlock |
sumBlock := [:sequence | sequence inject: 0 into: [:sum :each | sum + each]].
#(#(1 2) #(2 3) #(0 0)) sorted: sumBlock descending.

One can even use 2 arg blocks, for those cases where the function isn't expressible with objects that respond to < and =. The only catch, is that such a function has to return not true and false, but instead a collation order, values of -1 (for before), 0 (the same) or 1 (to follow). For example:

| oddBlock |
oddBlock :=
		[:a :b |
		a odd = b odd ifTrue: [0] ifFalse: [a odd ifTrue: [-1] ifFalse: [1]]].
#(1 5 1 3 2 7 9 4 6) asSortedCollection: oddBlock descending

Using #undefinedFirst and #undefinedLast it is possible to deal with nil values, moving them first or last. For Example:

#(a nil z b) sorted: #value ascending undefinedFirst.
#(a nil z b) sorted: #value ascending undefinedLast.

Different scenarios are implemented by my different subclasses:
- SortByPropertyFunction implement property comparison by selector or block
- CollatorBlockFunction represent two args collator block
- and ChainedSortFunction implements composition of collators created by comma message

Applications can implement their own domain sort functinos defining the method:
	threeWayCompare: anObject1 with: anObject2

Instance Variables
	direction	<SmallInteger>	1 for ascending, -1 for descending
"
Class {
	#name : #SortFunction,
	#superclass : #Object,
	#instVars : [
		'direction'
	],
	#category : #SortFunctions-Core
}

{ #category : #'instance creation' }
SortFunction class >> ascending [

	^self new beAscending
]

{ #category : #'instance creation' }
SortFunction class >> descending [

	^self new beDescending
]

{ #category : #converting }
SortFunction >> , aSortFunction [
	"Return a new SortFunction which is the concatenation of aSortFunction to me, I will be the primary sort, but if I compare equal, I will defer to the argument."

	^ChainedSortFunction startWith: self then: aSortFunction asSortFunction
]

{ #category : #comparing }
SortFunction >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ direction = anObject direction
]

{ #category : #converting }
SortFunction >> asSortFunction [

	^self
]

{ #category : #initailize-release }
SortFunction >> beAscending [

	direction := 1
]

{ #category : #initailize-release }
SortFunction >> beDescending [

	direction := -1
]

{ #category : #evaluating }
SortFunction >> collate: anObject1 with: anObject2 [
	"Do a three-way comparison between the anObject1 and anObject2 according to the given direction,
	which can be:
	1 - specifies native comparison logic.
	-1 - specifies reversed comparison logic. 
	Subclases implement native comparison logic in the method #threeWayCompare:with:. 
	Look at it for details"

	^(self threeWayCompare: anObject1 with: anObject2) * direction
]

{ #category : #accessing }
SortFunction >> direction [
	^ direction
]

{ #category : #initailize-release }
SortFunction >> direction: oneOrMinusOne [

	direction := oneOrMinusOne
]

{ #category : #comparing }
SortFunction >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ direction hash
]

{ #category : #initailize-release }
SortFunction >> initialize [
	super initialize.
	
	self beAscending
]

{ #category : #converting }
SortFunction >> inverted [
	"Return new sort function with inverted direction then my, 
	if I'm currently ascending, I will return descending instance, and vice-versa."

	^self copy 
		direction: direction negated
]

{ #category : #testing }
SortFunction >> isAscending [

	^direction = 1
]

{ #category : #testing }
SortFunction >> isDescending [

	^direction = -1
]

{ #category : #evaluating }
SortFunction >> threeWayCompare: anObject1 with: anObject2 [
	"Do a three-way comparison between the anObject1 and anObject2, returning
	-1 if anObject1 < anObject2
	0 if anObject1 = anObject2
	1 if anObject1 > anObject2
	This assumes a total order in accordance with the mathematical law of trichotomy.
	See also:  http://en.wikipedia.org/wiki/Three-way_comparison"
	self subclassResponsibility 
]

{ #category : #evaluating }
SortFunction >> value: anObject1 value: anObject2 [
	"Masquerade as a two argument block, used by many of the sorting APIs, 
	by returning whether anObject1 should be placed before anObject2 or not"

	| result |
	result := self collate: anObject1 with: anObject2.
	^result <= 0
]
