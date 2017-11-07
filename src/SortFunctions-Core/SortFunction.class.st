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
	#category : #SortFunctions-Core
}

{ #category : #converting }
SortFunction >> , aSortFunction [
	"Return a new SortFunction which is the concatenation of aSortFunction to me, I will be the primary sort, but if I compare equal, I will defer to the argument."

	^ChainedSortFunction startWith: self then: aSortFunction asSortFunction
]

{ #category : #converting }
SortFunction >> asSortFunction [

	^self
]

{ #category : #evaluating }
SortFunction >> collate: anObject1 with: anObject2 [
	"Do a three-way comparison between the anObject1 and anObject2, returning
	-1 if anObject1 < anObject2
	0 if anObject1 = anObject2
	1 if anObject1 > anObject2
	This assumes a total order in accordance with the mathematical law of trichotomy.
	See also:  http://en.wikipedia.org/wiki/Three-way_comparison"
	self subclassResponsibility 
]

{ #category : #converting }
SortFunction >> reversed [
	"Return new sort function with inverted direction then my, 
	if I'm currently ascending, I will return descending instance, and vice-versa."

	^ReverseSortFunction new baseSortFunction: self
]

{ #category : #converting }
SortFunction >> undefinedFirst [
	^(UndefinedSortFunction new baseSortFunction: self) undefinedFirst
]

{ #category : #converting }
SortFunction >> undefinedLast [
	^(UndefinedSortFunction new baseSortFunction: self) undefinedLast
]

{ #category : #evaluating }
SortFunction >> value: anObject1 value: anObject2 [
	"Masquerade as a two argument block, used by many of the sorting APIs, 
	by returning whether anObject1 should be placed before anObject2 or not"

	| result |
	result := self collate: anObject1 with: anObject2.
	^result <= 0
]
