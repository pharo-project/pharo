"
I represent a set of objects without duplicates.  I can hold anything that responds to
#hash and #=, except for nil.  My instances will automatically grow, if necessary,
Note that I rely on #=, not #==.  If you want a set using #==, use IdentitySet.

Instance structure:

  array	An array whose non-nil elements are the elements of the set,
		and whose nil elements are empty slots.  There is always at least one nil.
		In fact I try to keep my ""load"" at 75% or less so that hashing will work well.

  tally	The number of elements in the set.  The array size is always greater than this.

The core operation is #findElementOrNil:, which either finds the position where an
object is stored in array, if it is present, or finds a suitable position holding nil, if
its argument is not present in array,
"
Class {
	#name : #Set,
	#superclass : #HashedCollection,
	#category : #'Collections-Unordered'
}

{ #category : #'instance creation' }
Set class >> newFrom: aCollection [
	"Answer an instance of me containing the same elements as aCollection."
	| newCollection |
	newCollection := self new: aCollection size.
	newCollection addAll: aCollection.
	^ newCollection
"
	Set newFrom: {1. 2. 3}
	{1. 2. 3} as: Set
"
]

{ #category : #'instance creation' }
Set class >> sizeFor: nElements [
	"Large enough size to hold nElements with some slop (see fullCheck)"

	^ HashTableSizes atLeast: nElements * 4 // 3
]

{ #category : #comparing }
Set >> = aSet [
	self == aSet ifTrue: [^ true].	"stop recursion"
	(aSet isKindOf: Set) ifFalse: [^ false].
	self size = aSet size ifFalse: [^ false].
	self do: [:each | (aSet includes: each) ifFalse: [^ false]].
	^ true
]

{ #category : #adding }
Set >> add: newObject [
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	index := self scanFor: newObject.
	(array at: index) ifNil: [self atNewIndex: index put: newObject asSetElement].
	^ newObject
]

{ #category : #adding }
Set >> add: newObject withOccurrences: anInteger [
	^ self add: newObject
]

{ #category : #converting }
Set >> asSet [
	^self
]

{ #category : #enumerating }
Set >> collect: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newSet |
	newSet := self species new: self size.
	array do: [:each | each ifNotNil: [newSet add: (aBlock value: each enclosedSetElement)]].
	^ newSet
]

{ #category : #removing }
Set >> copyWithout: oldElement [ 
	"Answer a copy of the receiver that does not contain any
	elements equal to oldElement."

	^ self copy
		remove: oldElement ifAbsent: [];
		yourself
]

{ #category : #enumerating }
Set >> difference: aCollection [
	"Answer the set theoretic difference of two collections. Optimized version for Sets where no intermediate Set is necessary"

	"#(a b c d e f) difference:  #(a b z k)
	=> #(#f #d #e #c)
	
	#(a b z k) difference: #(a b c d e f) 
	=> #(#k #z) 
	"

	| set |
	set := self copy.
	aCollection do: [ :each | set remove: each ifAbsent: [  ] ].
	^ set
]

{ #category : #enumerating }
Set >> do: aBlock [ 
	tally = 0 ifTrue: [^ self].
	1 to: array size do:
		[:index |
		| each |
		(each := array at: index) ifNotNil: [aBlock value: each enclosedSetElement]]
]

{ #category : #private }
Set >> fixCollisionsFrom: start [
	"The element at start has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one"

	| element index |
	index := start.
	[ (element := array at: (index := index \\ array size + 1)) == nil ] whileFalse: [
		| newIndex |
		(newIndex := self scanFor: element enclosedSetElement) = index ifFalse: [
			array swap: index with: newIndex ] ]
]

{ #category : #private }
Set >> grow [
	"Grow the elements array and reinsert the old elements"

	| oldElements |
	oldElements := array.
	array := Array new: (HashTableSizes atLeast: oldElements size * 2).
	tally := 0.
	oldElements
		do:
			[ :each | 
			each == nil
				ifFalse: [ self noCheckAdd: each enclosedSetElement] ]
]

{ #category : #testing }
Set >> includes: anObject [ 
	^ (array at: (self findElementOrNil: anObject)) ~~ nil
]

{ #category : #enumerating }
Set >> intersection: aCollection [
	"Answer the set theoretic intersection of two collections.
	Optimized version for Sets where no intermediate Set is necessary"
	"(#(1 2 3 4) asSet intersection: #(3 4 5) asSet) >>> #(3 4) asSet"
	"(#(1 2 3 4) asSet intersection: #() asSet) >>> Set new"
	"( #() asSet intersection: #(1 2 3 4) asSet) >>> Set new"
	
	| outputSet |
	outputSet := self class new.
	aCollection do: [ :each|
		((self includes: each) and: [(outputSet includes: each) not])
				ifTrue: [ outputSet add: each]].
	^ outputSet
]

{ #category : #testing }
Set >> isHealthy [
	"Test that object hashes match their positions stored in set's array,
	answer true if everything ok, false otherwise
	
	Set allSubInstances select: [:badSet |
		badSet isHealthy not ]
	"
	array withIndexDo: [ :element :index |
		element ifNotNil: [
			(self scanFor: element enclosedSetElement) == index
				ifFalse: [ ^ false ]]].
	^ true
]

{ #category : #accessing }
Set >> like: anObject [
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties.
	Note, use #like:ifAbsent: if you need to match against nil as element"

	^ self like: anObject ifAbsent: [ nil ]
]

{ #category : #accessing }
Set >> like: anObject ifAbsent: aBlock [
	"Answer an object in the receiver that is equal to anObject,
	or evaluate the block if not found. Relies heavily on hash properties"
	| element |
	element := array at: (self scanFor: anObject).
	^ element ifNil: [ aBlock value ] ifNotNil: [ element enclosedSetElement ]
]

{ #category : #private }
Set >> noCheckAdd: anObject [
	"This method should be deprecated"
	array at: (self findElementOrNil: anObject) put: anObject asSetElement.
	tally := tally + 1
]

{ #category : #private }
Set >> noCheckNoGrowFillFrom: anArray [
	"Add the elements of anArray except nils to me assuming that I don't contain any of them, they are unique and I have more free space than they require."

	1 to: anArray size do: [ :index |
		(anArray at: index) ifNotNil: [ :object |
			array
				at: (self scanForEmptySlotFor: object enclosedSetElement)
				put: object ] ]
]

{ #category : #testing }
Set >> occurrencesOf: anObject [ 
	^ (self includes: anObject) ifTrue: [1] ifFalse: [0]
]

{ #category : #copying }
Set >> postCopy [
	super postCopy.
	array := array copy
]

{ #category : #private }
Set >> rehash [
	| newSelf |
	newSelf := self species new: self size.
	self do: [:each | newSelf noCheckAdd: each].
	array := newSelf array
]

{ #category : #removing }
Set >> remove: oldObject ifAbsent: aBlock [

	| index |
	index := self findElementOrNil: oldObject.
	(array at: index) == nil ifTrue: [ ^ aBlock value ].
	array at: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ oldObject
]

{ #category : #private }
Set >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or raise an error if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start |
	index := start := anObject hash \\ array size + 1.
	[ 
		| element |
		((element := array at: index) == nil or: [ element enclosedSetElement = anObject ])
			ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace
]

{ #category : #private }
Set >> withArray: anArray [
	"private -- for use only in copy"
	"I want to get a conflict"
	array := anArray
]
