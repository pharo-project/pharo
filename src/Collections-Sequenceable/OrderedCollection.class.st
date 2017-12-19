"
I am one of the most common collection. I can grow, and elements can be added sequentially by the user.  

Description 
-------------------
I am more general than Array; my size grows on demand. I store data inside an Array and remember the first and last index. If I need, I can replace this Array by a larger one.

I am usually used to store an unknown amount of objects. When my contents size will not move, one can send me the #asArray message to get better performances, but I cannot grow anymore (add: and remove: are not supported on Array).

Public API and Key Messages
-------------------	

- #new / #withAll: aCollection / #with: anObject 	are common constructors

- #add: anObject / #at: anIndex put: anObject / #at: anIndex ifAbsentPut: anObject 	allow to add new elements to myself.

- #remove: anObject / #removeIndex: anIndex 	allow to remove an element.
	
- #do: aBlock / #collect: aBlock / #select: aBlock / #reject: aBlock 	are common iterators.

Examples
-------------------	
	
	""There is many ways to create an OrderedCollection, here are some:""
	ordCol := OrderedCollection new.
	ordCol
		add: 'one';
		add: 'two';
		addFirst: 'zero';
		addLast: 'three'.
	ordCol.		""returns: an OrderedCollection('zero' 'one' 'two' 'three')""

	""or""
	ordCol := OrderedCollection with: 'one' with: 'two' with: 'three'.
	ordCol.		""returns: an OrderedCollection('one' 'two' 'three')""

	""or from an other collection""
	ordCol := OrderedCollection withAll: #('one' 'two' 'three').
	ordCol.		""returns: an OrderedCollection('one' 'two' 'three')""

	""or""
	#('one' 'two' 'three') asOrderedCollection.

	""Some manipulations""
	ordCol := OrderedCollection ofSize: 2.
	ordCol
		at: 1 put: 'one';
		at: 2 put: 'two';
		at: 2 ifAbsentPut: 'three'.
	ordCol.		""returns: an OrderedCollection('one' 'two')""
	ordCol
		remove: 'two';
		removeIndex: 1.
	ordCol.		""returns:  an OrderedCollection()""

	""A last one""
	ordCol := OrderedCollection with: $b with: $c with: $a.
	ordCol sort: [ :first :second | first < second ].		""returns: an OrderedCollection($a $b $c)""
	ordCol collect: [ :element | element asUppercase ].		""returns:  an OrderedCollection($A $B $C)""
	ordCol select: [ :element | element >= $b ].		""returns:  an OrderedCollection($b $c)""
	ordCol do: [ :element | element inspect ].
	ordCol asArray		""returns: #($a $b $c)""
			 
Internal Representation and Key Implementation Points.
-------------------	

    Instance Variables
	array:			<Array> 		An Array where I store my elements. If I need a bigger one I can remove this one and create a new one.
	firstIndex:		<Integer> 	The index of my first element.
	lastIndex:		<Integer> 	The index of my last element.

I store my elements inside an array. This array is AT LEAST of the size of my elements. If someone adds an element and my array is not large enough, I remove it and I create a new one larger with the same elements (usually, the size double).
"
Class {
	#name : #OrderedCollection,
	#superclass : #SequenceableCollection,
	#instVars : [
		'array',
		'firstIndex',
		'lastIndex'
	],
	#category : #'Collections-Sequenceable'
}

{ #category : #private }
OrderedCollection class >> arrayType [
	^ Array
]

{ #category : #'instance creation' }
OrderedCollection class >> new [
	^ self new: 10
]

{ #category : #'instance creation' }
OrderedCollection class >> new: anInteger [ 
	^ self basicNew setCollection: (self arrayType new: anInteger)
]

{ #category : #'stream creation' }
OrderedCollection class >> new: size streamContents: aBlock [
	^ self withAll: (super new: size streamContents: aBlock)
]

{ #category : #'instance creation' }
OrderedCollection class >> new: anInteger withAll: anObject [
	^ self basicNew setContents: (self arrayType new: anInteger withAll: anObject)
]

{ #category : #'instance creation' }
OrderedCollection class >> newFrom: aCollection [ 
	"Answer an instance of me containing the same elements as aCollection."

	| newCollection |
	newCollection := self new: aCollection size.
	newCollection addAll: aCollection.
	^newCollection

"	OrderedCollection newFrom: {1. 2. 3}
	{1. 2. 3} as: OrderedCollection
	{4. 2. 7} as: SortedCollection
"
]

{ #category : #'instance creation' }
OrderedCollection class >> ofSize: n [
	"Create a new collection of size n with nil as its elements.
	This method exists because OrderedCollection new: n creates an
	empty collection,  not one of size n."
	| collection |
	collection := self new: n.
	collection setContents: (collection collector).
	^ collection

]

{ #category : #accessing }
OrderedCollection class >> streamSpecies [
	^ Array
]

{ #category : #adding }
OrderedCollection >> add: newObject [

	^self addLast: newObject
]

{ #category : #adding }
OrderedCollection >> add: newObject after: oldObject [ 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just succeeding oldObject. Answer newObject."
	
	| index |
	index := self find: oldObject.
	self insert: newObject before: index + 1.
	^newObject
]

{ #category : #adding }
OrderedCollection >> add: newObject afterIndex: index [ 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just after index. Answer newObject."
	(index between: 0 and: self size) ifFalse:[^self errorSubscriptBounds: index].
	self insert: newObject before: firstIndex + index.
	^ newObject
]

{ #category : #adding }
OrderedCollection >> add: newObject before: oldObject [ 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just preceding oldObject. Answer newObject."
	
	| index |
	index := self find: oldObject.
	self insert: newObject before: index.
	^newObject
]

{ #category : #adding }
OrderedCollection >> add: newObject beforeIndex: index [ 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just before index. Answer newObject."
	(index between: 1 and: self size+1) ifFalse:[^self errorSubscriptBounds: index].
	self insert: newObject before: firstIndex + index - 1.
	^ newObject
]

{ #category : #adding }
OrderedCollection >> addAll: aCollection [ 
	"Add each element of aCollection at my end. Answer	aCollection."

	^ self addAllLast: aCollection
]

{ #category : #adding }
OrderedCollection >> addAllFirst: anOrderedCollection [ 
	"Add each element of anOrderedCollection at the beginning of the 
	receiver. Answer anOrderedCollection."

	anOrderedCollection reverseDo: [:each | self addFirst: each].
	^anOrderedCollection
]

{ #category : #adding }
OrderedCollection >> addAllFirstUnlessAlreadyPresent: anOrderedCollection [ 
	"Add each element of anOrderedCollection at the beginning of the receiver, preserving the order, but do not add any items that are already in the receiver.  Answer anOrderedCollection."

	anOrderedCollection reverseDo:
		[:each | (self includes: each) ifFalse: [self addFirst: each]].
	^ anOrderedCollection
]

{ #category : #adding }
OrderedCollection >> addAllLast: aCollection [ 
	"Add each element of aCollection at the end of the receiver. 
	Answer aCollection."

	aCollection do: [:each | self addLast: each].
	^aCollection
]

{ #category : #adding }
OrderedCollection >> addFirst: newObject [ 
	"Add newObject to the beginning of the receiver. Answer newObject."

	firstIndex = 1 ifTrue: [self makeRoomAtFirst].
	firstIndex := firstIndex - 1.
	array at: firstIndex put: newObject.
	^ newObject
]

{ #category : #adding }
OrderedCollection >> addLast: newObject [ 
	"Add newObject to the end of the receiver. Answer newObject."

	lastIndex = array size ifTrue: [self makeRoomAtLast].
	lastIndex := lastIndex + 1.
	array at: lastIndex put: newObject.
	^ newObject
]

{ #category : #converting }
OrderedCollection >> asArray [
	^ (Array new: self size) replaceFrom: 1 to: self size with: array startingAt: firstIndex.
]

{ #category : #converting }
OrderedCollection >> asOrderedCollection [ 

	self species == OrderedCollection ifTrue: [ ^self ].
	^super asOrderedCollection 
]

{ #category : #accessing }
OrderedCollection >> at: anInteger [
	"Answer my element at index anInteger. at: is used by a knowledgeable
	client to access an existing element"

	| index |
	anInteger < 1
		ifTrue: [ self errorSubscriptBounds: anInteger ].
	(index := anInteger + firstIndex - 1) > lastIndex
		ifTrue: [ self errorSubscriptBounds: anInteger ].
	^ array at: index
]

{ #category : #adding }
OrderedCollection >> at: index ifAbsentPut: block [
	"Return value at index, however, if value does not exist (nil or out of bounds) then add block's value at index (growing self if necessary)"

	| v |
	index <= self size ifTrue: [
		^ (v := self at: index)
			ifNotNil: [v]
			ifNil: [self at: index put: block value]
	].
	[self size < index] whileTrue: [self add: nil].
	^ self at: index put: block value
]

{ #category : #accessing }
OrderedCollection >> at: anInteger put: anObject [
	"Put anObject at element index anInteger. at:put: cannot be used to
	append, front or back, to an ordered collection; it is used by a
	knowledgeable client to replace an element."

	| index |
	anInteger < 1
		ifTrue: [ self errorSubscriptBounds: anInteger ].
	(index := anInteger + firstIndex - 1) > lastIndex
		ifTrue: [ self errorSubscriptBounds: anInteger ].
	^ array at: index put: anObject
]

{ #category : #accessing }
OrderedCollection >> capacity [
	"Answer the current capacity of the receiver."

	^ array size
]

{ #category : #enumerating }
OrderedCollection >> collect: aBlock [ 
	"Evaluate aBlock with each of my elements as the argument. Collect the 
	resulting values into a collection that is like me. Answer the new 
	collection. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection := self species new: self size.
	firstIndex to: lastIndex do:
		[:index |
		newCollection addLast: (aBlock value: (array at: index))].
	^ newCollection
]

{ #category : #enumerating }
OrderedCollection >> collect: aBlock from: fromIndex to: toIndex [
	"Override superclass in order to use addLast:, not at:put:."
	| result |
	self ensureBoundsFrom: fromIndex to: toIndex.
	result := self species new: toIndex - fromIndex + 1.
	firstIndex + fromIndex - 1 to: firstIndex + toIndex - 1 do:
		[:index | result addLast: (aBlock value: (array at: index))].
	^ result

]

{ #category : #enumerating }
OrderedCollection >> collect: collectBlock thenSelect: selectBlock [
    "Optimized version Collection>>#collect:thenSelect:"

    | newCollection newElement |

    newCollection := self copyEmpty.
    firstIndex to: lastIndex do: [ :index | 
		newElement := collectBlock value: (array at: index).
		(selectBlock value: newElement) 
			ifTrue: [ newCollection addLast: newElement ]].
    ^ newCollection
]

{ #category : #private }
OrderedCollection >> collector [  "Private"
	^ array
]

{ #category : #copying }
OrderedCollection >> copyEmpty [
	"Answer a copy of the receiver that contains no elements."

	^self species new
]

{ #category : #copying }
OrderedCollection >> copyFrom: startIndex to: endIndex [ 
	"Answer a copy of the receiver that contains elements from position
	startIndex to endIndex."

	^self shallowCopy postCopyFrom: startIndex to: endIndex
]

{ #category : #copying }
OrderedCollection >> copyReplaceFrom: start to: stop with: replacementCollection [ 
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| newOrderedCollection delta startIndex stopIndex |
	"if start is less than 1, ignore stop and assume this is inserting at the front. 
	if start greater than self size, ignore stop and assume this is appending. 
	otherwise, it is replacing part of me and start and stop have to be within my 
	bounds. "
	delta := 0.
	startIndex := start.
	stopIndex := stop.
	start < 1
		ifTrue: [startIndex := stopIndex := 0]
		ifFalse: [startIndex > self size
				ifTrue: [startIndex := stopIndex := self size + 1]
				ifFalse: 
					[(stopIndex < (startIndex - 1) or: [stopIndex > self size])
						ifTrue: [self errorOutOfBounds].
					delta := stopIndex - startIndex + 1]].
	newOrderedCollection := 
		self species new: self size + replacementCollection size - delta.
	1 to: startIndex - 1 do: [:index | newOrderedCollection add: (self at: index)].
	1 to: replacementCollection size do: 
		[:index | newOrderedCollection add: (replacementCollection at: index)].
	stopIndex + 1 to: self size do: [:index | newOrderedCollection add: (self at: index)].
	^newOrderedCollection
]

{ #category : #copying }
OrderedCollection >> copyWith: newElement [ 
	"Answer a copy of the receiver that is 1 bigger than the receiver and 
	includes the argument, newElement, at the end."

	| newCollection |
	newCollection := self copy.
	newCollection add: newElement.
	^newCollection
]

{ #category : #enumerating }
OrderedCollection >> do: aBlock [
	"Override the superclass for performance reasons."

	firstIndex to: lastIndex do: [ :index | 
		aBlock value: (array at: index) ]
]

{ #category : #private }
OrderedCollection >> ensureBoundsFrom: fromIndex to: toIndex [
	(fromIndex < 1)
		ifTrue: [^self errorSubscriptBounds: fromIndex].
	(toIndex + firstIndex - 1 > lastIndex)
		ifTrue: [^self errorSubscriptBounds: toIndex].
]

{ #category : #private }
OrderedCollection >> errorConditionNotSatisfied [

	self error: 'no element satisfies condition'
]

{ #category : #private }
OrderedCollection >> find: oldObject [
  "  This method answers an index in the range firstIndex .. lastIndex, which is meant for internal use only.
     Never use this method in your code, the methods for public use are:
        #indexOf:
        #indexOf:ifAbsent: "

	| index |
	index := firstIndex.
	[index <= lastIndex]
		whileTrue:
			[(array at: index) = oldObject ifTrue: [^ index].
			index := index + 1].
	self errorNotFound: oldObject
]

{ #category : #private }
OrderedCollection >> growAtFirst [
	"Add new empty slots to the front of array, while keeping the empty slots at the end."

	| newArray newFirstIndex newLastIndex |
	newArray := self class arrayType new: (array size * 2 max: 1).
	newFirstIndex := newArray size - array size + firstIndex.
	newLastIndex := newFirstIndex + lastIndex - firstIndex.
	newArray 
		replaceFrom: newFirstIndex
		to: newLastIndex
		with: array
		startingAt: firstIndex.
	array := newArray.
	firstIndex := newFirstIndex.
	lastIndex := newLastIndex
]

{ #category : #private }
OrderedCollection >> growAtLast [
	"Add new empty slots to the end of array, while keeping the empty slots at the front."

	| newArray |
	newArray := self class arrayType new: (array size * 2 max: 1).
	newArray 
		replaceFrom: firstIndex
		to: lastIndex
		with: array
		startingAt: firstIndex.
	array := newArray
]

{ #category : #private }
OrderedCollection >> insert: anObject before: spot [

  "  spot is an index in the range firstIndex .. lastIndex, such an index is not known from outside the collection. 
     Never use this method in your code, it is meant for private use by OrderedCollection only.
     The methods for use are:
        #add:before:   to insert an object before another object
        #add:beforeIndex:   to insert an object before a given position. "
	| "index" delta spotIndex|
	spotIndex := spot.
	delta := spotIndex - firstIndex.
	firstIndex = 1
		ifTrue: 
			[self makeRoomAtFirst.
			spotIndex := firstIndex + delta].
	firstIndex := firstIndex - 1.
	array
		replaceFrom: firstIndex
		to: spotIndex - 2
		with: array
		startingAt: firstIndex + 1.
	array at: spotIndex - 1 put: anObject.
"	index := firstIndex := firstIndex - 1.
	[index < (spotIndex - 1)]
		whileTrue: 
			[array at: index put: (array at: index + 1).
			index := index + 1].
	array at: index put: anObject."
	^ anObject
]

{ #category : #splitjoin }
OrderedCollection >> join: aCollection [ 
	"Append the elements of the argument, aSequenceableCollection, separating them by the receiver."
	
	| result |
	result := self class new.
	aCollection
		do: [:each | each appendTo: result]
		separatedBy: [self appendTo: result].
	^ result
]

{ #category : #private }
OrderedCollection >> makeRoomAtFirst [
	"Make some empty slots at the front of the array. If we have more than 50% free space, then just move the elements, so that the first 50% of the slots are free, otherwise add new free slots to the front by growing. Precondition: firstIndex = 1"
	
	| tally newFirstIndex newLastIndex |
	tally := self size.
	tally * 2 >= array size ifTrue: [ ^self growAtFirst ].
	tally = 0 ifTrue: [ ^self resetTo: array size + 1 ].
	newFirstIndex := array size // 2 + 1.
	newLastIndex := newFirstIndex - firstIndex + lastIndex.
	0 to: tally - 1 do: [ :offset |
		array at: newLastIndex - offset put: (array at: lastIndex - offset) ].
	array from: firstIndex to: newFirstIndex - 1 put: nil.
	firstIndex := newFirstIndex.
	lastIndex := newLastIndex
]

{ #category : #private }
OrderedCollection >> makeRoomAtLast [
	"Make some empty slots at the end of the array. If we have more than 50% free space, then just move the elements, so that the last 50% of the slots are free, otherwise add new free slots to the end by growing. Precondition: lastIndex = array size"
	
	| tally newFirstIndex newLastIndex |
	tally := self size.
	tally * 2 >= lastIndex ifTrue: [ ^self growAtLast ].
	tally = 0 ifTrue: [ ^self resetTo: 1 ].
	newLastIndex := lastIndex // 2.
	newFirstIndex := newLastIndex - lastIndex + firstIndex.
	array 
		replaceFrom: newFirstIndex
		to: newLastIndex
		with: array
		startingAt: firstIndex.
	array from: newLastIndex + 1 to: lastIndex put: nil.
	firstIndex := newFirstIndex.
	lastIndex := newLastIndex
]

{ #category : #copying }
OrderedCollection >> postCopy [
	array := array copy
]

{ #category : #copying }
OrderedCollection >> postCopyFrom: startIndex to: endIndex [ 
	"finish copying the array in a certain range."

	endIndex < startIndex ifFalse: [
		"Because actual size of the array may be greater than used size,
		postCopyFrom:to: may fail to fail and answer an incorrect result
		if this sanity check were not applied."
		(startIndex between: 1 and: self size) ifFalse: [^SubscriptOutOfBounds signalFor: startIndex lowerBound: (1 min: self size) upperBound: self size in: self].
		(endIndex between: 1 and: self size) ifFalse: [^SubscriptOutOfBounds signalFor: endIndex lowerBound: (1 min: self size) upperBound: self size in: self]].
	
	"Add a protection that lacks in Array>>postcopy"
	array := array copyFrom: startIndex + firstIndex - 1 to: (endIndex max: startIndex - 1) + firstIndex - 1.
	firstIndex := 1.
	lastIndex := array size
]

{ #category : #enumerating }
OrderedCollection >> reject: rejectBlock [ 
	"Optimized version of Collection>>#reject:"

	| newCollection element |
	
	newCollection := self copyEmpty.
	
	firstIndex to: lastIndex do: [ :index |
		(rejectBlock value: (element := array at: index))
			ifFalse: [ newCollection addLast: element ]].
		
	^ newCollection
]

{ #category : #enumerating }
OrderedCollection >> reject: rejectBlock thenCollect: collectBlock [
    " Optimized version of Collection>>#reject:thenCollect: "

	| newCollection |

    newCollection := self copyEmpty.

    firstIndex to: lastIndex do: [ :index |
		| element |
		element := array at: index.
		(rejectBlock value: element) 
			ifFalse: [ newCollection addLast: (collectBlock value: element) ]].

    ^ newCollection
]

{ #category : #removing }
OrderedCollection >> remove: oldObject ifAbsent: absentBlock [

	| index |
	index := firstIndex.
	[index <= lastIndex]
		whileTrue: 
			[oldObject = (array at: index)
				ifTrue: 
					[self removeIndex: index.
					^ oldObject]
				ifFalse: [index := index + 1]].
	^ absentBlock value
]

{ #category : #removing }
OrderedCollection >> removeAll [
	"remove all the elements from this collection.
	Keep same amount of storage"
	
	self setCollection: (self class arrayType new: array size)
]

{ #category : #removing }
OrderedCollection >> removeAllSuchThat: aBlock [ 
	"Remove each element of the receiver for which aBlock evaluates to true.
	The method in Collection is O(N^2), this is O(N)."

	| n |
	n := firstIndex.
	firstIndex to: lastIndex do: [:index |
	    (aBlock value: (array at: index)) ifFalse: [
			array at: n put: (array at: index).
			n := n + 1]].
	array from: n to: lastIndex put: nil.
	lastIndex := n - 1
]

{ #category : #removing }
OrderedCollection >> removeAt: index [
	| removed |
	removed := self at: index.
	self removeIndex: index + firstIndex - 1.
	^removed
]

{ #category : #removing }
OrderedCollection >> removeFirst [
	"Remove the first element of the receiver and answer it. If the receiver is 
	empty, create an error notification."
	| firstObject |
	self emptyCheck.
	firstObject := array at: firstIndex.
	array at: firstIndex put: nil.
	firstIndex := firstIndex + 1.
	^ firstObject
]

{ #category : #removing }
OrderedCollection >> removeFirst: n [ 
	"Remove first n object into an array"
	| list |
	list := self class arrayType new: n.
	1
		to: n
		do:
			[ : i | list
				at: i
				put: self removeFirst ].
	^ list
]

{ #category : #private }
OrderedCollection >> removeIndex: removedIndex [
  "  removedIndex is an index in the range firstIndex .. lastIndex, such an index is not known from outside the collection.
    Never use this method in your code, it is meant for private use by OrderedCollection only.
     The method for public use is:
        #removeAt: "

	array 
		replaceFrom: removedIndex 
		to: lastIndex - 1 
		with: array 
		startingAt: removedIndex+1.
	array at: lastIndex put: nil.
	lastIndex := lastIndex - 1.
]

{ #category : #removing }
OrderedCollection >> removeLast [
	"Remove the last element of the receiver and answer it. If the receiver is 
	empty, create an error notification."
	| lastObject |
	self emptyCheck.
	lastObject := array at: lastIndex.
	array at: lastIndex put: nil.
	lastIndex := lastIndex - 1.
	^ lastObject
]

{ #category : #removing }
OrderedCollection >> removeLast: n [ 
	"Remove last n object into an array with last in last position"
	| list |
	list := self class arrayType new: n.
	n
		to: 1
		by: -1
		do:
			[ : i | list
				at: i
				put: self removeLast ].
	^ list
]

{ #category : #removing }
OrderedCollection >> reset [
	"Quickly remove all elements. The objects will be still referenced, but will not be 	accessible."
	
	self resetTo: 1
]

{ #category : #private }
OrderedCollection >> resetTo: index [
	firstIndex := index.
	lastIndex := firstIndex - 1
]

{ #category : #enumerating }
OrderedCollection >> reverseDo: aBlock [
	"Override the superclass for performance reasons."

	lastIndex to: firstIndex by: -1 do: [ :index | 
		aBlock value: (array at: index) ]
]

{ #category : #copying }
OrderedCollection >> reversed [
	"Answer a copy of the receiver with element order reversed.  "
	| newCol |
	newCol := self species new.
	self reverseDo:
		[:elem | newCol addLast: elem].
	^ newCol

"#(2 3 4 'fred') reversed"
]

{ #category : #enumerating }
OrderedCollection >> select: selectBlock [ 
	"Optimized version of Collection>>#select: "

	| newCollection element |
	
	newCollection := self copyEmpty.
	
	firstIndex to: lastIndex do: [ :index |
		(selectBlock value: (element := array at: index))
			ifTrue: [ newCollection addLast: element ]].
		
	^ newCollection
]

{ #category : #enumerating }
OrderedCollection >> select: selectBlock thenCollect: collectBlock [
    " Optimized version Collection>>#select:thenCollect: "

	| newCollection element |

    newCollection := self copyEmpty.

    firstIndex to: lastIndex do: [ :index |
		element := array at: index.
		(selectBlock value: element) 
			ifTrue: [ newCollection addLast: (collectBlock value: element) ]].

    ^ newCollection
]

{ #category : #private }
OrderedCollection >> setCollection: anArray [
	array := anArray.
	self reset
]

{ #category : #private }
OrderedCollection >> setContents: anArray [
	array := anArray.
	firstIndex := 1.
	lastIndex := array size.
]

{ #category : #accessing }
OrderedCollection >> size [
	"Answer how many elements the receiver contains."

	^ lastIndex - firstIndex + 1
]

{ #category : #enumerating }
OrderedCollection >> sort: aSortBlock [ 
	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	self size <= 1 ifTrue: [^ self].  "nothing to do"
	array
		mergeSortFrom: firstIndex
		to: lastIndex 
		src: array shallowCopy 
		dst: array 
		by: aSortBlock
]

{ #category : #enumerating }
OrderedCollection >> with: otherCollection collect: twoArgBlock [ 
	"Collect and return the result of evaluating twoArgBlock with 
	corresponding elements from this collection and otherCollection."
	| result |
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	result := self species new: self size.
	1 to: self size do:
		[:index | result addLast: (twoArgBlock value: (self at: index)
									value: (otherCollection at: index))].
	^ result
]

{ #category : #enumerating }
OrderedCollection >> withIndexCollect: elementAndIndexBlock [ 
	"Just like with:collect: except that the iteration index supplies the second argument to the block. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection := self species new: self size.
	firstIndex to: lastIndex do:
		[:index |
		newCollection addLast: (elementAndIndexBlock
			value: (array at: index)
			value: index - firstIndex + 1)].
	^ newCollection
]
