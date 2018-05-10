"
I'm a Set with weak elements. 

Implementation.

	flag is a special object that indicates that at a given slot the set has a free entry.
"
Class {
	#name : #WeakSet,
	#superclass : #Set,
	#instVars : [
		'flag'
	],
	#category : #'Collections-Weak-Base'
}

{ #category : #adding }
WeakSet >> add: newObject [
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject"

	| index element |
	index := self scanFor: newObject.
	((element := array at: index) == flag or: [ element == nil ])
		ifTrue: [self atNewIndex: index put: newObject asSetElement].
	^newObject
]

{ #category : #accessing }
WeakSet >> anyOne [
	"First we will try to return any real object which is not garbage collected. It will skip all slots with flag. In case when everything is garbage collected we will return nil. WeakSet is not empty in this case (isEmpty will return false). So we should not signal any error"
	| result |
	result := super anyOne.
	^result == self 
		ifTrue: [ nil ] 
		ifFalse: [ result ]
]

{ #category : #converting }
WeakSet >> asArray [

	| newArray |
	
	newArray := OrderedCollection new.
	
	self do: [:each | newArray add: each].
	
	^ newArray asArray
]

{ #category : #enumerating }
WeakSet >> collect: aBlock [

	| newSet |
	newSet := self species new: self size.
	tally = 0 ifTrue: [ ^newSet ].
	1 to: array size do: [ :index |
		(array at: index) ifNotNil: [ :object |
			object == flag ifFalse: [
				newSet add: (aBlock value: object enclosedSetElement) ] ] ].
	^newSet
]

{ #category : #enumerating }
WeakSet >> do: aBlock [

	tally = 0 ifTrue: [ ^self ].
	1 to: array size do: [ :index |
		(array at: index) ifNotNil: [ :object |
			object  == flag ifFalse: [
				aBlock value: object enclosedSetElement] ] ]
]

{ #category : #public }
WeakSet >> do: aBlock after: anElement [

	| startIndex |
	tally = 0 ifTrue: [ ^self ].
	startIndex := anElement
		ifNil: [ 0 ]
		ifNotNil: [ self scanFor: anElement ].
	startIndex + 1 to: array size do: [ :index |
		(array at: index) ifNotNil: [ :object |
			object == flag ifFalse: [
				aBlock value: object enclosedSetElement] ] ]
]

{ #category : #private }
WeakSet >> fixCollisionsFrom: start [
	"The element at start has been removed and replaced by flag.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one."

	| element index |
	index := start.
	[ (element := array at: (index := index \\ array size + 1)) == flag ] whileFalse: [
		element 
			ifNil: [ "This object is gone"
				array at: index put: flag.
				tally := tally - 1 ]
			ifNotNil: [
				| newIndex |
				(newIndex := self scanFor: element enclosedSetElement) = index ifFalse: [
					array 
						at: newIndex put: element;
						at: index put: flag ] ] ]

]

{ #category : #private }
WeakSet >> grow [
	"Grow the elements array if needed.
	Since WeakSets just nil their slots, alot of the occupied (in the eyes of the set) slots are usually 	empty. Doubling size if unneeded can lead to BAD performance, therefore we see if reassigning 	the <live> elements to a Set of similiar size leads to a sufficiently (50% used here) empty set first.
	and reinsert the old elements"

	| oldTally |
	oldTally := tally.
	self growTo: array size.
	oldTally >> 1 < tally
		ifTrue: [ self growTo: (HashTableSizes atLeast: 2 * array size) ]
]

{ #category : #private }
WeakSet >> growTo: anInteger [
	"Grow the elements array and reinsert the old elements"

	| oldElements |
	oldElements := array.
	array := WeakArray new: anInteger withAll: flag.
	self noCheckNoGrowFillFrom: oldElements
]

{ #category : #testing }
WeakSet >> includes: anObject [ 
	
	^(array at: (self scanFor: anObject))
		ifNil: [ false ]
		ifNotNil: [ :object | object ~~ flag ]
]

{ #category : #initialization }
WeakSet >> initialize: n [
	"Initialize array to an array size of n"

	flag := Object new.
	array := WeakArray new: n.
	array atAllPut: flag.
	tally := 0
]

{ #category : #testing }
WeakSet >> isHealthy [
	"Test that object hashes match their positions stored in set's array,
	answer true if everything ok, false otherwise
	
	WeakSet allSubInstances select: [:badSet |
		badSet isHealthy not ]
	"
	array withIndexDo: [ :element :index |
		(element isNotNil and: [ element ~~ flag ]) ifTrue: [
			(self scanFor: element) == index
				ifFalse: [ ^ false ]]].
	^ true
]

{ #category : #accessing }
WeakSet >> like: anObject [
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties"

	| element |
	^(element  := array at: (self scanFor: anObject)) == flag
		ifFalse: [ element enclosedSetElement]
]

{ #category : #accessing }
WeakSet >> like: anObject ifAbsent: aBlock [
	"Answer an object in the receiver that is equal to anObject,
	or evaluate the block if not found. Relies heavily on hash properties"

	| element |
	((element  := array at: (self scanFor: anObject)) == flag or: [ element == nil ])
		ifTrue: [ ^aBlock value ]
		ifFalse: [ ^element enclosedSetElement ]
]

{ #category : #private }
WeakSet >> noCheckNoGrowFillFrom: anArray [
	"Add the elements of anArray except nils and flag to me assuming that I don't contain any of them, they are unique and I have more free space than they require."

	tally := 0.
	1 to: anArray size do: [ :index |
		(anArray at: index) ifNotNil: [ :object |
			object == flag ifFalse: [ 
				array
					at: (self scanForEmptySlotFor: object enclosedSetElement)
					put: object.
				tally := tally + 1 ] ] ]
]

{ #category : #copying }
WeakSet >> postCopy [
	| oldFlag |
	super postCopy.
	oldFlag := flag.
	flag := Object new.
	array replaceAll: oldFlag with: flag.
]

{ #category : #public }
WeakSet >> printElementsOn: aStream [
	| oldPos |
	aStream nextPut: $(.
	oldPos := aStream position.
	self do: [:element | aStream print: element; space].
	aStream position > oldPos ifTrue: [aStream skip: -1 "remove the extra space"].
	aStream nextPut: $)
]

{ #category : #private }
WeakSet >> rehash [
	self growTo: array size
]

{ #category : #removing }
WeakSet >> remove: oldObject ifAbsent: aBlock [

	| index |
	index := self scanFor: oldObject.
	(array at: index) == flag ifTrue: [ ^ aBlock value ].
	array at: index put: flag.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^oldObject
]

{ #category : #private }
WeakSet >> scanFor: anObject [
	"Scan the key array for the first slot containing either flag (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or raise an error if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start |
	index := start := anObject hash \\ array size + 1.
	[ 
		| element |
		((element := array at: index) == flag or: [ element enclosedSetElement = anObject ])
			ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace
]

{ #category : #private }
WeakSet >> scanForEmptySlotFor: aKey [
	"Scan the key array for the first slot containing an empty slot (indicated by flag or a nil). Answer the index of that slot. This method will be overridden in various subclasses that have different interpretations for matching elements."
	
	| index start |
	index := start := aKey hash \\ array size + 1.
	[ 
		| element |
		((element := array at: index) == flag or: [ element == nil ]) ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace
]

{ #category : #private }
WeakSet >> scanForLoadedSymbol: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements"

	| element start finish |

	start := (anObject hash \\ array size) + 1.
	finish := array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == flag or: [element asString = anObject asString])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == flag or: [element asString = anObject asString])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]

{ #category : #public }
WeakSet >> slowSize [
	"Careful! Answer the maximum amount
	of elements in the receiver, not the
	exact amount"

	| count |
	count := 0.
	1 to: array size do: [ :index |
		(array at: index) ifNotNil: [ :object |
			object == flag ifFalse: [
				count := count + 1 ] ] ].
	^count
]
