"
I represent an unordered collection of possibly duplicate elements.
	
I store these elements in a dictionary, tallying up occurrences of equal objects. Because I store an occurrence only once, my clients should beware that objects they store will not necessarily be retrieved such that == is true. If the client cares, a subclass of me should be created.
"
Class {
	#name : #Bag,
	#superclass : #Collection,
	#instVars : [
		'contents'
	],
	#category : 'Collections-Unordered'
}

{ #category : #'instance creation' }
Bag class >> contentsClass [
	^Dictionary
]

{ #category : #'instance creation' }
Bag class >> new [
	^ self new: 4
]

{ #category : #'instance creation' }
Bag class >> new: nElements [
	^ super new setContents: (self contentsClass new: nElements)
]

{ #category : #'instance creation' }
Bag class >> newFrom: aCollection [ 
	"Answer an instance of me containing the same elements as aCollection."

	^ self withAll: aCollection

"Examples:
	Bag newFrom: {1. 2. 3. 3}
	{1. 2. 3. 3} as: Bag
"
]

{ #category : #comparing }
Bag >> = aBag [
	"Two bags are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same size.
	 (c) each element occurs the same number of times in both of them"

	(aBag isKindOf: Bag) ifFalse: [^false].
	self size = aBag size ifFalse: [^false].
	contents associationsDo: [:assoc|
		(aBag occurrencesOf: assoc key) = assoc value
			ifFalse: [^false]].
	^true


]

{ #category : #adding }
Bag >> add: newObject [ 
	"Include newObject as one of the receiver's elements. Answer newObject."

	^ self add: newObject withOccurrences: 1
]

{ #category : #adding }
Bag >> add: newObject withOccurrences: anInteger [ 
	"Add newObject anInteger times to the receiver. Answer newObject."

	contents at: newObject put: (contents at: newObject ifAbsent: [0]) + anInteger.
	^ newObject
]

{ #category : #converting }
Bag >> asBag [
	^ self
]

{ #category : #converting }
Bag >> asSet [
	"Answer a set with the elements of the receiver."

	^ contents keys asSet
]

{ #category : #accessing }
Bag >> at: index [ 
	self errorNotKeyed
]

{ #category : #accessing }
Bag >> at: index put: anObject [ 
	self errorNotKeyed
]

{ #category : #accessing }
Bag >> cumulativeCounts [
	"Answer with a collection of cumulative percents covered by elements so far."
	| s n |
	s := self size / 100.0. n := 0.
	^ self sortedCounts asArray collect:
		[:a | n := n + a key. (n / s roundTo: 0.1) -> a value]
]

{ #category : #enumerating }
Bag >> do: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements as the argument."

	contents associationsDo: [:assoc | assoc value timesRepeat: [aBlock value: assoc key]]
]

{ #category : #enumerating }
Bag >> doWithOccurrences: aTwoArgBlock [ 
    "Iterate over the receiver and apply a two argument block on the element and its occurrences."
        
    contents associationsDo: [:assoc | aTwoArgBlock value: assoc key value: assoc value ]
]

{ #category : #testing }
Bag >> includes: anObject [ 
	"Refer to the comment in Collection|includes:."

	^contents includesKey: anObject
]

{ #category : #enumerating }
Bag >> keysAndValuesDo: aTwoArgBlock [ 
    "Iterate over the receiver and apply a two argument block on the element and its occurrences."
     
    contents associationsDo: [:assoc | aTwoArgBlock value: assoc key value: assoc value ]
]

{ #category : #testing }
Bag >> occurrencesOf: anObject [ 
	"Refer to the comment in Collection|occurrencesOf:."

	(self includes: anObject)
		ifTrue: [^contents at: anObject]
		ifFalse: [^0]
]

{ #category : #copying }
Bag >> postCopy [
	super postCopy.
	contents := contents copy
]

{ #category : #removing }
Bag >> remove: oldObject ifAbsent: exceptionBlock [ 
	"Refer to the comment in Collection|remove:ifAbsent:."

	| count |
	count := contents at: oldObject ifAbsent: [^ exceptionBlock value].
	count = 1
		ifTrue: [contents removeKey: oldObject]
		ifFalse: [contents at: oldObject put: count - 1].
	^ oldObject
]

{ #category : #removing }
Bag >> removeAll [
	"Implementation Note: as contents will be overwritten, a shallowCopy of self would be modified.
	An alternative implementation preserving capacity would be to create a new contents:
	self setContents: (self class contentsClass new: contents size)."
	
	contents removeAll
]

{ #category : #private }
Bag >> setContents: aDictionary [
	contents := aDictionary
]

{ #category : #accessing }
Bag >> size [
	"Answer how many elements the receiver contains."

	| tally |
	tally := 0.
	contents do: [:each | tally := tally + each].
	^ tally
]

{ #category : #accessing }
Bag >> sortedCounts [
	"Answer with a collection of counts with elements, sorted by decreasing
	count."
	"Suggested by l. Uzonyi"

	^(Array new: contents size streamContents: [ :stream |
 			contents associationsDo: [ :each |
 				stream nextPut: each value -> each key ] ])
 		sort: [:x :y | x >= y ];
 		yourself
]

{ #category : #accessing }
Bag >> sortedElements [
	"Answer with a collection of elements with counts, sorted by element."
	"Suggested by l. Uzonyi"
	
	^contents associations
 		sort;
 		yourself
]

{ #category : #'math functions' }
Bag >> sum [
	"Faster than the superclass implementation when you hold many instances of the same value (which you probably do, otherwise you wouldn't be using a Bag)."
	
	| sum first |
	first := true.
	contents keysAndValuesDo: [ :value :count |
		first 
			ifTrue: [ sum := value * count. first := false ]
			ifFalse: [ sum := sum + (value * count) ] ].
	first ifTrue: [ self errorEmptyCollection ].
	^sum
]

{ #category : #accessing }
Bag >> valuesAndCounts [

	^ contents
]
