"
I represent an unordered collection of possibly duplicate elements.
	
I store these elements in a dictionary, tallying up occurrences of equal objects. Because I store an occurrence only once, my clients should beware that objects they store will not necessarily be retrieved such that == is true. If the client cares, a subclass of me should be created.

## Creating 
Bags can be created from other collections or built up progressively using the `Bag>>#add:` message. The example below shows four different ways to create a `Bag` with 3 occurrences of 1.
```
b := #(1 1 1) asBag.
""or""
b:= Bag newFrom: #(1 1 1).
""or""
b := Bag new add: 1; add: 1; add: 1.
""or"" 
b add: 1 withOccurrences: 3.

b valuesAndCounts.  ""Dictionary(1->3)""
```

## Enumerating

It is possible to iterate over each individual occurrance of an object using the `Bag>>#do:` message, or pairs of objects and their occurances using `Bag>>#doWithOccurrences:` or the synonymous messages `Bag>>#associationsDo:` and `Bag>>#keysAndValuesDo:`.

```
b := #(1 1 1 1) asBag.

""The block will receive each occurrence. obj = 1 and there will be 4 iterations""
|counter|
counter := 0
b do: [:obj | counter := counter + 1].
counter = 4  

""The block will receive the obj and it's count. obj = 1 count = 4""
b doWithOccurences: [:obj :count | ...].
```

## Accessing 
As well as enumerating the contents it is possible to get a dictionary of all object counts using the `Bag>>#valuesAndCounts` message or get the counts of a single object using `Bag>>#occurrencesOf:`.

```
b := #(1 1 1 2 2 3) asBag.
b valuesAndCounts.  ""Dictionary(1->3 2->2 3->1)""
b occurrencesOf: 1  ""3""
```
"
Class {
	#name : #Bag,
	#superclass : #Collection,
	#instVars : [
		'contents'
	],
	#category : #'Collections-Unordered-Bags'
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
	"#(1 2 2 3 1 1 1) asBag asSet >>> #(1 2 2 3 1 1 1) asSet"

	^ contents keys asSet
]

{ #category : #enumerating }
Bag >> associationsDo: aBlock [
	"Evaluate aBlock for each of the receiver's elements (key/value
	associations).  Provided for compatibility with Dictionaries"

	contents associationsDo: aBlock
]

{ #category : #accessing }
Bag >> cumulativeCounts [
	"Answer with a collection of cumulative percents covered by elements so far."

	"#(1 2 2 3 1 1 1) asBag cumulativeCounts >>> {57.1->1 . 85.7->2 . 100.0->3}"

	| s n |
	s := self size / 100.0. n := 0.
	^ self sortedCounts asArray collect:
		[:a | n := n + a key. (n / s roundTo: 0.1) -> a value]
]

{ #category : #enumerating }
Bag >> do: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.

	This will enumerate all of the occurrences of the object. Use doWithOccurrences:
	to gets pairs of objects and their count.
	"

	contents associationsDo: [:assoc | assoc value timesRepeat: [aBlock value: assoc key]]
]

{ #category : #enumerating }
Bag >> doWithOccurrences: aTwoArgBlock [
    "Iterate over the receiver and apply a two argument block on the element and its occurrences."

    contents associationsDo: [:assoc | aTwoArgBlock value: assoc key value: assoc value ]
]

{ #category : #testing }
Bag >> includes: anObject [
	"Answer whether anObject is one of the receiver's elements."

	"(#(1 2 2 3 1 1 1) asBag includes: 5) >>> false"
	"(#(1 2 2 3 1 1 1) asBag includes: 1) >>> true"

	^ contents includesKey: anObject
]

{ #category : #enumerating }
Bag >> keysAndValuesDo: aTwoArgBlock [
    "Iterate over the receiver and apply a two argument block on the element and its occurrences."

    contents associationsDo: [:assoc | aTwoArgBlock value: assoc key value: assoc value ]
]

{ #category : #testing }
Bag >> occurrencesOf: anObject [
	"Answer how many of the receiver's elements are equal to anObject."

	"(#(1 2 2 3 1 1 1) asBag occurrencesOf: 1) >>> 4"

	^ (self includes: anObject)
		ifTrue: [ contents at: anObject]
		ifFalse: [ 0 ]
]

{ #category : #copying }
Bag >> postCopy [
	super postCopy.
	contents := contents copy
]

{ #category : #removing }
Bag >> remove: oldObject ifAbsent: exceptionBlock [
	"Remove oldObject from the receiver's elements. If several of the
	elements are equal to oldObject, only one is removed. If no element is
	equal to oldObject, answer the result of evaluating anExceptionBlock.
	Otherwise, answer the argument, oldObject."

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

{ #category : #removing }
Bag >> removeKey: oldObject ifAbsent: exceptionBlock [
	"Remove oldObject's entry (e.g., the object and all its occurrences) from the receiver's elements.
	If no element is equal to oldObject, answer the result of evaluating anExceptionBlock.
	Otherwise, answer the argument, oldObject."

	| count |
	count := contents at: oldObject ifAbsent: [^ exceptionBlock value].
	contents removeKey: oldObject.
	^ oldObject
]

{ #category : #private }
Bag >> setContents: aDictionary [
	contents := aDictionary
]

{ #category : #accessing }
Bag >> size [
	"Answer how many elements the receiver contains."

	"#(1 2 2 3 1 1 1) asBag size >>> 7"

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

	"#(1 2 2 3 1 1 1) asBag sortedElements >>> {1->4. 2->2. 3->1}"

	^contents associations
 		sort;
 		yourself
]

{ #category : #'math functions' }
Bag >> sum [
	"Return the sum (+) of the elements held in the receiver."
	"Faster than the superclass implementation when you hold many instances of the same value (which you probably do, otherwise you wouldn't be using a Bag)."

	"#(1 2 2 3 1 1 1) asBag sum >>> 11"

	| sum first |
	first := true.
	contents keysAndValuesDo: [ :value :count |
		first
			ifTrue: [ sum := value * count. first := false ]
			ifFalse: [ sum := sum + (value * count) ] ].
	first ifTrue: [ self errorEmptyCollection ].
	^ sum
]

{ #category : #accessing }
Bag >> valuesAndCounts [

	^ contents
]
