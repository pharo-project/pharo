"
I represent a set of methods related to element sorting. 

My public api is composed of the messages:
        #sort, #isSorted, #isSortedBy:, and #sort: 

To get the behavior I define, my users should implement:
	#isEmpty, #size, #first, and #at: 
"
Trait {
	#name : #TSortable,
	#category : #'Collections-Abstract'
}

{ #category : #sorting }
TSortable >> isSorted [
	"Return true if the receiver is sorted by the given criterion.
	Optimization for isSortedBy: [:a :b | a <= b]."

	| lastElm elm |
	self isEmpty ifTrue: [^ true].
	lastElm := self first.
	2 to: self size do: 
		[:index | 
		elm := self at: index.
		lastElm <= elm ifFalse: [^ false].
		lastElm := elm].
	^ true
]

{ #category : #sorting }
TSortable >> isSortedBy: aBlock [
	"Return true if the receiver is sorted by the given criterion."

	| lastElm elm |
	self isEmpty ifTrue: [^ true].
	lastElm := self first.
	2 to: self size do: 
		[:index | 
		elm := self at: index.
		(aBlock value: lastElm value: elm) ifFalse: [^ false].
		lastElm := elm].
	^ true
]

{ #category : #sorting }
TSortable >> mergeFirst: first middle: middle last: last into: dst by: aBlock [
	"Private. Merge the sorted ranges [first..middle] and [middle+1..last] 
	of the receiver into the range [first..last] of dst."

	| i1 i2 val1 val2 out |
	i1 := first.
	i2 := middle + 1.
	val1 := self at: i1.
	val2 := self at: i2.
	out := first - 1.  "will be pre-incremented"

	"select 'lower' half of the elements based on comparator"
	[(i1 <= middle) and: [i2 <= last]] whileTrue:
		[(aBlock value: val1 value: val2)
			ifTrue: [dst at: (out := out + 1) put: val1.
					val1 := self at: (i1 := i1 + 1)]
			ifFalse: [dst at: (out := out + 1) put: val2.
					i2 := i2 + 1.
					i2 <= last ifTrue: [val2 := self at: i2]]].

	"copy the remaining elements"
	i1 <= middle
		ifTrue: [dst replaceFrom: out + 1 to: last with: self startingAt: i1]
		ifFalse: [dst replaceFrom: out + 1 to: last with: self startingAt: i2]
]

{ #category : #sorting }
TSortable >> mergeSortFrom: startIndex to: stopIndex by: aBlock [
	"Sort the given range of indices using the mergesort algorithm.
	Mergesort is a worst-case O(N log N) sorting algorithm that usually
	does only half as many comparisons as heapsort or quicksort."

	"Details: recursively split the range to be sorted into two halves,
	mergesort each half, then merge the two halves together. An extra 
	copy of the data is used as temporary storage and successive merge 
	phases copy data back and forth between the receiver and this copy.
	The recursion is set up so that the final merge is performed into the
	receiver, resulting in the receiver being completely sorted."

	self size <= 1 ifTrue: [^ self].  "nothing to do"
	startIndex = stopIndex ifTrue: [^ self].
	[startIndex >= 1 and: [startIndex < stopIndex]] assert. "bad start index"
	[stopIndex <= self size] assert. "bad stop index"
	self
		mergeSortFrom: startIndex
		to: stopIndex 
		src: self copy 
		dst: self 
		by: aBlock
]

{ #category : #sorting }
TSortable >> mergeSortFrom: first to: last src: src dst: dst by: aBlock [
	"Private. Split the range to be sorted in half, sort each half, and 
	merge the two half-ranges into dst."

	| middle |
	first = last ifTrue: [^ self].
	middle := (first + last) // 2.
	self mergeSortFrom: first to: middle src: dst dst: src by: aBlock.
	self mergeSortFrom: middle + 1 to: last src: dst dst: src by: aBlock.
	src mergeFirst: first middle: middle last: last into: dst by: aBlock
]

{ #category : #sorting }
TSortable >> sort [
	"Sort this collection into ascending order using the '<=' operator."

	self sort: [:a :b | a <= b]
]

{ #category : #sorting }
TSortable >> sort: aSortBlock [ 
	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	self
		mergeSortFrom: 1
		to: self size
		by: aSortBlock
]

{ #category : #sorting }
TSortable >> sorted [
	"Return a new sequenceable collection which contains the same elements as self but its 
elements are sorted in ascending order using the #'<=' operator."
	
	^self sorted: [ :a :b| a <= b ]
]

{ #category : #sorting }
TSortable >> sorted: aSortBlockOrNil [
	"Return a new sequenceable collection which contains the same elements as self but its 
elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if 
the first element should preceed the second one. If aSortBlock is nil then <= is used for 
comparison."
	
	^self copy sort: aSortBlockOrNil
]
