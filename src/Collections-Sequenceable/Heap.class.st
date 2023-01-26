"
Heap implements a special data structure commonly referred to as ['heap'](http://en.wikipedia.org/wiki/Heap_%28data_structure%29)

Heaps are good at handling priority queues because:
1. Greatest priority element according to the sort block will be stored in first position and thus accessed in O(1) operations
2. Worse time for inserting or removing an element is in O(log n) operations, where n is the size of the Heap. Insertion/Removal times are more efficient than above upper bound, provided that:
    a. Elements are only removed at the beginning
    b. Elements are added with arbitrary sort order.
3. there is no need to fully sort the Heap, which makes it more efficient than a `SortedCollection`

The heap can be fully sorted by sending the message `Heap>>#fullySort`.
Worst time for fully sorting the Heap is in O(n log n) operations, but this is rarely used a feature.
Remember that the Heap does not fully sort the collection if you don't ask.
Thus don't expect `Heap>>#do:` and other iterators to enumerate elements according to the sortBlock order,
however successive calls to `Heap>>#removeFirst` will return the elements in sorted order.

The Heap does arrange to preserve the following invariant:
For any children B of a node A, A is sorted before B, in other words, (self sort: A before: B) = true
This implies that the root is always the first element according to sort order.

## Public API and Key Messages
### Initializing and Adding
```
h := Heap withAll: #(1 13 4 3 89 34 2)
```

```
h := Heap new.
h add: 5.
h add: 12.
h add: 2.
```
### Removing
```
h := Heap withAll: #(13 4 3 89 34 2).
h removeFirst. ""2""
h removeFirst. ""3""
h removeFirst. ""4""
```

## Instance variables
- **array <Array>**: The data repository
- **tally <Integer>**: The number of elements in the heap
- **sortBlock <Block|nil>**: A two-argument block defining the sort order, or nil in which case the default sort order is `[:element1 :element2| element1 <= element2]`
- **indexUpdateBlock <Block|nil>**: A two-argument block of the form `[:data :index | ... ]` which allows an application object to keep track of its index within the heap.  Useful for quick heap update when object's sort value changes (for example, when an object in a priority queue has its priority increased by an external event, you don't want to have to search through the whole heap to find the index before fixing the heap).  No update occurs if nil.

The Heap can be viewed as a binary tree (every node in the tree has at most two children).
The root is stored in first slot of internal array.
The children are stored in next two slots.
The children of children in next four slots.
etc...

For a node A of index i (1 based), the two children B1 and B2 are thus stored in indices (2*i) and (2*i+1).
Of course, the children indices must be less than the tally otherwise they are considered inexistent.



"
Class {
	#name : #Heap,
	#superclass : #Collection,
	#instVars : [
		'array',
		'tally',
		'sortBlock',
		'indexUpdateBlock'
	],
	#classInstVars : [
		'sortBlock'
	],
	#category : #'Collections-Sequenceable-Base'
}

{ #category : #accessing }
Heap class >> defaultSortBlock [
	"Cache the default sort block here. Since the sortBlock is used to compare instances, a single instance
	 for the default block helps us."
	^ sortBlock ifNil: [
		sortBlock := [ :a :b | a <= b]]
]

{ #category : #examples }
Heap class >> heapExample [
	"self heapExample"
	"Create a sorted collection of numbers, remove the elements
	sequentially and add new objects randomly.
	Note: This is the kind of benchmark a heap is designed for."

	^ String streamContents: [ :str |
		| n rnd array time sorted |
		n := 5000. "# of elements to sort"
		rnd := Random new.
		array := (1 to: n) collect:[:i| rnd next].
		"First, the heap version"
		time := Time millisecondsToRun:[
		sorted := self withAll: array.
			1 to: n do:[:i|
				sorted removeFirst.
				sorted add: rnd next].
	].
	str << 'Time for Heap: ' << time printString <<' msecs '; cr.
	"The quicksort version"
	time := Time millisecondsToRun:[
		sorted := SortedCollection withAll: array.
		1 to: n do:[:i|
			sorted removeFirst.
			sorted add: rnd next].
	].
	str << 'Time for SortedCollection: '<< time printString << ' msecs'.]
]

{ #category : #examples }
Heap class >> heapSortExample [
	"self heapSortExample"
	"Sort a random collection of Floats and compare the results with
	SortedCollection (using the quick-sort algorithm) and
	ArrayedCollection>>mergeSortFrom:to:by: (using the merge-sort algorithm)."

	^ String streamContents: [ :str |
		| n rnd array  time sorted |
		n := 10000. "# of elements to sort"
		rnd := Random new.
		array := (1 to: n) collect:[:i| rnd next].
		"First, the heap version"
		time := Time millisecondsToRun:[
			sorted := Heap withAll: array.
			1 to: n do:[:i| sorted removeFirst].
		].
	str << 'Time for heap-sort: ' << time printString << ' msecs ';cr.
	"The quicksort version"
	time := Time millisecondsToRun:[
		sorted := SortedCollection withAll: array.
	].
	str << 'Time for quick-sort: ' << time printString <<' msecs '; cr.
	"The merge-sort version"
	time := Time millisecondsToRun:[
		array mergeSortFrom: 1 to: array size by: [:v1 :v2| v1 <= v2].
	].
	str << 'Time for merge-sort: ' << time printString  << ' msecs'; cr.
	]
]

{ #category : #'instance creation' }
Heap class >> new [
	^self new: 10
]

{ #category : #'instance creation' }
Heap class >> new: n [
	^super new setCollection: (Array new: n)
]

{ #category : #'instance creation' }
Heap class >> sortBlock: aBlock [
	"Create a new heap sorted by the given block"
	^self new sortBlock: aBlock
]

{ #category : #'instance creation' }
Heap class >> withAll: aCollection [
	"Create a new heap with all the elements from aCollection"
	^(self basicNew)
		setCollection: aCollection asArray copy tally: aCollection size;
		reSort;
		yourself
]

{ #category : #'instance creation' }
Heap class >> withAll: aCollection sortBlock: aBlock [
	"Create a new heap with all the elements from aCollection"
	^(self basicNew)
		setCollection: aCollection asArray copy tally: aCollection size;
		sortBlock: aBlock;
		yourself
]

{ #category : #comparing }
Heap >> = anObject [
	"Heap are considered equals only if they have same sort order and same elements."

	self == anObject ifTrue: [^true].
	anObject isHeap ifFalse: [^false].
	self sortBlock = anObject sortBlock ifFalse: [^false].
	self size = anObject size ifFalse: [^false].
	^(self asArray sort: sortBlock) = (anObject asArray sort: sortBlock)
]

{ #category : #adding }
Heap >> add: anObject [
	"Include newObject as one of the receiver's elements. Answer newObject."
	tally = array size ifTrue:[self grow].
	array at: (tally := tally + 1) put: anObject.
	self updateObjectIndex: tally.
	self upHeap: tally.
	^anObject
]

{ #category : #private }
Heap >> array [
	^array
]

{ #category : #accessing }
Heap >> at: index [
	"Heap are not designed to be accessed sequentially."

	self shouldNotImplement
]

{ #category : #accessing }
Heap >> at: index put: newObject [
	"Heap are not designed to be accessed sequentially.
	Please consider using #add: instead."

	self shouldNotImplement
]

{ #category : #enumerating }
Heap >> collect: aBlock [
	^self collect: aBlock as: Array
]

{ #category : #copying }
Heap >> copyEmpty [
	"Answer a copy of the receiver without any of the receiver's elements."

	^self class sortBlock: sortBlock
]

{ #category : #accessing }
Heap >> defaultSortBlock [
	^ self class defaultSortBlock
]

{ #category : #enumerating }
Heap >> do: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument."
	1 to: tally do:[:i| aBlock value: (array at: i)]
]

{ #category : #private }
Heap >> downHeap: anIndex [
	"Check the heap downwards for correctness starting at anIndex.
	 Everything above (i.e. left of) anIndex is ok."
	| value k n j |
	anIndex = 0 ifTrue:[^self].
	n := tally bitShift: -1.
	k := anIndex.
	value := array at: anIndex.
	[k <= n] whileTrue:[
		j := k + k.
		"use max(j,j+1)"
		(j < tally and:[self sorts: (array at: j+1) before: (array at: j)])
				ifTrue:[ j := j + 1].
		"check if position k is ok"
		(self sorts: value before: (array at: j))
			ifTrue:[	"yes -> break loop"
					n := k - 1]
			ifFalse:[	"no -> make room at j by moving j-th element to k-th position"
					array at: k put: (array at: j).
					self updateObjectIndex: k.
					"and try again with j"
					k := j]].
	array at: k put: value.
	self updateObjectIndex: k
]

{ #category : #private }
Heap >> downHeapSingle: anIndex [
	"This version is optimized for the case when only one element in the receiver can be at a wrong position. It avoids one comparison at each node when travelling down the heap and checks the heap upwards after the element is at a bottom position. Since the probability for being at the bottom of the heap is much larger than for being somewhere in the middle this version should be faster."
	| value k n j |
	anIndex = 0 ifTrue:[^self].
	n := tally bitShift: -1.
	k := anIndex.
	value := array at: anIndex.
	[k <= n] whileTrue:[
		j := k + k.
		"use max(j,j+1)"
		(j < tally and:[self sorts: (array at: j+1) before: (array at: j)])
				ifTrue:[	j := j + 1].
		array at: k put: (array at: j).
		self updateObjectIndex: k.
		"and try again with j"
		k := j].
	array at: k put: value.
	self updateObjectIndex: k.
	self upHeap: k
]

{ #category : #accessing }
Heap >> first [
	"Return the first element in the receiver"
	^array at: 1
]

{ #category : #sorting }
Heap >> fullySort [
	"Fully sort the heap.
	This method preserves the heap invariants and can thus be sent safely"
	self privateReverseSort.
	1 to: tally // 2 do: [:i | array swap: i with: 1 + tally - i]
]

{ #category : #growing }
Heap >> grow [
	"Become larger."
	self growTo: self size + self growSize
]

{ #category : #growing }
Heap >> growSize [
	"Return the size by which the receiver should grow if there are no empty slots left."
	^array size max: 5
]

{ #category : #growing }
Heap >> growTo: newSize [
	"Grow to the requested size."
	| newArray |
	newArray := Array new: (newSize max: tally).
	newArray replaceFrom: 1 to: array size with: array startingAt: 1.
	array := newArray
]

{ #category : #accessing }
Heap >> indexUpdateBlock: aBlockOrNil [

	indexUpdateBlock := aBlockOrNil
]

{ #category : #testing }
Heap >> isEmpty [
	"Answer whether the receiver contains any elements."
	^tally = 0
]

{ #category : #testing }
Heap >> isHeap [

	^ true
]

{ #category : #sorting }
Heap >> isSorted [
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
Heap >> isSortedBy: aBlock [
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
Heap >> mergeFirst: first middle: middle last: last into: dst by: aBlock [
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
Heap >> mergeSortFrom: startIndex to: stopIndex by: aBlock [
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
Heap >> mergeSortFrom: first to: last src: src dst: dst by: aBlock [
	"Private. Split the range to be sorted in half, sort each half, and
	merge the two half-ranges into dst."

	| middle |
	first = last ifTrue: [^ self].
	middle := (first + last) // 2.
	self mergeSortFrom: first to: middle src: dst dst: src by: aBlock.
	self mergeSortFrom: middle + 1 to: last src: dst dst: src by: aBlock.
	src mergeFirst: first middle: middle last: last into: dst by: aBlock
]

{ #category : #copying }
Heap >> postCopy [
	super postCopy.
	array := array copy
]

{ #category : #private }
Heap >> privateRemoveAt: index [
	"Remove the element at the given index and make sure the sorting order is okay"
	| removed |
	removed := array at: index.
	array at: index put: (array at: tally).
	array at: tally put: nil.
	tally := tally - 1.
	index > tally ifFalse:[
		"Use #downHeapSingle: since only one element has been removed"
		self downHeapSingle: index].
	^removed
]

{ #category : #private }
Heap >> privateReverseSort [
	"Arrange to have the array sorted in reverse order.
	WARNING: this method breaks the heap invariants. It's up to the sender to restore them afterwards."
	| oldTally |
	oldTally := tally.
	[tally > 1] whileTrue:
		 [array swap: 1 with: tally.
		tally := tally - 1.
		 self downHeapSingle: 1].
	tally := oldTally
]

{ #category : #accessing }
Heap >> reSort [
	"Resort the entire heap"
	self isEmpty ifTrue:[^self].
	tally // 2 to: 1 by: -1 do:[:i| self downHeap: i]
]

{ #category : #removing }
Heap >> remove: oldObject ifAbsent: aBlock [
	"Remove oldObject as one of the receiver's elements. If several of the
	elements are equal to oldObject, only one is removed. If no element is
	equal to oldObject, answer the result of evaluating anExceptionBlock.
	Otherwise, answer the argument, oldObject."
	1 to: tally do:[:i|
		(array at: i) = oldObject ifTrue:[^self privateRemoveAt: i]].
	^aBlock value
]

{ #category : #removing }
Heap >> removeAll [

	array atAllPut: nil.
	tally := 0
]

{ #category : #removing }
Heap >> removeFirst [
	"Remove the first element from the receiver"
	self emptyCheck.
	^self privateRemoveAt: 1
]

{ #category : #private }
Heap >> setCollection: aCollection [
	array := aCollection.
	tally := 0
]

{ #category : #private }
Heap >> setCollection: aCollection tally: newTally [
	array := aCollection.
	tally := newTally
]

{ #category : #accessing }
Heap >> size [
	"Answer how many elements the receiver contains."

	^ tally
]

{ #category : #sorting }
Heap >> sort [
	"Sort this collection into ascending order using the '<=' operator."

	self sort: [:a :b | a <= b]
]

{ #category : #sorting }
Heap >> sort: aSortBlock [
	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	self
		mergeSortFrom: 1
		to: self size
		by: aSortBlock
]

{ #category : #accessing }
Heap >> sortBlock [
	^ sortBlock ifNil: [ sortBlock := self defaultSortBlock ]
]

{ #category : #accessing }
Heap >> sortBlock: aBlock [
	sortBlock := aBlock.
	self reSort
]

{ #category : #sorting }
Heap >> sorted [
	"Return a new sequenceable collection which contains the same elements as self but its
elements are sorted in ascending order using the #'<=' operator."

	^self sorted: [ :a :b| a <= b ]
]

{ #category : #sorting }
Heap >> sorted: aSortBlockOrNil [
	"Return a new sequenceable collection which contains the same elements as self but its
elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if
the first element should preceed the second one. If aSortBlock is nil then <= is used for
comparison."

	^self copy sort: aSortBlockOrNil
]

{ #category : #testing }
Heap >> sorts: element1 before: element2 [
	"Return true if element1 should be sorted before element2.
	This method defines the sort order in the receiver"
	^sortBlock == nil
		ifTrue:[element1 <= element2]
		ifFalse:[sortBlock value: element1 value: element2]
]

{ #category : #growing }
Heap >> trim [
	"Remove any empty slots in the receiver."
	self growTo: self size
]

{ #category : #private }
Heap >> upHeap: anIndex [
	"Check the heap upwards for correctness starting at anIndex.
	 Everything below anIndex is ok."
	| value k kDiv2 tmp |
	anIndex = 0 ifTrue:[^self].
	k := anIndex.
	value := array at: anIndex.
	[ (k > 1) and:[self sorts: value before: (tmp := array at: (kDiv2 := k bitShift: -1))] ]
		whileTrue:[
			array at: k put: tmp.
			self updateObjectIndex: k.
			k := kDiv2].
	array at: k put: value.
	self updateObjectIndex: k
]

{ #category : #private }
Heap >> updateObjectIndex: index [
	"If indexUpdateBlock is not nil, notify the object at index of its new position in the heap array."
	indexUpdateBlock ifNotNil: [
		indexUpdateBlock value: (array at: index) value: index]
]
