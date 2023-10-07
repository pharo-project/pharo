Extension { #name : 'Heap' }

{ #category : '*Collections-Sequenceable-Tests' }
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

{ #category : '*Collections-Sequenceable-Tests' }
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
