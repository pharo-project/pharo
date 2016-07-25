examples
heapSortExample	"HeapTest new heapSortExample"
	"Sort a random collection of Floats and compare the results with
	SortedCollection (using the quick-sort algorithm) and 
	ArrayedCollection>>mergeSortFrom:to:by: (using the merge-sort algorithm)."
	| n rnd array  time sorted |
	n := 10000. "# of elements to sort"
	rnd := Random new.
	array := (1 to: n) collect:[:i| rnd next].
	"First, the heap version"
	time := Time millisecondsToRun:[
		sorted := Heap withAll: array.
		1 to: n do:[:i| sorted removeFirst].
	].
	Transcript cr; show:'Time for heap-sort: ', time printString,' msecs'.
	"The quicksort version"
	time := Time millisecondsToRun:[
		sorted := SortedCollection withAll: array.
	].
	Transcript cr; show:'Time for quick-sort: ', time printString,' msecs'.
	"The merge-sort version"
	time := Time millisecondsToRun:[
		array mergeSortFrom: 1 to: array size by: [:v1 :v2| v1 <= v2].
	].
	Transcript cr; show:'Time for merge-sort: ', time printString,' msecs'.
