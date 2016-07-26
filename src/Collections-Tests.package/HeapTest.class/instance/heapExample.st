examples
heapExample	"HeapTest new heapExample"
	"Create a sorted collection of numbers, remove the elements
	sequentially and add new objects randomly.
	Note: This is the kind of benchmark a heap is designed for."
	| n rnd array time sorted |
	n := 5000. "# of elements to sort"
	rnd := Random new.
	array := (1 to: n) collect:[:i| rnd next].
	"First, the heap version"
	time := Time millisecondsToRun:[
		sorted := Heap withAll: array.
		1 to: n do:[:i| 
			sorted removeFirst.
			sorted add: rnd next].
	].
	Transcript cr; show:'Time for Heap: ', time printString,' msecs'.
	"The quicksort version"
	time := Time millisecondsToRun:[
		sorted := SortedCollection withAll: array.
		1 to: n do:[:i| 
			sorted removeFirst.
			sorted add: rnd next].
	].
	Transcript cr; show:'Time for SortedCollection: ', time printString,' msecs'.
