sorting
mergeSortFrom: startIndex to: stopIndex by: aBlock
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