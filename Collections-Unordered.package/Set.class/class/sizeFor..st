instance creation
sizeFor: nElements
	"Large enough size to hold nElements with some slop (see fullCheck)"

	^ HashTableSizes atLeast: nElements * 4 // 3