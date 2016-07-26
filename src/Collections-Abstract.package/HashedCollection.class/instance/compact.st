private
compact
	"Reduce the size of array so that the load factor will be ~75%."
	
	| newCapacity |
	newCapacity := HashTableSizes atLeast: tally * 4 // 3.
	self growTo: newCapacity