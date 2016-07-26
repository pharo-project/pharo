private
growSize
	"Answer what my next higher table size should be"

	^HashTableSizes atLeast: self capacity * 3 // 2 + 2
	