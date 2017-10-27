*SortFunctions-Core-controlling
ascending
	"Return a SortFunction around the receiver. If the receiver is a 2 arg block, it is assumed it will do the collation directly itself, returning -1, 0, or 1. If the receiver is a one arg block, it will be evaluated for each a and b and of the sort input, and the result of sending <=> to those will be used."

	^self asSortFunction beAscending