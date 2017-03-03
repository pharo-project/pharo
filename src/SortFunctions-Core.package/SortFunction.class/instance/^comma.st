converting
, aSortFunction
	"Return a new SortFunction which is the concatenation of aSortFunction to me, I will be the primary sort, but if I compare equal, I will defer to the argument."

	^(ChainedSortFunction new)
		collator: collator;
		direction: direction;
		next: aSortFunction asSortFunction