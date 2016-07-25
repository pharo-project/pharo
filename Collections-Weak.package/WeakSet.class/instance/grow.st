private
grow
	"Grow the elements array if needed.
	Since WeakSets just nil their slots, alot of the occupied (in the eyes of the set) slots are usually 	empty. Doubling size if unneeded can lead to BAD performance, therefore we see if reassigning 	the <live> elements to a Set of similiar size leads to a sufficiently (50% used here) empty set first.
	and reinsert the old elements"

	| oldTally |
	oldTally := tally.
	self growTo: array size.
	oldTally >> 1 < tally
		ifTrue: [ self growTo: (HashTableSizes atLeast: 2 * array size) ]