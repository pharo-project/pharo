private
grow
	"Grow the elements array and reinsert the old elements"

	| oldElements |
	oldElements := array.
	array := Array new: (HashTableSizes atLeast: oldElements size * 2).
	tally := 0.
	oldElements
		do:
			[ :each | 
			each == nil
				ifFalse: [ self noCheckAdd: each enclosedSetElement] ]