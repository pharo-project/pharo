querying
fastFindFirstLineSuchThat: lineBlock
	"Perform a binary search of the lines array and return the index
	of the first element for which lineBlock evaluates as true.
	This assumes the condition is one that goes from false to true for
	increasing line numbers (as, eg, yval > somey or start char > somex).
	If lineBlock is not true for any element, return size+1."
	| index low high |
	low := 1.
	high := self lines size.
	[index := high + low // 2.
	low > high]
		whileFalse: 
			[(lineBlock value: (self lines at: index))
				ifTrue: [high := index - 1]
				ifFalse: [low := index + 1]].
	^ low