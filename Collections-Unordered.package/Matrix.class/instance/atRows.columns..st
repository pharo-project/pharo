accessing submatrices
atRows: rs columns: cs
	"Answer a Matrix obtained by slicing the receiver.
	 rs and cs should be sequenceable collections of positive integers."

	^self class rows: rs size columns: cs size tabulate: [:r :c |
		self at: (rs at: r) at: (cs at: c)]