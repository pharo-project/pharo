accessing subgrids
atRows: rs columns: cs
	"Answer a grid obtained by slicing the receiver. rs and cs should be sequenceable collections."

	^self class rows: rs size columns: cs size tabulate: [:r :c |
		self atRow: (rs at: r) atColumn: (cs at: c)]