accessing subgrids
atRows: r1 to: r2 columns: c1 to: c2
	"Answer a submatrix [r1..r2][c1..c2] of the receiver."
	|rd cd|

	rd := r1 - 1.
	cd := c1 - 1.
	^self class rows: r2-rd columns: c2-cd tabulate: [:r :c| self atRow: r+rd atColumn: c+cd]
