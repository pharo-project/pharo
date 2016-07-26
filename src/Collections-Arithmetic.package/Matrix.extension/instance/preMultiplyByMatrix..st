*Collections-arithmetic
preMultiplyByMatrix: m
	"Answer m +* self where m is a Matrix."
	|s|
	numberOfRows = m numberOfColumns ifFalse: [self error: 'dimensions do not conform'].
	^ Matrix 
		rows: m numberOfRows 
		columns: numberOfColumns 
		tabulate: [:row :col |
			s := 0.
			1 to: numberOfRows do: [:k | s := (m at: row at: k) * (self at: k at: col) + s].
			s ]