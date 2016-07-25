*Collections-arithmetic
preMultiplyByArray: a
	"Answer a +* self where a is an Array."

	numberOfRows = 1 ifFalse: [self error: 'dimensions do not conform'].
	^Matrix rows: a size columns: numberOfColumns tabulate: [:row :col |
		(a at: row) * (contents at: col)]
