private
indexForRow: row andColumn: column
	(row between: 1 and: numberOfRows)
		ifFalse: [self error: '1st subscript out of range'].
	(column between: 1 and: numberOfColumns)
		ifFalse: [self error: '2nd subscript out of range'].
	^(row-1) * numberOfColumns + column