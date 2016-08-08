row/column operations
atRow: rowNumber
	(rowNumber between: 1 and: numberOfRows)
		ifFalse: [self error: '1st subscript out of range'].
	^ contents 
			copyFrom: (rowNumber - 1) * numberOfColumns + 1 
			to: rowNumber*numberOfColumns