accessing
at: rowNumber at: columnNumber
	^ contents at: (self indexForRow: rowNumber andColumn: columnNumber)