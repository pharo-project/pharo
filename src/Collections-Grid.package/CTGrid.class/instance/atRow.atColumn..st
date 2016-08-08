accessing element
atRow: rowNumber atColumn: columnNumber
	^ contents at: (self indexForRow: rowNumber andColumn: columnNumber)