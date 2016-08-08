accessing element
atRow: rowNumber atColumn: columnNumber put: value
	"Put value at rowNumber,columnNumber"
	^ contents at: (self indexForRow: rowNumber andColumn: columnNumber) put: value