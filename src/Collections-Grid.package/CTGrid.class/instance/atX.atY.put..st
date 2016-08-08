to deprecate
atX: columnNumber atY: rowNumber put: value
	"Put value at columnNumber @ rowNumber"
	^ contents at: (self indexForRow: rowNumber andColumn: columnNumber) put: value