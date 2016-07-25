accessing
at: rowNumber at: columnNumber incrementBy: value
	"Add a value to the element available at rowNumber,columNumber."

	^ contents at: (self indexForRow: rowNumber andColumn: columnNumber) incrementBy: value