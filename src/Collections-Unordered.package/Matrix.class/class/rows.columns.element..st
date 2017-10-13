instance creation
rows: rowNumber columns: columnNumber element: element
	"Create a Matrix of rowNumber rows and columnNumber columns filled with element."
	^ self 
		rows: rowNumber 
		columns: columnNumber
		contents: ((Array new: rowNumber * columnNumber) atAllPut: element; yourself)