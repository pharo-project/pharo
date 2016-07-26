instance creation
rows: rowNumber columns: columnNumber
	"Create a Matrix of rowNUmber rows and columnNumber columns."
	^ self 
		rows: rowNumber 
		columns: columnNumber 
		contents: (Array new: rowNumber*columnNumber)