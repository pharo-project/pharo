new instance creation
rows: rowNumber columns: columnNumber
	"Create a grid of rowNUmber rows and columnNumber columns."
	^ self 
		rows: rowNumber 
		columns: columnNumber 
		contents: (Array new: rowNumber*columnNumber)