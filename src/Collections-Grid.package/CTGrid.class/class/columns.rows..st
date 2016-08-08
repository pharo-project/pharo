new instance creation
columns: columnNumber rows: rowNumber 
	"Create a grid of rowNUmber rows and columnNumber columns."
	^ self 
		rows: rowNumber 
		columns: columnNumber 
		contents: (Array new: rowNumber*columnNumber)