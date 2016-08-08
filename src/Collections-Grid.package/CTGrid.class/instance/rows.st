accessing - rows
rows
	"Returns the list of items grouped by rows."
	
	| rows | 
	rows := Array new: numberOfRows.
	1 to: numberOfRows do:  
			[ :rowNumber | 
					rows 
						at: rowNumber 
						put: (contents copyFrom: 1 + ((rowNumber-1) * numberOfColumns) to: numberOfColumns * rowNumber )].
	^ rows
	