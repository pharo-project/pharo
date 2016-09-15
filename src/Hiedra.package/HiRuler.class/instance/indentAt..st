accessing
indentAt: rowIndex
	freeColumnIndexByRow 
		at: rowIndex 
		put: (freeColumnIndexByRow at: rowIndex) + 1