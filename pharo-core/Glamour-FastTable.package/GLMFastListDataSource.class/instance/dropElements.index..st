drag and drop
dropElements: aCollection index: rowIndex
	self glamourPresentation 
		accept: (self collectionToDragPassenger: aCollection)
		droppedOnItem: (self elementOrZeroAt: rowIndex)