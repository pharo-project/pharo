column handling
swapColumnAt: oneIndex withColumnAt: anotherIndex
	"swap two column - very rough implementation which 
	simply rebuild everything"
	self columns swap: oneIndex with: anotherIndex. 
	self columnsChanged.
	self updateList.
	self columnResizersToFront
