row management
selectRow: index
	"Select the index-th row."
	
	selectedRows add: index.
	self invalidRect: (self selectionFrameForRow: index)