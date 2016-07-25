row management
unselectRow: index
	"Unselect the index-th row."
	
	selectedRows remove: index ifAbsent: [^self].
	self invalidRect: (self selectionFrameForRow: index)