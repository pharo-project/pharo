as yet unclassified
selectRow: index
	"select the index-th row"
	selectedRows add: index.
	self invalidRect: (self drawBoundsForRow: index)