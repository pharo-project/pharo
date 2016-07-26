layout-properties
listSpacing: aSymbol
	"Layout specific. This property describes how the heights for different rows in a table layout should be handled.
		#equal - all rows have the same height
		#none - all rows may have different heights
	"
	self assureTableProperties listSpacing: aSymbol.
	self layoutChanged.