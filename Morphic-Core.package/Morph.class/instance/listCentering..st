layout-properties
listCentering: aSymbol
	"Layout specific. This property describes how the rows/columns in a list-like layout should be centered.
		#topLeft - center at start of primary direction
		#bottomRight - center at end of primary direction
		#center - center in the middle of primary direction
		#justified - insert extra space inbetween rows/columns
	"
	self assureTableProperties listCentering: aSymbol.
	self layoutChanged.