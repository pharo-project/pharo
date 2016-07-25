layout-properties
wrapCentering: aSymbol
	"Layout specific. This property describes how the rows/columns in a list-like layout should be centered.
		#topLeft - center at start of secondary direction
		#bottomRight - center at end of secondary direction
		#center - center in the middle of secondary direction
		#justified - insert extra space inbetween rows/columns
	"
	self assureTableProperties wrapCentering: aSymbol.
	self layoutChanged.