layout-properties
maxCellSize: aPoint
	"Layout specific. This property specifies the maximum size of a table cell."
	self assureTableProperties maxCellSize: aPoint.
	self layoutChanged.