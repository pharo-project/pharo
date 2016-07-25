layout-properties
cellSpacing: aSymbol
	"Layout specific. This property describes how the cell size for each element in a list should be computed.
		#globalRect - globally equal rectangular cells
		#globalSquare - globally equal square cells
		#localRect - locally (e.g., per row/column) equal rectangular cells
		#localSquare - locally (e.g., per row/column) equal square cells
		#none - cells are sized based on available row/column constraints
	"
	self assureTableProperties cellSpacing: aSymbol.
	self layoutChanged.