layout-properties
rubberBandCells: aBool
	"Layout specific. This property describes if a parent that is #shrinkWrapped around its children should ignore any #spaceFill children. E.g., when #rubberBandCells is true, the compound layout will always stay at the smallest available size, even though some child may be able to grow."
	self assureTableProperties rubberBandCells: aBool.
	self layoutChanged.