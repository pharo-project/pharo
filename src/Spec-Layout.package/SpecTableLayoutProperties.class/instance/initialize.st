initialization
initialize
	super initialize.
	cellSpacing := listSpacing := wrapDirection := #none.
	cellPositioning := #center.
	listCentering := wrapCentering := #topLeft.
	listDirection := #topToBottom.
	reverseTableCells := rubberBandCells := false.
	layoutInset := cellInset := minCellSize := 0.
	maxCellSize := 1073741823. "SmallInteger maxVal"
