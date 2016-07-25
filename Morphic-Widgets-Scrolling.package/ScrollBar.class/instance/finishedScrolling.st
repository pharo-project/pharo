access
finishedScrolling
	"Scrolling has finished (button or paging area)."
	
	|bcu bcd|
	bcu := upButton borderStyle baseColor.
	bcd := downButton borderStyle baseColor.
	self stopStepping.
	self scrollBarAction: nil.
	upButton borderRaised.
	upButton borderStyle baseColor: bcu.
	downButton borderRaised.
	downButton borderStyle baseColor: bcd.
	pagingArea
		fillStyle: self normalFillStyle;
		borderStyle: self normalBorderStyle