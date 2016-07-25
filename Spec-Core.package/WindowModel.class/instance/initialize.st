initialization
initialize

	super initialize.

	model := nil asValueHolder.
	initialExtent := nil asValueHolder.
	title := self class title asValueHolder.
	isClosedHolder := false asValueHolder.
	isResizeable := true asValueHolder.
	
	model whenChangedDo: [:m | self updateTitle ].