layout
resizeScrollBar
	| inner outer |
	outer := super innerBounds.
	inner := outer withHeight: outer height - self scrollBarHeight.
	scrollBar bounds: ((inner left @ inner bottom) corner: outer bottomRight)