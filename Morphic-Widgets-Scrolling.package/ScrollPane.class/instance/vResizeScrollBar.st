geometry
vResizeScrollBar
	| w topLeft  |
	w := self scrollBarThickness.
	topLeft := bounds topRight - ((w + borderWidth - 0) @ (0 - borderWidth)).
	scrollBar bounds: (topLeft extent: w @ self vScrollBarHeight)