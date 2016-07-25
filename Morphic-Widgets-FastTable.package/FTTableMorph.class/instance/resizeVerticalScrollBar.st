private
resizeVerticalScrollBar
	| width height corner |
	width := self scrollBarThickness.
	height := self bounds height - (self borderWidth * 2).
	corner := self bounds topRight - ((width + self borderWidth)@(0 - self borderWidth)).
	self verticalScrollBar bounds: (corner extent: width@height)