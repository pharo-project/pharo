canvas drawing-rectangles
fillRectangle: aRectangle fillStyle: aFillStyle borderStyle: aBorderStyle
	self
		canvasClipBy: self clipRect
		during: [ 
			self canvas setPaint: aFillStyle.
			self canvas drawShape: (aRectangle insetBy: aBorderStyle width).
			self canvas setStrokePaint: aBorderStyle.
			self canvas drawShape: (aRectangle insetBy: aBorderStyle width / 2 asFloat) ]