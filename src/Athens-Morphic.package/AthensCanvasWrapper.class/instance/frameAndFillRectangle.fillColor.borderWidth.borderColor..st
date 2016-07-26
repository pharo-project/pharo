canvas drawing-rectangles
frameAndFillRectangle: aRectangle fillColor: aFillColor borderWidth: aBorderWidth borderColor: aBorderColor
	self
		canvasClipBy: self clipRect
		during: [ 
			self canvas setPaint: aFillColor.
			self canvas drawShape: aRectangle.
			(self canvas setStrokePaint: aBorderColor) width: aBorderWidth.
			self canvas drawShape: aRectangle ]