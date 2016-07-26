drawing-images
stencil: aForm at: aPoint sourceRect: aRect color: aColor
	myCanvas
		stencil: aForm
		at: aPoint
		sourceRect: aRect
		color: (self mapColor: aColor)