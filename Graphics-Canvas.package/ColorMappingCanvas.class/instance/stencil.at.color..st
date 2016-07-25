drawing-images
stencil: aForm at: aPoint color: aColor
	myCanvas
		stencil: aForm
		at: aPoint
		color: (self mapColor: aColor)