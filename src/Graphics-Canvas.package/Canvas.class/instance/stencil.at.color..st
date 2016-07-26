drawing-images
stencil: stencilForm at: aPoint color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	^self stencil: stencilForm
		at: aPoint
		sourceRect: stencilForm boundingBox
		color: aColor