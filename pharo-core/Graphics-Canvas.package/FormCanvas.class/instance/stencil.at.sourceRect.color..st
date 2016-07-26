drawing-images
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	self setPaintColor: aColor.
	port colorMap: stencilForm maskingMap.
	port stencil: stencilForm
		at: aPoint + origin
		sourceRect: sourceRect.