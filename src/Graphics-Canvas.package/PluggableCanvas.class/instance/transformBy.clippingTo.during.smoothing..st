drawing-support
transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize

	self apply: [ :clippedCanvas |
		clippedCanvas transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize ]