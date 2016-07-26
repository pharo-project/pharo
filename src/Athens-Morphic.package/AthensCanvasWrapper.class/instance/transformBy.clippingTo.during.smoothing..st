canvas drawing-support
transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock smoothing: cellSize
	self canvas
		transformBy: aDisplayTransform
		withClip: aClipRect
		in: aClipRect
		during: [ :v | aBlock value: self ]