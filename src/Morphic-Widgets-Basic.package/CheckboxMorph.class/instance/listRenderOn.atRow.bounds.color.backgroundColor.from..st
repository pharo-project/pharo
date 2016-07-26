drawing
listRenderOn: aCanvas atRow: aRow bounds: drawBounds color: drawColor backgroundColor: backgroundColor from: aMorph

	self color: backgroundColor.
	self bounds: drawBounds.
	self fullDrawOn: aCanvas.
	aMorph addMorph: self