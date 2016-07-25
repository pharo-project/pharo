Morphic-Base-Widgets
listRenderOn: aCanvas atRow: aRow bounds: drawBounds color: drawColor backgroundColor: backgroundColor from: aMorph

	self bounds: drawBounds.
	self fullDrawOn: aCanvas.
	(aMorph submorphs includes: self)
		ifFalse: [ aMorph addMorph: self ]