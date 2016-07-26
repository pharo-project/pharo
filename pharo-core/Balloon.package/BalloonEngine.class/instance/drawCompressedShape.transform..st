drawing
drawCompressedShape: shape transform: aTransform
	| fillIndexList |
	self edgeTransform: aTransform.
	self resetIfNeeded.

	fillIndexList := self registerFills: shape fillStyles.

	self primAddCompressedShape: shape points
		segments: shape numSegments
		leftFills: shape leftFills
		rightFills: shape rightFills
		lineWidths: shape lineWidths
		lineFills: shape lineFills
		fillIndexList: fillIndexList.
	self postFlushIfNeeded.