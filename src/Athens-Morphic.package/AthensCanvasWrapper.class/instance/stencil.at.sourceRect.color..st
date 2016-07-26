canvas drawing
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	|mask|
	mask := stencilForm asFormOfDepth: 32.
	mask mapColor:Color white to: Color transparent.
	mask:= mask asAthensPaintOn: self canvas.
	self canvas pathTransform restoreAfter:[
		self canvas pathTransform translateBy: aPoint.
		self canvas setPaint: aColor.
		self canvas draw.
		mask maskOn: self canvas.
		].
