drawing
paragraph: para bounds: bounds color: c

	| scanner |
	self setPaintColor: c.
	scanner := (port clippedBy: (bounds translateBy: origin)) displayScannerFor: para
		foreground: c background: Color transparent
		ignoreColorChanges: false.
	para displayOn: (self copyClipRect: bounds) using: scanner at: origin+ bounds topLeft.
