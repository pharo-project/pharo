*Rubric-Editing-Core
rubParagraph: para bounds: bounds color: c

	| scanner |
	self setPaintColor: c.
	scanner := (port clippedBy: (bounds translateBy: origin)) rubDisplayScannerFor: para
		foreground: c background: Color transparent
		ignoreColorChanges: false.
	para drawOn: (self copyClipRect: bounds) using: scanner at: origin + bounds topLeft.
