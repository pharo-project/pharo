copying
blankCopyOf: aRectangle scaledBy: scale
	^Form extent: (aRectangle extent * scale) truncated depth: 32