*Athens-Morphic
morphicDrawString: string at: position font: aFont color: aColor

	self pathTransform restoreAfter: [
		self pathTransform translateX: position x Y: position y + aFont getPreciseAscent.
		self 
			setFont: aFont;
			setPaint: aColor;
			drawString: string.
	]
