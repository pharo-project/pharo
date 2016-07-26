drawing-text
drawString: s in: boundsRect
	^self drawString: s from: 1 to: s size in: boundsRect font: nil color: Color black