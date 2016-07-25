drawing-text
drawString: s in: boundsRect font: fontOrNil color: c
	^self drawString: s from: 1 to: s size in: boundsRect font: fontOrNil color: c