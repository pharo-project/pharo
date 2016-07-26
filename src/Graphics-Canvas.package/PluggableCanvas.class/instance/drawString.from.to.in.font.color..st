drawing-text
drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c
	self apply: [ :clippedCanvas |
		clippedCanvas drawString: s from: firstIndex to: lastIndex in: boundsRect font: fontOrNil color: c]