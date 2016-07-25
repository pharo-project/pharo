drawing
line: pt1 to: pt2 width: w color: c
	self apply: [ :clippedCanvas |
		clippedCanvas line: pt1 to: pt2 width: w color: c ]