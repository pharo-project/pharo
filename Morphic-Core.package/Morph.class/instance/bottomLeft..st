geometry
bottomLeft: aPoint
	" Move me so that my bottom left corner is at aPoint. My extent (width & height) are unchanged "

	self position: ((aPoint x) @ (aPoint y - self height)).
