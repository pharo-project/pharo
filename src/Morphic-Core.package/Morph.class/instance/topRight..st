geometry
topRight: aPoint
	" Move me so that my top right corner is at aPoint. My extent (width & height) are unchanged "

	self position: ((aPoint x - bounds width) @ (aPoint y))
