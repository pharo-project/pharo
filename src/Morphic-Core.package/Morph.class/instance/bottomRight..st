geometry
bottomRight: aPoint
	" Move me so that my bottom right corner is at aPoint. My extent (width & height) are unchanged "

	self position: ((aPoint x - bounds width) @ (aPoint y - self height))
