geometry
bottom: aNumber
	" Move me so that my bottom is at the y-coordinate aNumber. My extent (width & height) are unchanged "

	self position: (bounds left @ (aNumber - self height))