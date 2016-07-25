geometry
right: aNumber
	" Move me so that my right side is at the x-coordinate aNumber. My extent (width & height) are unchanged "

	self position: ((aNumber - bounds width) @ bounds top)