initialization
beAllFont: aFont

	textStyle := TextStyle fontArray: (Array with: aFont).
	self releaseCachedState; changed