drawing-support
clipBy: newClipRect during: aBlock
	self apply: [ :c |
		c clipBy: newClipRect during: aBlock ]