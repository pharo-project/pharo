drawing-images
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	self apply: [ :c |
		c stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor ]