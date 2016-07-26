drawing
drawOn: aCanvas

	self render.

	aCanvas
		drawImage: surface asForm at: self bounds origin