placement
placeTop

	placement := [ :layoutBounds | 
		self bounds: (layoutBounds topLeft extent: layoutBounds width @ self height).
		layoutBounds origin x@ (layoutBounds origin y + self height) corner: layoutBounds corner
		].
	 