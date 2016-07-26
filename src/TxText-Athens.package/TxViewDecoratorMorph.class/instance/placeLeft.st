placement
placeLeft

	placement := [ :layoutBounds | 
		self bounds: (layoutBounds origin extent: self width @ layoutBounds height).
		
		layoutBounds origin x+self width @ layoutBounds origin y corner: layoutBounds corner
	].
