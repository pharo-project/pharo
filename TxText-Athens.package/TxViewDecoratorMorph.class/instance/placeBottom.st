placement
placeBottom

	placement := [ :layoutBounds | 
		self bounds: (layoutBounds left @ (layoutBounds bottom - self height) corner: layoutBounds corner).
		
		layoutBounds origin corner: layoutBounds right @ bounds top.
	].
