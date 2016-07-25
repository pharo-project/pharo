placement
placeRight

	placement := [ :layoutBounds | 
		self bounds: (layoutBounds right - self width @ layoutBounds top corner: layoutBounds corner).
		layoutBounds origin corner: bounds origin x@layoutBounds bottom 
		].
