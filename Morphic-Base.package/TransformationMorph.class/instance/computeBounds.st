geometry
computeBounds
	self hasSubmorphs ifTrue:
		[bounds := (transform localBoundsToGlobal:
					(Rectangle merging:
						(self submorphs collect: [:m | m fullBounds]))) truncated
				expandBy: 1].
	fullBounds := bounds.