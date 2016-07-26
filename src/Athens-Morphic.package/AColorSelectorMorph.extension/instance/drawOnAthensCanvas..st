*Athens-Morphic
drawOnAthensCanvas: anAthensCanvas
	anAthensCanvas setPaint: (InfiniteForm with: self hatchForm).
	anAthensCanvas drawShape: self innerBounds.
	super drawOnAthensCanvas: anAthensCanvas