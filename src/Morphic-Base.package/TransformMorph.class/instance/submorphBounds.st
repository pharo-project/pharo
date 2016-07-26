layout
submorphBounds
	"Answer, in owner coordinates, the bounds of my visible submorphs, or my bounds"
	^ (self localVisibleSubmorphBounds ifNotNil: [ :box |transform localBoundsToGlobal: box ] ifNil: [ self bounds ]) truncated.
