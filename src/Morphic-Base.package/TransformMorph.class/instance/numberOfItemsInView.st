geometry
numberOfItemsInView
	"Answer the number of my submorphs whose (transformed) bounds intersect mine.
	This includes items that are only partially visible.
	Ignore visibility of submorphs."

	^(submorphs select: [ :ea | self innerBounds intersects: (transform localBoundsToGlobal: ea bounds) ]) size