geometry
extent: newExtent
	bounds extent = newExtent ifTrue: [^ self].
	super extent: newExtent.
	self setScrollDeltas 