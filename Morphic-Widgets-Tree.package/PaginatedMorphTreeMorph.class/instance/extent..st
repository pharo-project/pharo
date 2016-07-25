geometry
extent: newExtent
	self extent = newExtent ifTrue: [^ self].
	super extent: newExtent.
	self updatePager.
