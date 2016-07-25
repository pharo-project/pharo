updating
changed
	self owner ifNil: [ ^ self ].
	self setNeedsRefreshExposedRows.
	super changed