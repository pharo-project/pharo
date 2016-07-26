initialization
initialize
	super initialize.
	self enableDragNDrop: true.
	self fillStyle: (self theme morphTreeColumnFillStyleFor: self).
	self borderStyle: (self theme morphTreeColumnBorderStyleFor: self)
