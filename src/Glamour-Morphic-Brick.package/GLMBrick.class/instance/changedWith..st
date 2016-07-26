morph-drawing
changedWith: aBlock

	self changed.
	aBlock cull: self.
	self changed.