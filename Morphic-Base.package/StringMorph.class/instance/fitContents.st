accessing
fitContents

	| newBounds boundsChanged |
	newBounds := self measureContents.
	boundsChanged := bounds extent ~= newBounds.
	self extent: newBounds.		"default short-circuits if bounds not changed"
	boundsChanged ifFalse: [self changed]