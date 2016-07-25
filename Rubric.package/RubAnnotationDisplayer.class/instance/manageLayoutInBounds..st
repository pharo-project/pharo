geometry
manageLayoutInBounds: aRectangle
	| ret |
	ret := super manageLayoutInBounds: aRectangle.
	self updateContents.
	^ ret