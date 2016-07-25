accessing
scale: newScale

	self changed.
	transform := transform withScale: newScale.
	self layoutChanged.
	self changed.
