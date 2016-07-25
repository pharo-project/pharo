accessing
offset: newOffset

	transform := transform withOffset: newOffset - self innerBounds topLeft.
	self changed