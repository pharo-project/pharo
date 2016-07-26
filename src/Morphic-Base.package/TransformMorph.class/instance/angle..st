accessing
angle: newAngle

	self changed.
	transform := transform withAngle: newAngle.
	self layoutChanged.
	self changed