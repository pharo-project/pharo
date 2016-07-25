accessing
setOffset: newOffset angle: newAngle scale: newScale

	transform := MorphicTransform offset: newOffset angle: newAngle scale: newScale.
	self changed