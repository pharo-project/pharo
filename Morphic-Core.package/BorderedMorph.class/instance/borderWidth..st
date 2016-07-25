accessing
borderWidth: anInteger
	borderColor ifNil: [borderColor := Color black].
	borderWidth := anInteger max: 0.
	self changed