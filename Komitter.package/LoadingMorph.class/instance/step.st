stepping
step

	self isStepping ifFalse: [ ^ self ].
	currentImageIndex := ((currentImageIndex + 1) \\ 3)+1.
	self changed