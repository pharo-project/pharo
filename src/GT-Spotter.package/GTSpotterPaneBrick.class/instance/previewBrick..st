accessing
previewBrick: aBrick

	previewBrick ifNotNil: [self removeBrick: aBrick].
	previewBrick := aBrick.
	
	self themer spotterThemer spotterPreviewStyleFor: previewBrick