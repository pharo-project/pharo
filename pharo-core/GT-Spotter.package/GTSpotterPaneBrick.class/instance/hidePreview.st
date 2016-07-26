action
hidePreview

	self isPreviewShown ifFalse: [ ^ self ].

	self removeBrick: self previewBrick.
	self contentsBrick layout hSpaceFill.
	self changed.