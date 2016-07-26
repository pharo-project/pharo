brick-interactable-actions
depress
	self isPressed ifTrue: [
		isPressed := false.
		self changed.
		^ true ].
	^ false