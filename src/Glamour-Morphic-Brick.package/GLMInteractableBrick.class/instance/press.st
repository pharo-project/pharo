brick-interactable-actions
press
	self isPressed ifFalse: [
		isPressed := true.
		self changed.
	].

	^ false