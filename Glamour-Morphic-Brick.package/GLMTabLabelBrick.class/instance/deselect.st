brick-interactable-actions
deselect
	self owner ifNil: [ ^ self ].
	self layoutProperties minWidth: 0.
	self owner doLayoutForce