actions
toggleHints

	self visible ifFalse: [ ^ self ].
	self stepModel spotter isShiftPressed
		ifTrue: [ self onRevealHints ]
		ifFalse: [ self onHideHints ]