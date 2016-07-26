actions
toggleHints

	(self hasSubbrick: self actionbarBrick) ifFalse: [ ^ self ].
	self stepModel spotter isShiftPressed
		ifTrue: [ self onRevealHints ]
		ifFalse: [ self onHideHints ]