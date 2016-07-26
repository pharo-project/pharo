scrollpane-testing
isLastVisible

	self band hasSubbricks ifFalse: [ ^ true ].
	^ self isVisible: self band lastSubbrick