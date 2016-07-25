scrollpane-testing
isFirstVisible

	self band hasSubbricks ifFalse: [ ^ true ].
	^ self isVisible: self band firstSubbrick