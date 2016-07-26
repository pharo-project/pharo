testing
isFullyVisible
	self band hasSubbricks ifFalse: [ ^ true ].
	^ self isLastAdded & self isFirstAdded & super isFullyVisible