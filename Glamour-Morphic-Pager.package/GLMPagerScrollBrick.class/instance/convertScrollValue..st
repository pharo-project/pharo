scrolling
convertScrollValue: aFloat
	self pagerModel actualNumberOfVisiblePages = 0 ifTrue: [ ^ 0 ].
	^ (self width - ((self width / self pagerModel actualNumberOfVisiblePages + self paneMargin) * self band subbricksCount)) * aFloat