opening
openBelowFor: aBrick

	self anchorBrick: aBrick.
	self triggerBrick: self newTriggerBrick.
	self contentBrick: self newContentBrick.
	self triggerBrick zIndex: 2.
	self triggerBrick
		borderDo: [ :b | b widthBottom: 0 ];
		marginBottom: self themer popupThemer borderWidth negated.
	self addContentBricks.
	
	^ aBrick ifNotNil: [ self openBelow: aBrick boundsInWorld ]