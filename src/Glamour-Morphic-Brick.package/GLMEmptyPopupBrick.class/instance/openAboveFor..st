opening
openAboveFor: aBrick

	self anchorBrick: aBrick.
	self contentBrick: self newContentBrick.
	self triggerBrick: self newTriggerBrick.
	self triggerBrick
		borderDo: [ :b | b widthTop: 0 ];
		marginTop: self themer popupThemer borderWidth negated.
	self addContentBricks.
	
	^ aBrick ifNotNil: [ self openAbove: aBrick boundsInWorld ]