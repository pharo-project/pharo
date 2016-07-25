label-brick-actions
beNormal
	
	self isReadMode ifTrue: [ ^ self ].
	self animateToNormal.
	self removeBrick: self editBrick.
	self activeHand releaseKeyboardFocus.
	self addBrickBack: self labelBrick.
	self announce: #onReadMode