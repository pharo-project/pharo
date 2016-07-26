scrolling
preventOverscrolling

	self band hasSubbricks ifFalse: [ ^ self ].

	self isFirstAdded & self isFirstVisible ifTrue: [
			(self bandTopOffset > 0) ifTrue: [
				self moveAllSubbricksBy: self topOffset negated.
				self moveBandTop.
				^ self update ] ].
	self isTopOverscrolled
		ifTrue: [ self moveBandTop.  ^ self update ].
	self isBottomOverscrolled & (self isFullyVisible not)
		ifTrue: [ self moveBandBottom. "self moveAllSubbricksBy: (self band height - self band lastSubbrick brickBounds bottom)." ^ self update ]