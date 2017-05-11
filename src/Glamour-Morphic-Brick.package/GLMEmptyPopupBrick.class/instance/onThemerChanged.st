popup - hooks
onThemerChanged
	| aThemer |
	super onThemerChanged.
	
	aThemer := self themer popupThemer.

	self color: aThemer backgroundColor.
	self borderWidth: aThemer borderWidth.
	self borderColor: aThemer borderColor.

	self inset: aThemer inset.
	self spacing: aThemer spacing.
	
	self shadowWidth: aThemer shadowWidth.
	self shadowColor: aThemer shadowColor.
	self shadowOffset: aThemer shadowOffset.
	self shadowInset: aThemer shadowInset.

	self contentBrick padding: aThemer contentPadding.
	self contentBrick color: aThemer contentBackgroundColor.
	self updateTriggerBrick