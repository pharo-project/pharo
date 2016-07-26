initialization
initializeShortcuts

	self
		bindKeyCombination: Character arrowLeft alt command
		toAction: [ self pagerModel selectToLeft ].
	
	self
		bindKeyCombination: Character arrowRight alt command
		toAction: [ self pagerModel selectToRight ].
	
	self
		bindKeyCombination: Character arrowLeft alt shift command
		toAction: [ self pressedScrollerSizeCombination: #left ].

	self
		bindKeyCombination: Character arrowRight alt shift command
		toAction: [ self pressedScrollerSizeCombination: #right ].