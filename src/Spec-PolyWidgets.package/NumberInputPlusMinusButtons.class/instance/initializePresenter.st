initialization
initializePresenter
	self plusButton
		action: [ self value: self value + (self valueToIncrement)  ].
	self minusButton
		action: [ self value: self value - (self valueToIncrement) ]