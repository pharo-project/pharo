brick-properties-events
onThemerChanged
	super onThemerChanged.
	
	self themer spotterThemer categoryTitleWidgetStyleFor: self.
	self themer spotterThemer categoryAmountWidgetStyleFor: self amountBrick.
	self themer spotterThemer categoryTitleNameWidgetStyleFor: self nameBrick.