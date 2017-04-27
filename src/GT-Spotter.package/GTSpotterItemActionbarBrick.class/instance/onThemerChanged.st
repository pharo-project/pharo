brick-properties-events
onThemerChanged
	super onThemerChanged.
	
	self themer spotterThemer itemActionbarStyleFor: self.
	self subbricks do: [ :aBrick | self themer spotterThemer itemActionButtonWidgetStyleFor: aBrick ]