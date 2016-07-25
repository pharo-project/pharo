brick-properties-events
onThemerChanged
	super onThemerChanged.
	
	self subbricks do: [ :each | self themer spotterThemer itemActionButtonWidgetStyleFor: each ]