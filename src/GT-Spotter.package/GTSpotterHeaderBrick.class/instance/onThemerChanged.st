brick-properties-events
onThemerChanged
	super onThemerChanged.
	
	self themer spotterThemer headerWidgetStyleFor: self.
	self themer spotterThemer closeButtonWidgetStyleFor: self closeButton.
	self themer spotterThemer searchFieldWidgetStyleFor: self searchField.
	self themer spotterThemer settingsButtonWidgetStyleFor: self settingsButton