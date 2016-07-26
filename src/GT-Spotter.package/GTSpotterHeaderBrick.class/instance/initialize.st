initialization
initialize
	super initialize.
	
	self searchField: self newSearchField.
	#GTEventRecorderSettings asClassIfPresent: [ 
		self settingsButton: self newSettingsButton ].
	self helpButton: self newHelpButton.
	self closeButton: self newCloseButton.
	