initialization
initialize
	super initialize.
	
	self searchField: self newSearchField.
	self class environment at: #GTEventRecorderSettings ifPresent: [
		self settingsButton: self newSettingsButton ].
	self helpButton: self newHelpButton.
	self closeButton: self newCloseButton.
	