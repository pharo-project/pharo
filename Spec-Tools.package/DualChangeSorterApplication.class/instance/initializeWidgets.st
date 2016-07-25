initialization
initializeWidgets

	changeSorterLeft := self instantiate: ChangeSorterApplication.
	changeSorterRight := self instantiate: ChangeSorterApplication.
		
	self menusRegistration.
	self shortcutsRegistration.
	self setFocusOrder