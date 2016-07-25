build ui buttons
buildHierarchyButton
	^ (PluggableToggleButtonMorph
		on: self
		getState: #hierarchyButtonState
		action: #hierarchyButtonAction:
		label: #hierarchyButtonLabel
		icon: #hierarchyButtonIcon
		menu: nil)
		getEnabledSelector: #hierarchyButtonEnabled;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		enabled: self model selectedClass notNil;
		helpText: 'Show the class hierarchy';
		yourself