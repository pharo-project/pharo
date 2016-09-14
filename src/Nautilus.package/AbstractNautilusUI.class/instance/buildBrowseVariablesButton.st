build ui buttons
buildBrowseVariablesButton
	^ (PluggableButtonMorph
		on: self
		getState: #browseVariablesButtonState
		action: #browseVariablesButtonAction
		label: #browseVariablesButtonLabel)
		getEnabledSelector: #browseVariablesButtonEnabled;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		enabled: (self selectedClass notNil and:[ self selectedClass isTrait not]);
		helpText: 'List the instance and class variables of the selected class';
		yourself