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
		enabled: self selectedClass notNil;
		helpText: 'List the instance and class variables of the selected class';
		yourself