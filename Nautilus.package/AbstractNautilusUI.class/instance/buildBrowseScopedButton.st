build ui buttons
buildBrowseScopedButton
	^ (PluggableButtonMorph
		on: self
		getState: #browseScopedButtonState
		action: #browseScopedButtonAction
		label: #browseScopedButtonLabel)
		getEnabledSelector: #browseScopedButtonEnabled;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		enabled: self selectedPackage notNil;
		helpText: 'Browse scoped the selected packages';
		yourself