build ui buttons
buildInstanceButton
	^ (PluggableToggleButtonMorph
		on: self
		getState: #instanceButtonState
		action: #instanceButtonAction:
		label: #instanceButtonLabel
		icon: #instanceButtonIcon
		menu: nil)
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		helpText: (self instanceButtonHelpTextFor: self instanceButtonState);
		yourself