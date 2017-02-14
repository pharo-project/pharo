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
		helpText: 'Browse the selected packaged in a scoped context: analysis tools such as ''Senders of...'', ''Implementors of...'', ''Class refs...'', etc. are limited to the package';
		yourself