build ui buttons
buildCommentButton
	^ (PluggableToggleButtonMorph
		on: self
		getState: #commentButtonState
		action: #commentButtonAction:
		label: #commentButtonLabel
		icon: #commentButtonIcon
		menu: nil)
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		helpText: 'Show the class comments';
		yourself