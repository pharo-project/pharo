widget API
action

	self widget ifNotNil: [:m | m takeKeyboardFocus ].
	self model performAction.