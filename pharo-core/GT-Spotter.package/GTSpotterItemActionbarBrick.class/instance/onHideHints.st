event handling
onHideHints

	self visible ifFalse: [ ^ self ].
	self themer spotterThemer hintHideStyleFor: self diveInButton.
	self themer spotterThemer hintHideStyleFor: self actOnButton.