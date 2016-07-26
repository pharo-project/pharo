submorphs-add/remove
delete
	self hasFocus
		ifTrue: [ self currentHand newKeyboardFocus: nil ].
	super delete