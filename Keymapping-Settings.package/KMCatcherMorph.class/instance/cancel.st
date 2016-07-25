api
cancel
	edited
		ifFalse: [ ^ self ].
	self keystrokes: OrderedCollection new.
	self showInitialShortcut.
	self initializeKeystrokes