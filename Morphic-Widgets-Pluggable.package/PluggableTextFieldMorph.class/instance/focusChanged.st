focus
focusChanged
	(self hasFocus or: [self chooserHasFocus])
		ifFalse: [self closeChooser].
	super focusChanged
