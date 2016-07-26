event handling
keyboardFocusChange: aBoolean
	aBoolean
		ifTrue: [
			self hasFocus: true.
			self showOverEditableTextCursor ]
		ifFalse: [ 
			self hasFocus: false.
			self hideOverEditableTextCursor ].
	super keyboardFocusChange: aBoolean.
	self focusChanged.
