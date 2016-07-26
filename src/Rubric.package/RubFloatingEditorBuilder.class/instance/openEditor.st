public editing
openEditor
	self buildEditor.
	editor ifNil: [ ^ self ].
	editor
		openInWorld;
		comeToFront.
	editor textArea takeKeyboardFocus