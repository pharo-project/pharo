basic-completion
selectorChooserFocusChange
	textArea hasFocus
		ifTrue: [ selectorChooser ifNotNil: [ selectorChooser comeToFront ] ]