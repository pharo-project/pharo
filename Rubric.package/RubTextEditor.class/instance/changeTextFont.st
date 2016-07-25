menu messages
changeTextFont
	"Present a menu of available fonts, and if one is chosen, apply it to the current selection."

	textArea
		forceCursorVisibleWhile: [ 
			| hadFocus |
			hadFocus := textArea hasFocus.
			textArea
				openModal:
					(FontChooser
						openWithWindowTitle: 'Choose a Font'
						for: self
						setSelector: #applyTextFont:
						getSelector: self fontToUse).
			hadFocus
				ifTrue: [ textArea takeKeyboardFocus ] ]