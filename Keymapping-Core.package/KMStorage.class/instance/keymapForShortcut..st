accessing
keymapForShortcut: aShortcut

	^ namedRegistry
		detect: [ :keymap | keymap shortcut = aShortcut ]
		ifNone: [ 
			anonymousRegistry
				detect: [ :keymap | keymap shortcut = aShortcut ]
				ifNone: [ nil ] ]