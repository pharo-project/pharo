activation
navigateFocus

	self rememberedKeyboardFocus
		ifNil: [self navigateFocusForward]
		ifNotNil: [ :keyboardFocus |
			keyboardFocus world
				"ifNil: [self rememberKeyboardFocus: nil]" "deleted"
				ifNotNil: [
					keyboardFocus wantsKeyboardFocus
						ifTrue: [keyboardFocus takeKeyboardFocus]
						ifFalse: [self navigateFocusForward]
					]
		].