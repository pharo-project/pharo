testing
handlesKeyboard: evt
	^(self handlesKeyDown: evt)
		or: [ (self handlesKeyUp: evt)
			or: [ self handlesKeyStroke: evt ]].