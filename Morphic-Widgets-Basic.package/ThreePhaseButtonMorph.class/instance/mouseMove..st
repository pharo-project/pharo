event handling
mouseMove: evt
	(self containsPoint: evt cursorPoint)
		ifTrue: [self state: #pressed.
				super mouseMove: evt]
				"Allow on:send:to: to set the response to events other than actWhen"
		ifFalse: [self state: #off].
