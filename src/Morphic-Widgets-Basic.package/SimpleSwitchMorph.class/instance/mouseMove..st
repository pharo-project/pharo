event handling
mouseMove: evt

	(self containsPoint: evt cursorPoint)
		ifTrue: [self setSwitchState: (oldColor = offColor)]
		ifFalse: [self setSwitchState: (oldColor = onColor)].
