event handling
mouseUp: evt

	(self containsPoint: evt cursorPoint)
		ifTrue: [  "toggle and do action"
			self setSwitchState: (oldColor = offColor).
			self doButtonAction]
		ifFalse: [  "restore old appearance"
			self setSwitchState: (oldColor = onColor)].
