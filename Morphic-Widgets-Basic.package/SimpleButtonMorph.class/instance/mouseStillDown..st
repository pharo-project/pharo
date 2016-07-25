event handling
mouseStillDown: evt
	actWhen == #whilePressed ifFalse:[^self].
	(self containsPoint: evt cursorPoint) ifTrue:[self doButtonAction].