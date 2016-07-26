event handling
mouseMove: evt
	actWhen == #buttonDown ifTrue: [^ self].
	self updateVisualState: evt.