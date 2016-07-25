brick-morph-mouse-events
mouseUp: evt
	super mouseUp: evt.
	(self bounds containsPoint: evt position) & (self isPressed) ifTrue: [
		self depress.
		^ self click: evt
	] ifFalse: [
		self deselect: evt.
		^ self depress ]