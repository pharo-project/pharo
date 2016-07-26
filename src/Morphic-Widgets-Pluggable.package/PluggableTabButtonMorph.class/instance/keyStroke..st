event
keyStroke: event

	(self navigationKey: event) ifTrue: [^self].
	super keyStroke: event