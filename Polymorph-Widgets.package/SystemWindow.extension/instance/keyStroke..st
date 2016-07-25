*Polymorph-Widgets
keyStroke: evt
	"Check for close window."
	super keyStroke: evt.
	(self navigationKey: evt) ifTrue: [^true].
	^false