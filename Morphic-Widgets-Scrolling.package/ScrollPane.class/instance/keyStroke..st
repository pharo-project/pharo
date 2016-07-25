event handling
keyStroke: evt
	"If pane is not empty, pass the event to the last submorph,
	assuming it is the most appropriate recipient (!)"

	(self scrollByKeyboard: evt) ifTrue: [^self].
	scroller submorphs last keyStroke: evt