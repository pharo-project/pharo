initialize
initialize

	super initialize.
	window := self buildWindow.
	shoutMorph := self buildText.
	window addMorph: shoutMorph frame: (0 @ 0 corner: 1 @ 1).