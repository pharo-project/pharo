panes
paneColorToUse
	^ Display depth <= 2
		ifTrue:
			[Color white]
		ifFalse:
			[self paneColor]