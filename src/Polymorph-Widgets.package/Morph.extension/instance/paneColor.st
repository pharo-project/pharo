*Polymorph-Widgets
paneColor
	"Answer the window's pane color or our color otherwise."

	^self paneColorOrNil ifNil: [self color]