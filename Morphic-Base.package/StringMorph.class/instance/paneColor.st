accessing
paneColor
	"Answer the window's pane color or our owner's color otherwise."

	^self paneColorOrNil ifNil: [self owner ifNil: [Color transparent] ifNotNil: [self owner color]]