operations
rightButtonLabel
	"Answer the label for a button for scrolling right."

	^AlphaImageMorph new image: (
		ScrollBar
			arrowOfDirection: #right
			size: self controlButtonWidth - 3
			color: self paneColor darker)