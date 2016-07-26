operations
leftButtonLabel
	"Answer the label for a button for scrolling left."

	^AlphaImageMorph new image: (
		ScrollBar
			arrowOfDirection: #left
			size: self controlButtonWidth - 3
			color: self paneColor darker)