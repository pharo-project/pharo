as yet unclassified
moreButtonLabel
	"Answer the label for the more button."

	^AlphaImageMorph new image: (
		ScrollBar
			arrowOfDirection: #right
			size: self buttonWidth
			color: self paneColor darker)