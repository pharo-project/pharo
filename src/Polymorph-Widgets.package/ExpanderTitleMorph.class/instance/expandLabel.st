operations
expandLabel
	"Answer the label for the expand button."

	^AlphaImageMorph new image: (
		ScrollBar
			arrowOfDirection: (self expanded ifTrue: [#top] ifFalse: [#bottom])
			size: self buttonWidth - 3
			color: self paneColor darker)