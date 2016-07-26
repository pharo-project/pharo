building
newButtonLabel: direction ofSize: size
	"Answer a new label for an inc/dec button."

	^AlphaImageMorph new
		image: (ScrollBar
				arrowOfDirection: direction
				size: size
				color: self paneColor darker)