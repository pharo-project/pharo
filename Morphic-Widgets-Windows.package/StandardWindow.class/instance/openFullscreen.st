open/close
openFullscreen
	"Open the receiver in a FullscreenMorph."

	^FullscreenMorph new
		setContentMorph: self;
		openInWorld