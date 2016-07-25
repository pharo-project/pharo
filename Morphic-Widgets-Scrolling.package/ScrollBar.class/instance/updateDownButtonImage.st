initialize
updateDownButtonImage
	"update the receiver's downButton.  put a new image inside"
	downButton removeAllMorphs.
	downButton
		addMorphCentered: (ImageMorph new form: self downImage)