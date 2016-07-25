accessing
selectedColor
	"Answer the value of selectedColor"

	^selectedColor ifNil: [self color]