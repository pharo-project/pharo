fill-styles-buttons
radioButtonNormalFillStyleFor: aRadioButton
	"Return the normal radio button fillStyle for the given button."
	
	^(ImageFillStyle form: self radioButtonForm) origin: aRadioButton topLeft