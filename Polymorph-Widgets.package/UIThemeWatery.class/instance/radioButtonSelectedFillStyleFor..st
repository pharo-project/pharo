fill-styles-buttons
radioButtonSelectedFillStyleFor: aRadioButton
	"Return the selected radio button fillStyle for the given button."
	
	^(ImageFillStyle form: self radioButtonSelectedForm) origin: aRadioButton topLeft