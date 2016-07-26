fill-styles-buttons
checkboxButtonNormalFillStyleFor: aCheckboxButton
	"Return the normal checkbox button fillStyle for the given checkbox button."
	
	^(ImageFillStyle form: self checkboxForm) origin: aCheckboxButton topLeft