fill-styles-buttons
checkboxButtonSelectedFillStyleFor: aCheckboxButton
	"Return the selected checkbox button fillStyle for the given checkbox button."
	
	^(ImageFillStyle form: self checkboxSelectedForm) origin: aCheckboxButton topLeft