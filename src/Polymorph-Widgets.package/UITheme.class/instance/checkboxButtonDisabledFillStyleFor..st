fill-styles-buttons
checkboxButtonDisabledFillStyleFor: aCheckboxButton
	"Return the disabled checkbox button fillStyle for the given button."
	
	^aCheckboxButton paneColor
		alphaMixed: 0.3
		with: Color white