border-styles-buttons
checkboxButtonDisabledBorderStyleFor: aCheckboxButton
	"Return the disabled checkbox button borderStyle for the given button."

	^BorderStyle simple
		width: 1;
		baseColor: aCheckboxButton paneColor darker