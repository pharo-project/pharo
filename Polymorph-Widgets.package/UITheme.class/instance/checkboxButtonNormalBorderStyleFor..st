border-styles-buttons
checkboxButtonNormalBorderStyleFor: aCheckboxButton
	"Return the normal checkbox button borderStyle for the given button."

	^BorderStyle simple
		width: 1;
		baseColor: aCheckboxButton paneColor twiceDarker