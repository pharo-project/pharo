controls
newButtonLabelFor: aModel label: aString getEnabled: enabledSel
	"Answer a new button text label."

	^self theme
		newButtonLabelIn: self
		for: aModel
		label: aString
		getEnabled: enabledSel