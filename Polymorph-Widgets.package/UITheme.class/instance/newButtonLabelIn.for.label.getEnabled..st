morph creation
newButtonLabelIn: aThemedMorph for: aModel label: aString getEnabled: enabledSel
	"Answer a new button text label."

	^(self buttonLabelForText: aString)
		model: aModel;
		getEnabledSelector: enabledSel