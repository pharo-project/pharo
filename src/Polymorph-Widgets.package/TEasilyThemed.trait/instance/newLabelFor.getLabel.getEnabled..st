controls
newLabelFor: aModel getLabel: labelSel getEnabled: enabledSel
	"Answer a new text label."

	^self theme
		newLabelIn: self
		for: aModel
		getLabel: labelSel
		getEnabled: enabledSel