morph creation
newLabelIn: aThemedMorph for: aModel getLabel: labelSel getEnabled: enabledSel
	"Answer a new text label."

	^(LabelMorph contents: '' font: self labelFont)
		model: aModel;
		getEnabledSelector: enabledSel;
		getTextSelector: labelSel