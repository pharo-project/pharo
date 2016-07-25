morph creation
newLabelIn: aThemedMorph for: aModel label: aString getEnabled: enabledSel
	"Answer a new text label."

	^(LabelMorph contents: aString font: self labelFont)
		model: aModel;
		getEnabledSelector: enabledSel