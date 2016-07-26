controls
newLabelFor: aModel label: aString getEnabled: enabledSel
	"Answer a new text label."

	^self theme
		newLabelIn: self
		for: aModel
		label: aString
		getEnabled: enabledSel