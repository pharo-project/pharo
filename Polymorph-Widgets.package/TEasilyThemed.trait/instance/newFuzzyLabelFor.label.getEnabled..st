controls
newFuzzyLabelFor: aModel label: aString getEnabled: enabledSel
	"Answer a new fuzzy label."

	^self theme
		newFuzzyLabelIn: self
		for: aModel
		label: aString
		offset: 1
		alpha: 0.5
		getEnabled: enabledSel