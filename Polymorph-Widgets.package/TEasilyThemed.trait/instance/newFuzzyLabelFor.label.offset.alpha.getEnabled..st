controls
newFuzzyLabelFor: aModel label: aString offset: offset alpha: alpha getEnabled: enabledSel
	"Answer a new fuzzy label."

	^self theme
		newFuzzyLabelIn: self
		for: aModel
		label: aString
		offset: offset
		alpha: alpha
		getEnabled: enabledSel