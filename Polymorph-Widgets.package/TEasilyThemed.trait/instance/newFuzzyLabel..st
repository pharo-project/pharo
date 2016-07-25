controls
newFuzzyLabel: aString
	"Answer a new fuzzy label."

	^self theme
		newFuzzyLabelIn: self
		for: nil
		label: aString
		offset: 1
		alpha: 0.5
		getEnabled: nil