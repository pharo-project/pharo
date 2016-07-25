morph creation
newFuzzyLabelIn: aThemedMorph for: aModel label: aString offset: offset alpha: alpha getEnabled: enabledSel
	"Answer a new fuzzy label."

	^(FuzzyLabelMorph contents: aString font: self labelFont)
		offset: offset;
		alpha: alpha;
		model: aModel;
		getEnabledSelector: enabledSel	