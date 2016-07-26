morph creation
newLabelIn: aThemedMorph label: aString
	"Answer a new text label."

	^self
		newLabelIn: aThemedMorph
		for: nil
		label: aString
		getEnabled: nil