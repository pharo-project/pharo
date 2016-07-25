morph creation
newTitleIn: aThemedMorph label: aString for: control
	"Answer a morph laid out with a column with the title and control aligned to the left."

	^(self newColumnIn: aThemedMorph for: {
		self newLabelIn: aThemedMorph label: aString.
		control})
		cellPositioning: #leftCenter;
		cellInset: 2