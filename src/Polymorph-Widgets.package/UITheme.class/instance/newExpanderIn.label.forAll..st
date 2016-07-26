morph creation
newExpanderIn: aThemedMorph label: aString forAll: controls
	"Answer an expander with the given label and controls."

	|answer|
	answer := ExpanderMorph basicNew
		basicTheme: self;
		initialize;
		font: self menuFont;
		titleText: aString;
		basicTheme: nil.
	controls do: [:m |
		answer addMorphBack: m].
	^answer