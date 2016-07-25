controls
newExpander: aString forAll: controls
	"Answer an expander with the given label and controls."

	^self theme
		newExpanderIn: self
		label: aString
		forAll: controls